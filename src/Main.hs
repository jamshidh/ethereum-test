-- External data modules
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Either
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.Monoid (Sum(..))

-- External function modules
import Control.Applicative ((<$>))
import qualified Data.ByteString as ByteS
import Data.Either (isLeft)
import Data.Function (on)
import Data.List (findIndex, deleteBy, intercalate, sortBy)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing, fromJust, fromMaybe)
import Debug.Trace
import Network.Haskoin.Crypto (withSource, devURandom)
import System.Environment (getProgName, getArgs)
import System.FilePath (takeFileName, (</>))

-- Blockchain modules
import Blockchain.Data.AddressStateDB (putAddressState, getAllAddressStates,
                                       deleteAddressState)
import Blockchain.BlockChain (addTransaction)
import qualified Blockchain.Colors as Color
import Blockchain.Context (Context(..))
import Blockchain.Database.MerklePatricia (emptyTriePtr)
import Blockchain.DBM (openDBs, setStateRoot)
import Blockchain.VM (runCodeFromStart)
import Blockchain.VM.VMState (startingState, VMState(..))

-- Internal modules
import TestDescriptions
import TestFiles
import TestStructures
import Conversions
import Debug

main = do
  prog <- takeFileName <$> getProgName
  args <- getArgs
  let l@[testDir, testFile, testName, err] = take 4 $ args ++ (repeat "")
  let err1 = not(null err)
      firstGiven = findIndex (not . null) l
      err2 = isJust firstGiven && (fromJust firstGiven /= 0)
  if err1 || err2
    then error $ "Invocation:\n" ++
         "    " ++ prog ++ ": run all tests under tests/\n" ++
         "    " ++ prog ++ " dir: run all tests under dir/\n" ++
         "    " ++ prog ++ " file: run all tests in file\n" ++
         "    " ++ prog ++ " dir file: run all tests in dir/file\n" ++
         "    " ++ prog ++ " file name: run test name in file\n" ++
         "    " ++ prog ++ " dir file name: run test name in dir/file"
    else do
    let testCount = runAllTests testDir testFile testName
        testState = execWriterT testCount
        testContext = evalStateT testState (Context [] 0 [] False)
        testResources = evalStateT testContext =<< openDBs "h"
    counts <- runResourceT testResources
    let
      passes = getSum $ fst counts
      total = getSum $ snd counts
      total' = max 1 total
    putStrLn $ Color.bright $ "Tests passed / total tests: " ++
      show passes ++ "/" ++ show total ++
      " (" ++ show (100*passes `div` total') ++ "%)"
      
runAllTests testDir testFile testName = do
  testTree <- liftIO $ load $ if null testDir then "tests" else testDir
  case testTree of
    TestFile _ _ ->
      if null testName
      then run testDir testFile testTree -- Actually test file, test name
      else error $ "Ambiguous: First argument specifies a file but other " ++
                   "file given separately." 
    _ ->
      let absTestFile = if null testFile then "" else testDir </> testFile
      in run absTestFile testName testTree

run _ _ (Ignore _) = return ()
run _ _ (NoTests name)= liftIO $ putStrLn $ "No tests found in: " ++ name
run testFile testName (TestDir dir contents) = do
  liftIO $ putStrLn $ Color.blue $ "#### Running tests under dir: " ++ dir
  mapM_ (run testFile testName) contents
run testFile testName (TestFile file tests)
  | not(null testFile) && testFile == file && not(null testName) =
    (lift $ do
      let test = flip fromMaybe (Map.lookup testName tests) $
                 error $ "Test " ++ testName ++ " not found in file: " ++ testFile
      modify (\cxt -> cxt{debugEnabled = True})
      liftIO $ putStrLn $ Color.yellow $
        "#### Running single test in file: " ++ testFile
      testPrompt testName
      return $
        flip fromMaybe (Map.lookup testName tests) $
        error $ "Test " ++ testName ++ " not found in file: " ++ testFile)
    >>= runTest

  | not(null testFile) && testFile /= file =
    return ()
    
  | null testFile && not(null testName) =
      error "Ambiguous: No test file specified but test name given."
      
  | otherwise =
    let runTestsIn ts =
          forM_ (Map.toList tests) $ \(name, test) -> do
            testPrompt name
            runTest test
    in (liftIO $ putStrLn $ Color.yellow $
       "#### Running all tests in file: " ++ file)
       >> runTestsIn tests
    
  where testPrompt name = liftIO $ putStr $ "> " ++ name ++ ": "

runTest test =
  (lift $ do 
    initializeMPDB test
    beforeAddressStates <- addressStates
    testResults <- doTest test
    afterAddressStates <- addressStates
    debugAddressStates beforeAddressStates afterAddressStates postTest
    return (testResults, afterAddressStates))
  >>= uncurry (showResults expectedResults)

  where postTest = makeHashed (post test)
        expectedResults =
          (out test, postTest, remainingGas test, logs' test, callcreates test)

initializeMPDB test = do
  lift $ setStateRoot emptyTriePtr
  forM_ (Map.toList $ pre test) $
    \(addr, s) -> do
      state' <- populateAndConvertAddressState addr s
      lift $ putAddressState addr state'

addressStates = do
  addrStates <- lift getAllAddressStates
  let addrs = map fst addrStates
      states = map snd addrStates
  states' <- mapM getDataAndRevertAddressState states
  return $ sortBy (compare `on` fst) $ zip addrs states'

doTest test@Test{theInput = IExec Exec{gas = g}} = do
  vmState <- liftIO $ startingState (testExecEnv test)
  (result, vmState') <-
    flip runStateT vmState{vmGasRemaining=getNumber g, debugCallCreates=Just []} $
    runEitherT $ do
      runCodeFromStart
      vmState' <- lift get
      debugSuicideList vmState'
      forM_ (suicideList vmState') $ lift . lift . lift . deleteAddressState
  return (result, returnVal vmState', vmGasRemaining vmState',
          logs vmState', debugCallCreates vmState')
  
doTest test@Test{theInput = ITransaction _, env = Env{currentGasLimit = gL}} = do
  signedTransaction <- liftIO $ withSource devURandom $ testTransaction test
  result <- runEitherT $ addTransaction (testBlock test) gL signedTransaction
  case result of
    Right (vmState, _) ->
      return (Right (), returnVal vmState, vmGasRemaining vmState,
              logs vmState, debugCallCreates vmState)
    Left _ ->
      return (Right (), Nothing, 0, [], Just [])

showResults
  expected@(retVal0, statesAfter0, gasRemaining0, logs0, returnedCallCreates0)
  results@(result, retVal, gasRemaining, logsR, returnedCallCreates)
  statesAfter
  = let
    retValMatches = RawData (fromMaybe ByteS.empty retVal) == retVal0
    stateMatches = (statesAfter == statesAfter0) ||
                   (null statesAfter0 && isLeft result)
    gasMatches = gasRemaining == fromMaybe gasRemaining gasRemaining0
    logsMatch = logsR == reverse logs0
    callCreatesMatch =
      (returnedCallCreates0 == fmap reverse returnedCallCreates) ||
      (isNothing returnedCallCreates0 && (returnedCallCreates == Just []))
  in either failWithDebug passWithDebug $
     analyzeResults retValMatches stateMatches
     gasMatches logsMatch callCreatesMatch results expected
  where
    passWithDebug = withDebug passIncr Color.green
    failWithDebug = withDebug failIncr Color.red
    passIncr = (Sum 1, Sum 1)
    failIncr = (Sum 0, Sum 1)

analyzeResults False  _  _  _  _
  (_, retVal, _, _, _) (RawData retVal0, _, _, _, _)
  = Left ("return values don't match",
          "returned (actual) : " ++ retValS  ++ "\n" ++
          "returned (correct): " ++ retValS0 ++ "\n")
    where
      retValS = bytesToHex (fromMaybe ByteS.empty retVal)
      retValS0= bytesToHex retVal0
      bytesToHex = intercalate " " . map showHexInt . ByteS.unpack
analyzeResults _  False  _  _  _ _ _ = Left ("address states don't match", "")
analyzeResults _  _  False  _  _
  (_, _, gasRemaining, _, _) (_, _, gasRemaining0, _, _) =
    Left
    ("remaining gas doesn't match",
     "remaining (actual) : " ++ show gasRemainingN ++ "\n" ++
     "remaining (correct): " ++ show gasRemaining0N ++ "\n" ++
     "difference = " ++ show (gasRemainingN - gasRemaining0N))

    where
      gasRemainingN = gasRemaining
      gasRemaining0N= fromMaybe gasRemaining gasRemaining0
analyzeResults _  _  _  False  _
  (_, _, _, logsR, _) (_, _, _, logs0, _) =
    Left ("logs don't match",
          "logs (actual) : " ++ show logsR ++ "\n" ++
          "logs (correct): " ++ show logs0 ++ "\n")
analyzeResults _  _  _  _  False
  (_, _, _, _, returnedCallCreates) (_, _, _, _, returnedCallCreates0) =
    Left ("callcreates don't match",
          "callcreates (actual) : " ++ show returnedCallCreates ++ "\n" ++
          "callcreates (correct): " ++  show returnedCallCreates0)
analyzeResults _ _ _ _ _ _ _= Right ("Success","")


