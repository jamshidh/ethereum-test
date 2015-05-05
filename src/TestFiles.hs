
module TestFiles where

--    "./tests/BlockTests/bcValidBlockTest.json",
--    "./tests/BlockTests/bcUncleTest.json",
--    "./tests/BlockTests/bcInvalidHeaderTest.json",
--    "./tests/BlockTests/bcBlockChainTest.json",
--    "./tests/BlockchainTests/basicBlockChain.json",
--    "./tests/BlockchainTests/badBlockChain.json",
--    "./tests/TrieTests/trietest_secureTrie.json",
--    "./tests/TrieTests/trietest.json",
--    "./tests/TrieTests/trieanyorder_secureTrie.json",
--    "./tests/TrieTests/trieanyorder.json",
--    "./tests/TrieTests/hex_encoded_securetrie_test.json",
--    "./tests/TrieTests/trietestnextprev.json",
--    "./tests/TransactionTests/ttTransactionTest.json",
--    "./tests/TransactionTests/tt10mbDataField.json",
--    "./tests/TransactionTests/ttWrongRLPTransaction.json",
--    "./tests/package.json",
--    "./tests/BasicTests/rlptest.json",
--    "./tests/BasicTests/genesishashestest.json",
--    "./tests/BasicTests/hexencodetest.json",
--    "./tests/BasicTests/blockgenesistest.json",
--    "./tests/BasicTests/txtest.json",
--    "./tests/BasicTests/keyaddrtest.json",
--    "./tests/BasicTests/crypto.json",
--    "./tests/PoWTests/ethash_tests.json",

vmTests = 
  [
    "./tests/VMTests/vmSha3Test.json",
    "./tests/VMTests/vmPushDupSwapTest.json",
    "./tests/VMTests/RandomTests/randomTest.json",
    "./tests/VMTests/vmSystemOperationsTest.json",
    "./tests/VMTests/vmtests.json",
    "./tests/VMTests/vmEnvironmentalInfoTest.json",
    "./tests/VMTests/vmPerformanceTest.json",
--    "./tests/VMTests/vmBlockInfoTest.json",
    "./tests/VMTests/vmLogTest.json",
    "./tests/VMTests/vmArithmeticTest.json",
    "./tests/VMTests/vmBitwiseLogicOperationTest.json",
    "./tests/VMTests/vmIOandFlowOperationsTest.json"
  ]

stateTests' =
  [
--    "./tests/StateTests/stTransactionTest.json",
--    "./tests/StateTests/stInitCodeTest.json",
--    "./tests/StateTests/stRecursiveCreate.json",
--    "./tests/StateTests/stPreCompiledContracts.json",
--    "./tests/StateTests/stSpecialTest.json",
--    "./tests/StateTests/stLogTests.json",
--    "./tests/StateTests/stBlockHashTest.json",
--    "./tests/StateTests/stSolidityTest.json",
--    "./tests/StateTests/stExample.json",
--    "./tests/StateTests/stSystemOperationsTest.json",
    "./tests/StateTests/stMemoryStressTest.json"
--    "./tests/StateTests/stRefundTest.json"
--    "./tests/StateTests/stMemoryTest.json"
    --"./tests/StateTests/stQuadraticComplexityTest.json"
  ]

stateTests =
  [
    "./tests/StateTests/stTransactionTest.json",
    "./tests/StateTests/stInitCodeTest.json",
    "./tests/StateTests/stRecursiveCreate.json",
    "./tests/StateTests/stPreCompiledContracts.json",
    "./tests/StateTests/stSpecialTest.json",
    "./tests/StateTests/stLogTests.json",
    "./tests/StateTests/stBlockHashTest.json",
    "./tests/StateTests/stSolidityTest.json",
    "./tests/StateTests/stExample.json",
    "./tests/StateTests/stSystemOperationsTest.json",
    "./tests/StateTests/stMemoryStressTest.json",
    "./tests/StateTests/stRefundTest.json",
    "./tests/StateTests/stMemoryTest.json",
    "./tests/StateTests/stQuadraticComplexityTest.json"
  ]

testFiles::[String]
testFiles =
  vmTests ++
  stateTests








