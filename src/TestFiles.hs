
module TestFiles where

testFiles =
  [
--    "tests/BlockchainTests/basicBlockChain.json",
--    "tests/BlockchainTests/badBlockChain.json",
--    "tests/TrieTests/trietest.json",
--    "tests/TrieTests/trieanyorder.json",
--    "tests/TrieTests/trietestnextprev.json",
--    "tests/TransactionTests/ttTransactionTest.json",
--    "tests/TransactionTests/tt10mbDataField.json",
--    "tests/BasicTests/rlptest.json",
--    "tests/BasicTests/genesishashestest.json",
--    "tests/BasicTests/hexencodetest.json",
--    "tests/BasicTests/blockgenesistest.json",
--    "tests/BasicTests/txtest.json",
--    "tests/BasicTests/keyaddrtest.json",
    "tests/VMTests/vmSha3Test.json", --0
    "tests/VMTests/vmPushDupSwapTest.json",
    --"tests/VMTests/vmInputLimitsTest2.json",
    "tests/VMTests/RandomTests/201501221718CPPJIT.json",
    "tests/VMTests/RandomTests/201502070041CPPJIT.json",
    "tests/VMTests/RandomTests/201501131330CPPJIT.json", --5
    "tests/VMTests/RandomTests/201501131453CPPJIT.json",
    "tests/VMTests/RandomTests/201501132305PYTHON.json",
    "tests/VMTests/RandomTests/201501082121.json",
    "tests/VMTests/RandomTests/201501221531CPPJIT.json",
    "tests/VMTests/RandomTests/201501082030.json", --10
    "tests/VMTests/RandomTests/201502052207CPPJIT.json",
    "tests/VMTests/RandomTests/201502071757CPPJIT.json",
    "tests/VMTests/RandomTests/201501222357PYTHON.json",
    "tests/VMTests/RandomTests/201502110340CPPJIT.json",
    "tests/VMTests/RandomTests/201501122225CALL_TO_PRECOMPILED_GO.json", --15
    "tests/VMTests/RandomTests/201501132239PYTHON.json",
    "tests/VMTests/RandomTests/201501082153.json",
    "tests/VMTests/RandomTests/201501140911CPPJIT.json",
    "tests/VMTests/RandomTests/goFail.json",
    "tests/VMTests/RandomTests/201501082028.json", --20
    "tests/VMTests/RandomTests/201501222304PYTHON.json",
    "tests/VMTests/RandomTests/201501140918PYTHON.json",
    "tests/VMTests/RandomTests/201501082026.json",
    "tests/VMTests/RandomTests/201501131414CPPJIT.json",
    --"tests/VMTests/RandomTests/201501221235PYTHON.json", --25
    "tests/VMTests/RandomTests/201501082116.json",
    "tests/VMTests/RandomTests/201501191859PYTHON.json",
    "tests/VMTests/RandomTests/201501082111.json",
    "tests/VMTests/RandomTests/201501082029.json",
    --"tests/VMTests/RandomTests/201501130902PYTHON_BLOCKHASH.json", --30
    "tests/VMTests/RandomTests/201501131655CREATE_RETURNS_DIFFERENT_ADDREESS_GO.json",
    "tests/VMTests/RandomTests/201501221647PYTHON.json",
    "tests/VMTests/RandomTests/201502101000CPPJIT.json",
    "tests/VMTests/RandomTests/201501131326STACKUNDERFLOW_CPPJIT.json",
    "tests/VMTests/RandomTests/201501082122.json", --35
    "tests/VMTests/RandomTests/201501130906PYTHON_INVALID_OPCODE.json",
    "tests/VMTests/RandomTests/201502070508CPPJIT.json",
    "tests/VMTests/RandomTests/201501131811CPPJIT.json",
    "tests/VMTests/RandomTests/201501091628.json",
    "tests/VMTests/RandomTests/201501121256GO.json", --40
    "tests/VMTests/RandomTests/201501082112.json",
    "tests/VMTests/RandomTests/201501082150.json",
    "tests/VMTests/RandomTests/201501131823CPPJIT.json",
    "tests/VMTests/RandomTests/goFail3.json",
    "tests/VMTests/RandomTests/201501091657.json", --45
    --"tests/VMTests/RandomTests/201501191953PYTHON.json",
    "tests/VMTests/RandomTests/201501121151GO.json",
    "tests/VMTests/RandomTests/201501131628CPPJIT.json",
    "tests/VMTests/RandomTests/201501110744GO.json",
    "tests/VMTests/RandomTests/201501132304PYTHON.json", --50
    "tests/VMTests/RandomTests/201501082023.json",
    "tests/VMTests/RandomTests/201501082131.json",
    --"tests/VMTests/RandomTests/201501230844PYTHON.json",
    "tests/VMTests/RandomTests/201502110556CPPJIT.json",
    "tests/VMTests/RandomTests/201502032117GO.json",
    "tests/VMTests/RandomTests/201501091658.json",
    "tests/VMTests/RandomTests/201501121303GO.json",
    "tests/VMTests/RandomTests/201501082141.json",
    "tests/VMTests/RandomTests/201501151106PYTHON.json",
    "tests/VMTests/RandomTests/201501082123.json", --60
    "tests/VMTests/RandomTests/201501082207.json",
    "tests/VMTests/RandomTests/201502051552CPPJIT.json",
    "tests/VMTests/RandomTests/201501091644.json",
    "tests/VMTests/RandomTests/201501082203.json",
    "tests/VMTests/RandomTests/201501140910CPPJIT.json",
    "tests/VMTests/RandomTests/201501120933PYTHON.json",
    "tests/VMTests/RandomTests/201501140914CPPJIT.json",
    "tests/VMTests/RandomTests/201501082205.json",
    "tests/VMTests/RandomTests/201501082156.json",
    "tests/VMTests/RandomTests/201501121149GO.json", --70
    "tests/VMTests/RandomTests/201501131826CPPJIT.json",
    "tests/VMTests/RandomTests/201501082039.json",
    "tests/VMTests/RandomTests/201501082036.json",
    "tests/VMTests/RandomTests/201502051153CPPJIT.json",
    "tests/VMTests/RandomTests/201501150842LARGE_DATA_IN_CALLCREATE_GOjson",
    "tests/VMTests/RandomTests/goFail2.json",
    "tests/VMTests/RandomTests/randomTest.json",
    "tests/VMTests/RandomTests/201501121301GO.json",
    "tests/VMTests/RandomTests/201501082151.json",
    "tests/VMTests/RandomTests/201501131824CPPJIT.json", --80
    "tests/VMTests/RandomTests/201501131627CPPJIT.json",
    "tests/VMTests/RandomTests/201502110412CPPJIT.json",
    "tests/VMTests/RandomTests/201501082159.json",
    "tests/VMTests/RandomTests/201501121148GO.json",
    "tests/VMTests/RandomTests/201501082200.json",
    "tests/VMTests/RandomTests/201501131331CPPJIT.json",
    "tests/VMTests/RandomTests/201412232335.json",
    "tests/VMTests/RandomTests/201501082119.json",
    "tests/VMTests/RandomTests/201501131820CPPJIT.json",
    "tests/VMTests/RandomTests/201501131821CPPJIT.json", --90
    "tests/VMTests/RandomTests/201501082137.json",
    "tests/VMTests/RandomTests/201501082052.json",
    "tests/VMTests/RandomTests/201501082146.json",
    "tests/VMTests/RandomTests/201502061553CPPJIT.json",
    "tests/VMTests/RandomTests/201501132242PYTHON.json",
    "tests/VMTests/RandomTests/201501082118.json",
    "tests/VMTests/RandomTests/201501131452CPPJIT.json",
    "tests/VMTests/RandomTests/201501130903PYTHON_INVALID_JUMP_DESTINATION.json",
    "tests/VMTests/RandomTests/201501082115.json", 
    "tests/VMTests/RandomTests/201501091831.json", --100
    "tests/VMTests/RandomTests/201501221210PYTHON.json",
    "tests/VMTests/RandomTests/201502031248CPPJIT.json",
    "tests/VMTests/RandomTests/201501082134.json",
    "tests/VMTests/RandomTests/201502110635CPPJIT.json",
    "tests/VMTests/RandomTests/201501082041.json",
    "tests/VMTests/RandomTests/201501140912CPPJIT.json",
    "tests/VMTests/RandomTests/201501131818CPPJIT.json",
    "tests/VMTests/RandomTests/201501231158PYTHON.json",
    "tests/VMTests/RandomTests/201501082132.json",
    "tests/VMTests/RandomTests/201501122239BALANCE_OF_ADDRESS_GO.json",
    --"tests/VMTests/RandomTests/201501191458PYTHON.json",
    "tests/VMTests/RandomTests/201501082040.json",
    "tests/VMTests/RandomTests/201502051555CPPJIT.json",
    "tests/VMTests/RandomTests/201501082145.json",
    --"tests/VMTests/vmSystemOperationsTest.json", --nested calls
    "tests/VMTests/vmtests.json",
    "tests/VMTests/vmEnvironmentalInfoTest.json",
    "tests/VMTests/vmBlockInfoTest.json",
    --"tests/VMTests/vmInputLimitsTest1.json",
    "tests/VMTests/vmLogTest.json",
    --"tests/VMTests/vmArithmeticTest.json", --merkel patricia db problem
    "tests/VMTests/vmBitwiseLogicOperationTest.json",
    "tests/VMTests/vmIOandFlowOperationsTest.json",
    "tests/StateTests/stTransactionTest.json",
    "tests/StateTests/stInitCodeTest.json",
    --"tests/StateTests/stRecursiveCreate.json",
    "tests/StateTests/stPreCompiledContracts.json",
    "tests/StateTests/stSpecialTest.json",
    "tests/StateTests/stLogTests.json",
    "tests/StateTests/stBlockHashTest.json",
    "tests/StateTests/stExample.json"
    --"tests/StateTests/stSystemOperationsTest.json",
    --"tests/StateTests/stRefundTest.json"
  ]














































{-
    --"tests/VMTests/vmInputLimitsTest2.json",
    --"tests/VMTests/vmSystemOperationsTest.json", --nested calls
    --"tests/VMTests/vmInputLimitsTest1.json",
    --"tests/VMTests/vmArithmeticTest.json", --merkel patricia db problem

    "tests/VMTests/vmSha3Test.json", --0
    "tests/VMTests/vmPushDupSwapTest.json",
    "tests/VMTests/RandomTests/201501221718CPPJIT.json",
    "tests/VMTests/vmtests.json",
    "tests/VMTests/vmEnvironmentalInfoTest.json",
    "tests/VMTests/vmBlockInfoTest.json",
    "tests/VMTests/vmLogTest.json",
    "tests/VMTests/vmBitwiseLogicOperationTest.json",
    "tests/VMTests/vmIOandFlowOperationsTest.json",
    "tests/StateTests/stTransactionTest.json",
    "tests/StateTests/stInitCodeTest.json",
    "tests/StateTests/stRecursiveCreate.json",
    "tests/StateTests/stPreCompiledContracts.json",
    "tests/StateTests/stSpecialTest.json",
    "tests/StateTests/stLogTests.json",
    "tests/StateTests/stBlockHashTest.json",
    "tests/StateTests/stExample.json"
    --"tests/StateTests/stSystemOperationsTest.json",
    --"tests/StateTests/stRefundTest.json" -- merkle patricia problem
-}
