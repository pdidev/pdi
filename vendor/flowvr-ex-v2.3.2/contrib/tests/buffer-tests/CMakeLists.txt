# THREAD-LEVEL AFFINITY EXAMPLE 

INCLUDE_DIRECTORIES(${FLOWVR_INCLUDE_DIR})

ADD_EXECUTABLE(flowvr-test-buffer buffer-test.cpp)
TARGET_LINK_LIBRARIES(flowvr-test-buffer
                      ${FLOWVR_base_LIBRARY}
                      ${FLOWVR_ftl_LIBRARY} 
                      ${CMAKE_THREAD_LIBS_INIT})
                      
INSTALL(TARGETS flowvr-test-buffer
        DESTINATION bin${BINDIR_SUFFIX})
        
################################################################
## TEST-SUITE
################################################################

INCLUDE(CTest)


SET( testcases
     	testCreateInvalidBuffer
     	test0AllocedBufferValidity
		testCreateInvalidBufferWrite
		test1ByteBufferNoImp
		testEmptyBuffer
		testUnique
		testNonUnique
		testBufferToStringOperator
		testSubWindow
		testReference
		testResize
		testSizeCalculation
		testGetSizeForInvalidBuffer
		testExpand
		testCopyConstructionEq
		testCopyConstructionUnEq
		testAssignment
		testComparisonUnEqEq
		testSubWindowTooLarge
		testZeroSizeSubWindow
		
		testSegmentCreation
		testSegmentedSubWindow
		testSegmentedLinearize
		testSegmentedSubWindowLinearize
		testSegmentedOperatorPlus
		testSegmentedOperatorMinus
		testSegmentedOperatorEqualPlus
		testSegmentedOperatorEqualMinus
		testSegmentedOperatorMinusNoMatch
		testSegmentedUnique
		testSegmentedNonUnique
		testOneSegmentLinearize
		testCopyToOperation
		testSegmentedToStringOperator
		
		)
		
FOREACH( test ${testcases} )
    ADD_TEST( flowvr-buffer-test-${test} flowvr-test-buffer ${test} )
ENDFOREACH( test ${testcases} )
