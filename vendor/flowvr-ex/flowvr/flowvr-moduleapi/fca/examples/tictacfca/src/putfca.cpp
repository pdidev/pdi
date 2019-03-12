/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                     Application Library                         *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA                                                           *
* ALL RIGHTS RESERVED.	                                          *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Thomas Arcila,                                               *
*    Jean-Denis Lesage.                                           *
*    Clement Menier,                                              *
*    Bruno Raffin                                                 *
*                                                                 *
*******************************************************************
*                                                                 *
* File: put.cpp                                                   *
*                                                                 *
* Contacts:                                                       *
*  26/02/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include <flowvr/module.h>
#include <iostream>
#include <unistd.h>

#include "fca/fca.h"

#include <time.h>

int sleep_time=2;

#define STRING_SIZE 8
#define ARRAY_LENGTH 9

int main(int argc, const char** argv)
{
	fca_port portText = fca_new_port("text", fca_OUT, 0, NULL);

	/*
	 * Integer and float stamp
	 */
	fca_stamp stampInt = fca_register_stamp(portText, "stampInt", fca_INT);
	printf("Registered stamp: %u - %s\n", fca_get_stamp_type(stampInt), fca_get_stamp_name(stampInt));

	fca_stamp stampFloat = fca_register_stamp(portText, "stampFloat", fca_FLOAT);
	printf("Registered stamp: %u - %s\n", fca_get_stamp_type(stampFloat), fca_get_stamp_name(stampFloat));

	/*
	 * String stamp
	 */
	fca_stamp stampString = fca_register_stamp(portText, "stampString", fca_STRING, STRING_SIZE);
	printf("Registered stamp: %u - %s\n", fca_get_stamp_type(stampString), fca_get_stamp_name(stampString));

	/*
	 * Binary stamp
	 */
//	fca_stamp stampBinary = fca_register_stamp(portText, "stampBinary", fca_BINARY, 13);
//	printf("Registered stamp: %u - %s\n", fca_get_stamp_type(stampBinary), fca_get_stamp_name(stampBinary));


	/*
	 * Array stamp
	 */
	fca_stamp stampArray = fca_register_stamp(portText, "stampArray", fca_ARRAY, ARRAY_LENGTH, fca_FLOAT);
	printf("Registered stamp : %u - %s\n", fca_get_stamp_type(stampArray), fca_get_stamp_name(stampArray));

 
	fca_trace trace = fca_new_trace("beginTrace", fca_trace_INT, NULL);
	if(trace != NULL) printf("Creation of trace succeded.\n"); else printf("Failed to create a trace.\n");
	
	fca_trace trace2 = fca_new_trace("endTrace", fca_trace_INT, NULL);
        if(trace2 != NULL) printf("Creation of trace succeded.\n"); else printf("Failed to create a trace.\n");

	//fca_module modulePut = fca_new_module_from_ports(portText, NULL);
	//if (!modulePut)
	//	return 1;
	fca_module modulePut = fca_new_empty_module();
	fca_append_port(modulePut, portText);
	fca_append_trace(modulePut, trace);
	fca_append_trace(modulePut, trace2);	

	if(!fca_init_module(modulePut)){
		printf("ERROR : init_module failed!\n");
		return 1;
	}

	fca_trace testTrace = fca_get_trace(modulePut,"beginTrace");
	if(testTrace == NULL) printf("ERROR : Test Trace FAIL!!\n"); else printf("Test Trace OK.\n");		

	int it=0;
	while (fca_wait(modulePut))
	{
		std::cout << "++++++++++++++++++++++++++" << std::endl
				  << "+++ Beginning exchange +++" << std::endl
				  << "++++++++++++++++++++++++++" << std::endl;
		/*
		 * Sending text
		 */
		std::string text=(it&1)?"tac":"tic";

		// Build data
		
		fca_message message = fca_new_message(modulePut, text.length());
		
		char* messageBuffer = (char*)fca_get_write_access(message, 0);
		memcpy(messageBuffer,text.c_str(),text.length());

		/*
		 * Appending stamp
		 */
		// Int
		int* bufferInt = new int(rand());
		fca_write_stamp(message, stampInt, (void*)bufferInt);

		// Float
		float* bufferFloat = new float(3.141592653589793238462643383279);
		fca_write_stamp(message, stampFloat, (void*)bufferFloat);

		// String
		char* testString;
		//testString = (char*)malloc(STRING_SIZE);
		testString = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Praesent mauris neque, lobortis ac blandit pellentesque, tincidunt sed orci. Morbi eleifend sollicitudin diam vel ornare. Maecenas malesuada, magna eu pharetra condimentum, ligula augue interdum orci, quis lobortis sem dui a orci. Etiam ac velit nulla. Vestibulum ac aliquam lorem. Donec at velit non lacus scelerisque dignissim nec ac nisi. In at orci quam. ";
		fca_write_stamp(message, stampString, (void*)testString);

		// Binary
		char* testBinary;
		testBinary = (char*)malloc(13);
		testBinary = "Test string.";

//		fca_write_stamp(message, stampBinary, (void*)testBinary);

		//Array
		float arrayFloat[ARRAY_LENGTH];
		for(int i = 0; i < ARRAY_LENGTH; i++)
			arrayFloat[i] = (float)i;
		fca_write_stamp(message, stampArray, (void*)arrayFloat);

		int* testBufferInt = (int*)fca_read_stamp(message, stampInt);
		float* testBufferFloat = (float*)fca_read_stamp(message, stampFloat);

		std::cout << std::endl << "(putfca) SENDING MESSAGE :" << std::endl
				<< "#  stampInt= " << *testBufferInt << std::endl
				<< "#  stampFloat= " << *testBufferFloat << std::endl
				<< "#  stampString= " << testString << std::endl
				//<< "; stampBinary= " << testBinary
				<< std::endl;


		/*
		 * Sending message
		 */
		bool success = fca_put(portText, message);
		
		if (success) {
			std::cout << "(putfca) Sent ";
		} else {
			std::cout << "(putfca) Not sent ";
		}

		/*
		 * Writing custom event
		 */
		sleep(1);
		if(!fca_write_trace(trace,&it))
			printf("ERROR : Not able to write a trace.\n");
		sleep(1);
		if(!fca_write_trace(trace2,&it))
                        printf("ERROR : Not able to write a trace.\n");
		sleep(1);

		/*
		 * Reading logs
		 */
		fca_stamp stampMit = fca_get_stamp(portText, "it");

		void* stampMitBuffer = fca_read_stamp(message, stampMit);
		int mit = *((int*)stampMitBuffer);
		free(stampMitBuffer);

		std::cout<<text<<" (it="<<mit<<")"<<std::endl;

		sleep(sleep_time);
		++it;
	}

	fca_free(modulePut);

	return 0;
}
