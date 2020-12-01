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
 * File: get.cpp                                                   *
 *                                                                 *
 * Contacts:                                                       *
 *  26/02/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/
#include "flowvr/module.h"
#include <iostream>
#include <unistd.h>

#include "fca/fca.h"

int sleep_time = 1;

#define STRING_SIZE 8
#define ARRAY_LENGTH 9

int main(int argc, const char** argv) {

	/*
	 * Declaring module getfca
	 */

	// User defined stamps
	fca_port portText = fca_new_port("text", fca_IN, 0, NULL);
	fca_stamp stampInt = fca_register_stamp(portText, "stampInt", fca_INT, NULL);
	fca_stamp stampFloat = fca_register_stamp(portText, "stampFloat", fca_FLOAT, NULL);
	fca_stamp stampString = fca_register_stamp(portText, "stampString", fca_STRING, STRING_SIZE);
//	fca_stamp stampBinary = fca_register_stamp(portText, "stampBinary", fca_BINARY, 13);
	fca_stamp stampArray = fca_register_stamp(portText, "stampArray", fca_ARRAY, ARRAY_LENGTH, fca_FLOAT);

	fca_module moduleGet = fca_new_module_from_ports(portText, NULL);
	if (!moduleGet)
		return 1;

	// System default stamps
	fca_stamp stampMit = fca_get_stamp(portText, "it");
	fca_stamp stampSource = fca_get_stamp(portText, "source");

	int it = 0;
	while (fca_wait(moduleGet)) {
		/*
		 * Retrieve a message
		 */
		fca_message message = fca_get(portText);

		const char* a = (const char*)fca_get_read_access(message, 0);

		/*
		 * Read message payload
		 */
		std::string text;
		text.append(a,
				(a + fca_get_segment_size(message, 0)) );

		/*
		 * Read message log (default stamps)
		 */
		// Mit
		void* stampMitBuffer = fca_read_stamp(message, stampMit);
		int mit = *((int*)stampMitBuffer);
		free(stampMitBuffer);

		// Source
		char* source = (char*)fca_read_stamp(message, stampSource);

		std::cout << "(getfca) Received " << text << "(it=" << mit << ") from " << source
						<< std::endl;

		/*
		 * Read user stamps
		 */
		// Int
		int* receivedInt = (int*)fca_read_stamp(message, stampInt);
		// Float
		float* receivedFloat = (float*)fca_read_stamp(message, stampFloat);
		// String
		char* receivedString = (char*)fca_read_stamp(message, stampString);

		//Array
		float* receivedArray = (float*)fca_read_stamp(message, stampArray);

		// Binary
//		char* binBuffer = (char*)fca_read_stamp(message, stampBinary);
//		std::string receivedBinary = "";
//		receivedBinary.append(binBuffer, 13);


		std::cout << std::endl << "(getfca) RECEIVED MESSAGE :" << std::endl
				<< "#  stampInt= " << *receivedInt << std::endl
				<< "#  stampFloat= " << *receivedFloat << std::endl
				<< "#  stampString= " << receivedString << std::endl
				<< "#  stampArray= [";
		for( int i = 0; i < ARRAY_LENGTH; i++)
				std::cout<<receivedArray[i]<<";";
				std::cout<<"]"<<std::endl;
				std::cout<< std::endl;
			//	<< "; stampBinary= " << receivedBinary << std::endl;


		std::cout << "--------------------------" << std::endl
				  << "--- Finishing exchange ---" << std::endl
				  << "--------------------------" << std::endl;
		sleep(sleep_time);
		++it;
	}

	fca_free(moduleGet);

	return 0;
}
