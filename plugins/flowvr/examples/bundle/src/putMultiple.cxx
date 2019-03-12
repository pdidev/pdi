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
*  11/10/2018 Reworked for PDI:       Karol Sierocinski           *
*                                                                 *
*******************************************************************
*                                                                 *
* File: putMultiple.cxx                                           *
*                                                                 *
* Contacts:                                                       *
*  26/02/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*  11/10/2018 Karol Sierocinski <ksiero@man.poznan.pl>            *
*                                                                 *
******************************************************************/
#include <iostream>
#include <unistd.h>

#include <pdi.h>

int sleep_time=1;

int main(int argc, const char** argv)
{
	PC_tree_t config = PC_parse_path("config/putMultiple.yml");
	PDI_init(config);

	int it = 0;
	int wait;
	// this calls wait() method in flowvr and saves result in wait variable
	PDI_expose("wait", &wait, PDI_IN);
	while (wait)
	{
		const char* text = (it & 1) ? "tac" : "tic";
		// access "text_shr" shared memory
		char* text_shr;
		PDI_access("text_shr", (void**)&text_shr, PDI_OUT);
		memcpy(text_shr, text, 4);
		PDI_release("text_shr"); //release descriptor that will be no longer used - very important

		
		const char* text2 = (it & 1) ? "TAC" : "TIC";
		// access "text2_shr" shared memory
		char* text2_shr;
		PDI_access("text2_shr", (void**)&text2_shr, PDI_OUT);
		memcpy(text2_shr, text2, 4);
		PDI_release("text2_shr"); //release descriptor that will be no longer used - very important
		
		// this calls wait() method in flowvr and saves result in wait variable
		PDI_expose("wait", &wait, PDI_IN);

		if (wait) { // if put succeed
			int text_it;
			PDI_expose("text_it", &text_it, PDI_IN);
			std::cout << "(Put Multiple 1)\tSent "<< text <<" (it = " << text_it << ")" << std::endl;

			int text_it2;
			PDI_expose("text2_it", &text_it2, PDI_IN);
			std::cout << "(Put Multiple 2)\tSent "<< text2 <<" (it = " << text_it2 << ")" << std::endl;
		}

		sleep(sleep_time);
		++it;
	}

	PDI_finalize();
	return 0;
}
