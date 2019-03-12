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
* File: getMultiple.cxx                                           *
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
	PC_tree_t config = PC_parse_path("config/getMultiple.yml");
	PDI_init(config);
	
	int it = 0;
	int wait;
	// this calls wait() method in flowvr and saves result in wait variable
	PDI_expose("wait", &wait, PDI_IN);
	while (wait)
	{
		// access "text_shr" shared memory
		char* text_shr;
		PDI_access("text_shr", (void**)&text_shr, PDI_IN);	
		
		int text_it;
		PDI_expose("text_it", &text_it, PDI_IN);

		char text_source[256];		
		PDI_expose("text_source", text_source, PDI_IN);

		std::cout << "(Get Multiple)\tReceived " << text_shr << " (it = " << text_it << ") from " << text_source << std::endl;

		//release all descriptors that will be no longer used - very important
		PDI_release("text_shr");


		/**
		 *  To make sure you always release all accesses, you can open
		 *  local scope after every PDI_access and call corresponding
		 *  PDI_release after closing the scope
		 */
		// access "text2_shr" shared memory
		char* text2_shr;
		PDI_access("text2_shr", (void**)&text2_shr, PDI_IN);
		{
			int text_it2;
			PDI_expose("text2_it", &text_it2, PDI_IN);

			char text_source2[256];
			PDI_expose("text2_source", text_source2, PDI_IN);
			
			std::cout << "(Get Multiple)\tReceived " << text2_shr << " (it = " << text_it2 << ") from " << text_source2 << std::endl;
		}
		PDI_release("text2_shr");
		

		sleep(sleep_time);
		++it;

		// this calls wait() method in flowvr and saves result in wait variable
		PDI_expose("wait", &wait, PDI_IN);
	}
	
	PDI_finalize();
	return 0;
}
