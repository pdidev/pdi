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
* File: get.cxx                                                   *
*                                                                 *
* Contacts:                                                       *
*  26/02/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*  11/10/2018 Karol Sierocinski <ksiero@man.poznan.pl>            *
*                                                                 *
******************************************************************/

#include <iostream>
#include <unistd.h>

#include <pdi.h>

/**
 *  TIP:
 *    To make sure you always release all accesses, you can open
 *    local scope after every PDI_access and call corresponding
 *    PDI_release after closing the scope (see: ../bundle/getMultiple.cxx)
 */

int main(int argc, char* argv[])
{
	PC_tree_t conf = PC_parse_path("get.yml");
	PDI_init(conf);
	
	int wait;
	// this calls wait() method in flowvr and saves result in wait variable
	PDI_expose("wait", &wait, PDI_IN);
	printf("Wait = %d\n", wait);
	while (wait)
	{
		// access "text_shr" shared memory
		char* text_shr;
		PDI_access("text_shr", (void**)&text_shr, PDI_IN);

		int text_it;
		PDI_expose("text_it", &text_it, PDI_IN);

		char text_source[256];
		PDI_expose("text_source", text_source, PDI_IN);

		std::cout << "(Get)\tReceived " << text_shr << " (it = " << text_it << ") from " << text_source << std::endl;
		
		//release all descriptors that will be no longer used - very important
		PDI_release("text_shr");

		int array_stamp[2];
		PDI_expose("array_stamp", array_stamp, PDI_IN);

		std::cout << "(Get)\tArray stamp = [" << array_stamp[0] << ", " << array_stamp[1] << "]" << std::endl;

		PDI_expose("wait", &wait, PDI_IN); // "wait" expose calls flowvr->wait()
	}
  
	PDI_finalize();
	return 0;
}
