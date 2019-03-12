/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                             Utils                               *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490) ALL RIGHTS RESERVED.                                 *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Ronan Gaugne,                                                *
*    Valerie Gouranton,                                           *
*    Loick Lecointre,                                             *
*    Sebastien Limet,                                             *
*    Clement Menier,                                              *
*    Bruno Raffin,                                                *
*    Sophie Robert,                                               *
*    Emmanuel Melin.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: ./src/modules/joypad.cpp                                  *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <linux/joystick.h>
#include <flowvr/module.h>

#include <sstream>

int main(int argc, char** argv)
{
  const char* devName = "/dev/js0";

  if (argc>=2) devName = argv[1];

  int fd = open(devName, O_RDONLY); // Open the joystick device

  if (fd<0)
  {
    std::cerr << "Device "<<devName<<" open failed."<<std::endl;
    return 1;
  }

  char joyName[256] = "unknown";

  int version=0;
  unsigned char nbAxes=0, nbButtons=0;
  ioctl(fd, JSIOCGVERSION, &version);
  ioctl(fd, JSIOCGAXES, &nbAxes);
  ioctl(fd, JSIOCGBUTTONS, &nbButtons);
  ioctl(fd, JSIOCGNAME(sizeof(joyName)), joyName);

  std::cout << "Found joystick \""<<joyName<<"\" with "<<(int)nbAxes<<" axes and "<<(int)nbButtons<<" buttons."<<std::endl;

  std::vector<std::vector<flowvr::Port*> >ports;

  // Looking for existing FLOWVR_MODNAME_x environment variables which indicates multi-module mode
   std::vector<char *> moduleNames;

   char * flowvr_modname = getenv("FLOWVR_MODNAME");
   unsetenv("FLOWVR_MODNAME");
   moduleNames.push_back(flowvr_modname);

   char * moduleName = getenv("FLOWVR_MODNAME_1");

   // Number of total modules (including the 'base' module)
   int numberOfModules = 1;
   while (moduleName != NULL) {
     moduleNames.push_back(moduleName);
     numberOfModules++;
     std::ostringstream oss;
     oss << numberOfModules;
     moduleName = getenv(std::string(("FLOWVR_MODNAME_") + oss.str()).c_str());
   }


  class Axe
  {
  public:
    flowvr::OutputPort port;
    flowvr::OutputPort sumPort;
    float val, sum;
    int it;
    Axe() : port("axe"), sumPort("s_axe"), val(0), sum(0), it(-1) {}
  };



  class Button
  {
  public:
    flowvr::OutputPort port;
    flowvr::OutputPort sumPort;
    bool val, sum;
    int it;
    Button() : port("bt"), sumPort("s_bt"), val(false), sum(false), it(-1) {}
  };

  std::vector<flowvr::ModuleAPI*> modules;
  Axe axe[nbAxes*numberOfModules];
  Button bt[nbButtons*numberOfModules];


  for(int k=0;k<numberOfModules;k++){
      std::vector<flowvr::Port*> port;
  for (int i=0;i<nbAxes;i++)
  { 
    char buf[16]; sprintf(buf,"%d",i);
    axe[i+k*nbAxes].port.name += buf;
    port.push_back(&(axe[i+k*nbAxes].port));
    axe[i+k*nbAxes].sumPort.name += buf;
    port.push_back(&(axe[i+k*nbAxes].sumPort));
  }

  for (int i=0;i<nbButtons;i++)
  {
    char buf[16]; sprintf(buf,"%d",i);
    bt[i+k*nbButtons].port.name += buf;
    port.push_back(&(bt[i+k*nbButtons].port));
    bt[i+k*nbButtons].sumPort.name += buf;
    port.push_back(&(bt[i+k*nbButtons].sumPort));
  }
  modules.push_back(flowvr::initModule(port,"",moduleNames[k]));

  }


  if (numberOfModules > 1) 
    std::cout << "Found environment variables corresponding to " << numberOfModules - 1 << " additional nested modules." << std::endl;

  // Template of loop to run through the additional modules :
  /*
  if (numberOfModules > 1) {
    for (int i = 0; i < (numberOfModules - 1); i++) {
      modules.at(i)...
       
      
    }
  }
  */
  
 

  bool initFailed = false;

    for (int i = 0; i < numberOfModules ; i++) {
      if (modules.at(i)==NULL) {
    	    initFailed=true;
        std::cerr << "ModuleAPI Init Failed in nested module : " << moduleNames.at(i) <<std::endl;
        modules.at(i)->close();
      }
    }

  if (initFailed) {
    close(fd);
    return 1;
  }

  std::vector<flowvr::BufferPool> bp_bts;
  std::vector<flowvr::BufferPool> bp_axes;

    for (int i = 0; i < numberOfModules ; i++) {
      bp_bts.push_back(flowvr::BufferPool(10*nbButtons));
      bp_axes.push_back(flowvr::BufferPool(10*nbAxes));
    }


  std::cout<<"before wait"<<std::endl;  
    for (int i = 0; i < numberOfModules ; i++) {
      std::cout<<"before wait of nested module # " << i << std::endl;
      modules.at(i)->wait();
    }
  std::cout<<"after wait"<<std::endl;




  int it = 0;

  js_event event;
  while (read(fd,&event,sizeof(event)) > 0)
  {
    if(event.type & JS_EVENT_BUTTON)
    {
      std::cout << "Button "<<(int)event.number<<" new val:"<<(int)event.value<<std::endl;
      bool val = (bool)event.value;
      if (event.number >= nbButtons)
	std::cerr << "Button event with invalid number "<<(int)event.number<<std::endl;
      else if (bt[event.number].it<0 || bt[event.number].val != val)
      {

	std::vector<flowvr::MessageWrite> messages;
    	  for (int i = 0; i < numberOfModules ; i++) {
            	flowvr::MessageWrite mtemp;
		mtemp.data = bp_bts.at(i).alloc(modules.at(i)->getAllocator(),1);
		*mtemp.data.getWrite<unsigned char>() = (unsigned char)val;
		messages.push_back(mtemp);
        }

	if (bt[event.number].it == it)
	{
          bool haveToBreak = false;
    	    for (int i = 0; i < numberOfModules; i++) {
      	      if(!modules.at(i)->wait()) haveToBreak = true;
  	  }
          if (haveToBreak) break;
	  ++it;
	}




//	module->put(&bt[event.number].port, m);
//	m.clear();
//	m.data = bp_bt.alloc(module, 1);
//	*m.data.getWrite<unsigned char>() = (unsigned char)bt[event.number].sum;
//	module->put(&bt[event.number].sumPort, m);

    	  for (int i = 0; i < numberOfModules ; i++) {
    			bt[event.number+i*nbButtons].it = it;
    			bt[event.number+i*nbButtons].val = val;
    			bt[event.number+i*nbButtons].sum = !bt[event.number+i*nbButtons].sum;

            	modules.at(i)->put(&bt[event.number+i*nbButtons].port, messages.at(i));
		messages.at(i).clear();
		messages.at(i).data = bp_bts.at(i).alloc(modules.at(i)->getAllocator(), 1);
		*messages.at(i).data.getWrite<unsigned char>() = (unsigned char)bt[event.number+i*nbButtons].sum;
		modules.at(i)->put(&bt[event.number+i*nbButtons].sumPort, messages.at(i));
          }
      }
    }
    else if(event.type & JS_EVENT_AXIS)
    {
        std::vector<flowvr::MessageWrite> messages;

      std::cout << "Axis: " << (int)event.number << " new val:" << (int)event.value << std::endl;
      float val = event.value/32767.0f;
      if (event.number >= nbAxes)
	std::cerr << "Axis event with invalid number "<<(int)event.number<<std::endl;
      else if (axe[event.number].it<0 || axe[event.number].val != val)
      {
	
    	  for (int i = 0; i < numberOfModules ; i++) {
            	flowvr::MessageWrite mtemp;
		mtemp.data = bp_axes.at(i).alloc(modules.at(i)->getAllocator(),sizeof(float));
		*mtemp.data.getWrite<float>() = val;
		messages.push_back(mtemp);
          }
        }



	if (axe[event.number].it == it)
	{
	  bool haveToBreak = false;
    	    for (int i = 0; i < numberOfModules ; i++) {
      	      if(!modules.at(i)->wait()) haveToBreak = true;
  	  }
          if (haveToBreak) break;
	  ++it;
	}


    	  for (int i = 0; i < numberOfModules; i++) {
    			axe[event.number+i*nbAxes].it = it;
    			axe[event.number+i*nbAxes].val = val;
    			axe[event.number+i*nbAxes].sum += val;

            	modules.at(i)->put(&axe[event.number+i*nbAxes].port, messages.at(i));
		messages.at(i).clear();
		messages.at(i).data = bp_axes.at(i).alloc(modules.at(i)->getAllocator(), sizeof(float));
		*messages.at(i).data.getWrite<float>() = axe[event.number+i*nbAxes].sum;
		modules.at(i)->put(&axe[event.number+i*nbAxes].sumPort, messages.at(i));
          }
      }
    }
	  for (int i = 0; i < numberOfModules; i++)
		  modules[i]->close();

  close(fd);
  return 0;
}
