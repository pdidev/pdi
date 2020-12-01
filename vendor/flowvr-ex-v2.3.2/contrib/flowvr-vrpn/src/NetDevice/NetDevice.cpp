/******* COPYRIGHT ************************************************
*                                                                 *
*                         FlowVR VRPN                             *
*                    FlowVR VRPN Coupling Modules                 *
*                                                                 *
*-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (EA 4022) ALL RIGHTS RESERVED.                                  *
*                                                                 *
* This source is covered by the GNU GPL, please refer to the      *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Sebastien Limet,                                             *
*    Sophie Robert.                                               *
*                                                                 *
*******************************************************************
*                                                                 *
* File: ./src/SimulatedDevice/SimulatedDevice.cpp                 *
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
//VRPN IMPORTS
//#include <vrpn_Tracker.h>
//#include <vrpn_ForceDevice.h>
#include <vrpn_Button.h>
#include <vrpn_Analog.h>
#include <flowvr_vrpn_stamps.h>

//FlowVR IMPORTS
#include <flowvr/moduleapi.h>

//other includes
#include "InputDevice.h"
#include <iostream>
#include <vector>


#include <quat.h>
#include <math.h>


#include <unistd.h>

// nécessaire à la manipulation de sockets
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <stdio.h>
#include <errno.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdlib.h>
#include <string.h>
#include <stddef.h>
#include <fcntl.h>

 
#define PORT_SERV 8080
#define TAILLE_TAMPON 20



using namespace std;
using namespace flowvr;

ModuleAPI* flowvrmodule = 0;

OutputPort pOut("vrpnmsg");
std::vector<flowvr::Port*> ports;

MessageWrite m;
StampInfo *TypeMsg = new StampInfo("TypeMsg",TypeInt::create());


#define NB_BUTTON_PER_COLUMN 4

int controle = 0;



  int cli; // l'identifiant de la socket client
  int socket_serveur;
  int socket_client;
  
  int msg;
  
  // l'adresse de la socket serveur
  //unsigned short int htons_port_serv = htons(PORT_SERV);
  //unsigned long int htonl_inaddr_any = htonl(INADDR_ANY);

  struct sockaddr_in addr_serveur; //= {AF_INET,htons(PORT_SERV), htonl(INADDR_ANY)};
  
  //l'adresse de la socket client
  struct sockaddr addr_client;
  
  socklen_t addr_cli_size=sizeof(addr_client);
  
  char mot[20];
  
 

// sending one button value
void sendButton(unsigned char button, int state){
  vrpn_BUTTONCB b;

	b.button=button;
	b.state=state;

  // !! timestamps
  gettimeofday(& b.msg_time,NULL);
  
  
  cout << "Button :" << b.button << " " <<  b.state << endl;

  int taille = sizeof(b);
  m.data = flowvrmodule->alloc(taille);
  memcpy(m.data.writeAccess(),(void*)(&b),taille);
  m.stamps.write(*TypeMsg,flowvr_vrpn_BUTTONCB);
  if (flowvrmodule->wait()) {
    //    cout << "sending button\n";
    flowvrmodule->put(&pOut,m);
  }

  else {
    flowvrmodule->close();
    exit(0);
  }
}

// sending the axes
void sendAnalog(vector <struct_axes> analog){
  vrpn_ANALOGCB a;

  vector<struct_axes>::iterator it1;

  for(it1 = analog.begin(); it1 != analog.end(); it1++){
	cout<<"id analog = "<<(*it1).id<<" val analog = "<<(*it1).val<<endl;
    a.channel[(*it1).id]=(*it1).val;
  }
	
  a.num_channel=analog.size();
  // !! timestamps
  gettimeofday(& a.msg_time,NULL);


   int taille = sizeof(a);
   for (int i=0; i< a.num_channel; i++)
          cout << "channel " << i << ":" << a.channel[i] << endl;

  m.data = flowvrmodule->alloc(taille);
  memcpy(m.data.writeAccess(),(void*)(&a),taille);
  m.stamps.write(*TypeMsg,flowvr_vrpn_ANALOGCB);
  if (flowvrmodule->wait()) {
        cout << "sending analog\n";
    flowvrmodule->put(&pOut,m);
  }
  else  {
    flowvrmodule->close();
    exit(0);
  }
}


void sendEmptyMsg() {
  //cerr << "sendEmptyMsg()" << endl;
  //flowvrmodule->put(&pOut,m);
  int toto = 1;
  m.data = flowvrmodule->alloc(sizeof(toto));
  memcpy(m.data.writeAccess(),(void*)(&toto),sizeof(toto));
  m.stamps.write(*TypeMsg,flowvr_vrpn_NODATA);
  if (flowvrmodule->wait()) {
    //cerr << "sending empty msg\n";
    flowvrmodule->put(&pOut,m);
  }
  else  {
    flowvrmodule->close();
    exit(0);
  }
}



void boucle(){


int buttonNumber;
int buttonState;
vector <struct_axes> vectorAxes;
struct_axes axes;
int sent=0;

sendButton(static_cast<unsigned char>(0), 0); 

for (int i = 0; i < 3; i++) {
	struct_axes temp;
	temp.id = i;
	temp.val = 0.0;
	vectorAxes.push_back(temp);
}
sendAnalog(vectorAxes);


while(1){


  

  vrpn_SleepMsecs(10);
  //vector <struct_buttons> v=dev->getButtonChanges();
  vector<struct_buttons>::iterator it1;
 


    // ------------------------------------------
    // on attend une connexion
    // on attend 1 connexion sur la socket serveur
    listen(socket_serveur, 10);
    
    //------------------------------------------
    // on accepte une connexion
    // sur la socket serveur
    // venant d'une addresse client
    // de taille...
    socket_client = accept(socket_serveur,&addr_client,&addr_cli_size);

	if(socket_client>=0){
    
    cerr<<"La socket du client : "<<socket_client<<endl;
    
    
    // ------------------------------------------
    // on attend un message
    // identifiant de la socket du client
    // adresse pour recevoir le message
    // taille de la zone de message
    // des options
    // cli = recv(socket_client,(int*)&msg, 4,0);
    cli = read(socket_client, mot, TAILLE_TAMPON);
    cout<<"On a lu "<<cli<<" octets"<<endl;
    		mot[cli]= '\0';
    		cout<<"Le message est le suivant : "<<mot<<endl<<endl;
		//int essai=0;

	std::string text(mot);
	buttonNumber = -1;
	axes.id = -1;
	

	if(text=="monter" || text=="stopmonter") { buttonNumber = 0; }
	if(text=="descendre" || text=="stopdescendre") { buttonNumber = 1; }
	if(text=="regarderhaut" || text=="stopregarderhaut") { buttonNumber = 2; }
	if(text=="regarderbas" || text=="stopregarderbas") { buttonNumber = 3; }
	if(text=="reset") {  buttonNumber = 8; }
	if(text=="fildefer") {  buttonNumber = 11; }

	if(text.substr(0, 4) == "stop") { buttonState = 0;}
	else { buttonState = 1;}

	if(buttonNumber > -1) sendButton(static_cast<unsigned char>(buttonNumber), buttonState);
	


	if(text=="avancer" || text=="stopavancer" || text=="reculer" || text=="stopreculer") {  axes.id = 1; }
	if(text=="decalagedroite" || text=="stopdecalagedroite" || text=="decalagegauche" || text=="stopdecalagegauche") {  axes.id = 0; }
	if(text=="tournerdroite" || text=="stoptournerdroite" || text=="tournergauche" || text=="stoptournergauche") { axes.id = 2; }

	if(text=="reculer" || text=="decalagedroite" || text=="tournerdroite" ) { axes.val = 1.0; }
	if(text=="avancer" || text=="decalagegauche" || text=="tournergauche" ) { axes.val = -1.0; }
	if(text.substr(0, 4) == "stop") { axes.val = 0.0;}


	if(axes.id > -1) {
		vectorAxes.at(axes.id).val = axes.val;
		sendAnalog(vectorAxes);
	}

	


	}
	

}
}

/*
// callback functions for the release buttons of sticks and Axes

void resetStickCb( int id )
{
  dev->updateStick(id,0.,0.);
}

void resetAxeCb( int id )
{
  dev->updateAxe(id,0.);
}
*/

/*
void parseDevice( InputDevice *dev, std::string fileName ) {
  flowvr::xml::DOMNode * rootnode;
  flowvr::xml::DOMNode * node1;
  flowvr::xml::DOMNode * node2;
  flowvr::xml::DOMNode * node3;
  flowvr::xml::DOMDocument * file;

  std::string name;
  std::string id1;
  std::string id2;

  file = new DOMDocument(fileName);
  if (!file->LoadFile()){
    cerr << "Loading Error..." << endl;
    cerr << "error #" << file->ErrorId() << " : " << file->ErrorDesc() << endl;
    exit(1);
  }


  // getting the device name
  rootnode= file->FirstChildElement("device");
  name=rootnode->ToElement()->Attribute("name");


  // creating the device
  dev->setName(name);



  // going to the input element
  node1= rootnode->FirstChildElement("input");

  // first create the buttons

  node2= node1->FirstChildElement("buttons");
  if (node2) {
    // parsing all the buttons
    node3= node2->FirstChildElement("button");

    while (node3) {
      id1=node3->ToElement()->Attribute("id");
      name=node3->ToElement()->Attribute("name");
      (*dev).addButton( new Button(name.c_str(), atoi(id1.c_str())));
      node3=node3->NextSiblingElement("button");
    }
  }

  //going to the analog elements
  node2=node1->FirstChildElement("analog");
  if (node2) {
    // parsing all the dpad
    node3= node2->FirstChildElement("dpad");

    while (node3) {
      id1=node3->ToElement()->Attribute("idtd");
      id2=node3->ToElement()->Attribute("idlr");
      name=node3->ToElement()->Attribute("name");
      (*dev).addStick( new Stick(name.c_str(),
				 atoi(id1.c_str()),
				 atoi(id2.c_str()),0));
      node3=node3->NextSiblingElement("dpad");
    }


    // parsing all the sticks
    node3= node2->FirstChildElement("stick");

    while (node3) {
      id1=node3->ToElement()->Attribute("idtd");
      id2=node3->ToElement()->Attribute("idlr");
      name=node3->ToElement()->Attribute("name");
      (*dev).addStick( new Stick(name.c_str(),
				 atoi(id1.c_str()),
				 atoi(id2.c_str()),1));
      node3=node3->NextSiblingElement("stick");
    }


    // parsing all the sliders
    node3= node2->FirstChildElement("slider");

    while (node3) {
      id1=node3->ToElement()->Attribute("id");
      name=node3->ToElement()->Attribute("name");
      (*dev).addAxe( new Axe(name.c_str(),
				   atoi(id1.c_str())));
      node3=node3->NextSiblingElement("slider");
    }
  }


  // going to the trackers
  node2= node1->FirstChildElement("tracker");
  if (node2) {
    // getting the name of the tracker
    name=node2->ToElement()->Attribute("name");

    float min[3];
    float max[3];
    // getting the workspace bounds
    if (node3 = node2->FirstChildElement("workspace")) {
      id1=node3->ToElement()->Attribute("minX");
      min[0]=atof(id1.c_str());
      id1=node3->ToElement()->Attribute("minY");
      min[1]=atof(id1.c_str());
      id1=node3->ToElement()->Attribute("minZ");
      min[2]=atof(id1.c_str());

      id1=node3->ToElement()->Attribute("maxX");
      max[0]=atof(id1.c_str());
      id1=node3->ToElement()->Attribute("maxY");
      max[1]=atof(id1.c_str());
      id1=node3->ToElement()->Attribute("maxZ");
      max[2]=atof(id1.c_str());

      (*dev).setWorkspace(min,max);
    } //    if (node3 = node2->FirstChildElement("workspace"))

    // parsing all the sensors
    node3= node2->FirstChildElement("sensor");

    while (node3) {
      id1=node3->ToElement()->Attribute("id");
      name=node3->ToElement()->Attribute("name");
      if (node3->FirstChildElement("position")){
	(*dev).addTracker( new Tracker(name.c_str(),atoi(id1.c_str())));
      }

      //     if (node3->FirstChildElement("velocity")){
      //       cout << "velocity" << endl;
      //     }

      //     if (node3->FirstChildElement("position")){
      //       cout << "position" << endl;
      //     }

      //     type=node3->ToElement()->Attribute("type");
      //     cout << "sensor type " << type<< endl;
      node3=node3->NextSiblingElement("sensor");
    } //     while (node3)
  } //   if (node2)
}
*/
/*
void buildInterface(InputDevice dev)
{

  // these variables are used to better present the buttons
  int cnt;
  int first;
  // building the buttons
  vector<Button*>::iterator it1;
  vector<Button *> vb=dev.getButtons(); 

  if (!vb.empty()){
    // there are some buttons on this device
    cnt=0;
    first=1;
    // creating a pannel for these buttons
    buttonPanel = glui->add_panel("Buttons");
    //adding the buttons to the panel
    for(it1 = vb.begin(); it1 != vb.end(); it1++){
      if ((!first)&& (cnt%NB_BUTTON_PER_COLUMN==0))
	// every 4th button a new column is created
	glui->add_column_to_panel(buttonPanel,true);
      (*it1)->setSimButton(glui,buttonPanel);
      cnt++;
      first=0;
    }
  }

  // building the sticks
  vector<Stick *>::iterator it3;
  vector<Stick *> vs=dev.getSticks();

  if (!vs.empty()){
    // there are some sticks on this device
    // creating a pannel for these sticks
    stickPanel = glui->add_panel("Sticks");

    first=1;
    // adding the sticks to the panel
    for(it3=vs.begin();  it3!=vs.end(); it3++){
      if (!first)
	// one column per axe
	glui->add_column_to_panel(stickPanel,false);
      (*it3)->setSimStick(glui,stickPanel,resetStickCb);
      first=0;
    }
  }

  // building the axes
  vector<Axe *>::iterator it2;
  vector<Axe *> va=dev.getAxes();
  if (!va.empty()){
    // there are some axes on this device
    // creating a pannel for these axes
    axePanel = glui->add_panel("Sliders");

    first=1;
    // adding the sliders to the panel
    for(it2=va.begin();  it2!=va.end(); it2++){
      if (!first)
	// one column per axe
	glui->add_column_to_panel(axePanel,false);

      (*it2)->setSimAxe(glui,axePanel,resetAxeCb);
      first=0;
    }
  }

  // building the trackers
  vector<Tracker *>::iterator it4;
  vector<Tracker *> vt=dev.getTrackers();
  if (!vt.empty()){
    // there are some trackers on this device
    // creating a pannel for these trackers
    trackerPanel = glui->add_panel("Trackers");

    first=1;
    // adding the trackers to the panel
    for(it4=vt.begin();  it4!=vt.end(); it4++){
      if (!first)
	// one column per tracker
	glui->add_column_to_panel(trackerPanel,true);

      (*it4)->setSimTracker(glui,trackerPanel);
      first=0;
    }
  }


}
*/

void initSockets() {

 addr_serveur.sin_family = AF_INET;
  addr_serveur.sin_port = htons(PORT_SERV);
  addr_serveur.sin_addr.s_addr = htonl(INADDR_ANY);
  bzero(&(addr_serveur.sin_zero), 8);

// ------------------------------------------
  // on crée une socket et on recupère son descripteur. Les param :
  // PF_INET,le domaine, signifie qu'on utilise IPv4
  // SOCK_STREAM, pour dire qu'on envoye un flux
  // 0, mais on c'est deja que c'est du TCP
  socket_serveur = socket(AF_INET, SOCK_STREAM, 0);
 
  
  // IMPORTANT : on rend la socket non bloquante, sinon l'application se bloquerai en attendant des connexions (manip clavier/souris et joypad    impossibles dans ce cas là)
  fcntl(socket_serveur, F_SETFL, O_NONBLOCK); 
  

  //------------------------------------------
  // dans le cas du serveur il faut faire un bind
  // identifiant de la socket
  // adresse de la socket
  // taille de l'adresse
  bind(socket_serveur,(struct sockaddr*)&addr_serveur,sizeof(addr_serveur));
}

/**************************************** main() ********************/

int main(int argc, char* argv[])
{
  //addr_serveur = struct sockaddr_in {AF_INET,htons(PORT_SERV), htonl(INADDR_ANY)};
 
  // checking the arguments
/*
  if (argc != 2) {
    cerr <<argc << "usage: simulateddevice XMLDEVICEDESCRIPTION"<<endl;
    exit(1);
  }
*/
  
  

  initSockets();




  /****************************************/
  /*   Initialize GLUT and create window  */
  /****************************************/

  // Partie FlowVR
  pOut.stamps->add(TypeMsg);
  ports.push_back(&pOut);


  flowvrmodule = initModule(ports);

  if (!flowvrmodule)
    return -1;

  /*
  glutInit(&argc, argv);
  glutInitWindowPosition( 50, 50 );

  parseDevice( dev, (std::string)argv[1] );
  glui = GLUI_Master.create_glui( (char*)dev->getName().c_str() );
  buildInterface( *dev);

  GLUI_Master.set_glutIdleFunc( myGlutIdle );

  glutMainLoop();
  */

  boucle();

  flowvrmodule->close();


  close(socket_serveur);


  return EXIT_SUCCESS;
}
