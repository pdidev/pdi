/******************************************************************
*                                                                 *
* File: ./settingFrequency.cpp                                    *
*                                                                 *
* Contacts:                                                       *
*     2009  Yoann KERNOA   <yoann.kernoa@etu.univ-orleans.fr>     *
*                                                                 *
******************************************************************/

/**
 * This module permit you to control a VariableFrequencySynchronizor
 *  through a GLUI window.
 * This window can modify : 
 *  - the current frequency of VariableFrequencySync.
 *  - the lower/upper bound of VariableFrequencySync.
 * 
 * Moreover, when you instantiate a metamoduleSettingFreq, you CAN set :
 *  - the current frequency of the module by "NameModule.setParameter("freqHz",the_current_frequency);"
 *  - the lower bound of the frequency by "NameModule.setParameter("freqMin",the_frequency_min);"
 *  - the upper bound of the frequency by "NameModule.setParameter("freqMax",the_frequency_max);"
 *  - the title of the module by "NameModule.setParameter("title","the_title_ofèthe_module");" => It is useful to name and identify, later, which GLUI window control which VariableFrequencySynchronizor if there are lots of VarFerquencySync.
 * 
 * USEFUL : 
 * 	The scrolling speed of the frequency can vary according to the keys pressed when you want to change the frequency!
 * 		CTRL + button : low scrolling speed to obtain an accurate frequency
 * 		SHIFT+ button : high scrolling speed to rapidly varying frequency
 * 			   button : default scrolling speed to change frequency
 *  
**/

/*!
 * Added :
 * 	you can now control the GUI with the Wiimote!
 *  - touch "+" to increase the frequency
 *  - touch "-" to decrease the frequency
 *  - touch "B" :the frequency decrease/increase 100 times slower/faster
 **/

// Stream includes
#include <iostream>
#include <sstream>
#include <string>

// Others includes
#include <math.h>
#include <stdlib.h>

// glui / glut / glu / gl includes
#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif


// FlowVR includes
#include <flowvr/module.h>
#include <flowvr/xml.h>
#include <flowvr/tinyxml.h>


#include <ftl/chunkevents.h>
#include <ftl/chunkreader.h>

#include <glui.h>

using namespace ftl;


// OpenGL variables ---------------------------------------------------

GLuint main_window;   // ID main Window
GLuint list; // CallList

// CONSTANTS
int WIDTH_WIN      = 330;
int HEIGHT_WIN     = 120;
uint WAIT_TIMER    = 250; // millisecondes


// GLUI variables -----------------------------------------------------

GLUI * glui_subwin;  // ID glui subWindow
GLUI_Spinner *spinner_freq; // controls ...
GLUI_Rollout *rollout_freq;
GLUI_EditText *edit_text_min_freq;
GLUI_EditText *edit_text_max_freq;
GLUI_StaticText *static_text_percent;

// glui variables
float freq;   					 // current frequency (at the beginning)
float freq_min     = 1.0f;       // lower bound of the frequency
float freq_max     = 1000000.0f; // upper bound of the frequceny
float speed_freq   = 0.001f;     // the scrolling speed of the button

// CONSTANTS 
float DEFAULT_FREQ = freq_max/2.0f; // parameterizable in command ligne

enum {SPINNER_FREQ = 0,  // declare the IDs of controls generating callbacks
	  ROLLOUT_MIN_FREQ, 
	  ROLLOUT_MAX_FREQ
	 };
	 

// FlowVR objects & variables -----------------------------------------

flowvr::ModuleAPI* pFlowVRModule   = 0; // to initialize ports, retrieve messages on ports...
flowvr::OutputPort* pPortFreq      = 0; // to send message on the "frequency" port connected to the VARIABLE FREQUENCY SYNCHRONIZOR
flowvr::InputPort* pButton =0;
flowvr::StampInfo * StampTypeFreq  = 0; // to store the frequency type (new,lower bound,upper bound)
flowvr::StampInfo * StampValueFreq = 0; // to store the frequency value
	
flowvr::MessagePut mOut; // Put Message (freq) to synchronizor
flowvr::Message MsgButtonW; // Message from Wiimote

enum typeMsg {NEW=0,MIN,MAX}; // NEW : new frequency, MIN:lower bound of the frequency, MAX : upper bound of the frequency
typeMsg type;  // The nature of the message
bool sendMsg;  // Should we send the message ? it's used to avoid overloading the output port
			   // sendMsg = "true" at the beginning to send a first message to the synchronizor


// Others variables ---------------------------------------------------

std::string title; // In Title bar - useful to identify which module is controled by the GUI
float speed_wiimote=0.001; // CTRL+button : 1% times slower | SHITF(MAJ)+button : 1 times faster | else : +/- 0.1%
bool buttonPlusPressed = false;
bool buttonMinusPressed = false;

// --------------------------------------------------------------------
// Others functions 
// --------------------------------------------------------------------

/**
 * @brief to convert type T on string type
 * @param type T as float,int,...
 * @return type string which contain the value of T
 */
// need include <sstream>
template <typename N>
std::string toString (N n)
{
  	// créer un flux de sortie
    std::ostringstream oss;
 	// écrire un nombre dans le flux
    oss << n;
 	// récupérer une chaîne de caractères
    return oss.str();
}

//ex: std::string s = toString<float>(n); // n : float

// ascii characters : 97(a) -> 122(z)
std::string str2Upper(std::string str)
{
    int leng=str.length();
    for(int i=0; i<leng; i++)
        if (97<=str[i]&&str[i]<=122)//a-z
            str[i]-=32;
    return str;
}

// ascii characters : 65(A) -> 90(Z)
std::string str2Lower(std::string str)
{
    int leng=str.length();
    for(int i=0; i<leng; i++)
        if (65<=str[i]&&str[i]<=90)//A-Z
            str[i]+=32;
    return str;
}


static std::string readEnv(const char* envname) {
	const char* val = getenv(envname);
	if (val==NULL)
		val="";
	//std::cout<<"env."<<envname<<": "<<val<<std::endl;
	return std::string(val);
}

// --------------------------------------------------------------------
// FlowVR functions 
// --------------------------------------------------------------------

/**
 * @brief Initialize FlowVR module, output port, stampList...
 */
int SetupFlowVR()
{
  sendMsg = true;
	
  // Declare user defined ports : ATTENTION les ports doivent avoir le meme nom que ceux définis dans moduleVisu.h
  pPortFreq   = new flowvr::OutputPort("oFreq");    // button pressed
  pButton     = new flowvr::InputPort("Button"); // Wiimote
  // déclaration des stamps
  StampTypeFreq = new flowvr::StampInfo("typeFreq", flowvr::TypeString::create());
  StampValueFreq = new flowvr::StampInfo("valueFreq", flowvr::TypeString::create());

  // ajout des stamps
  pPortFreq->stamps->add(StampTypeFreq);
  pPortFreq->stamps->add(StampValueFreq);


  std::vector <flowvr::Port*> ports;
  ports.push_back(pPortFreq);
  ports.push_back(pButton);
  // Initializes the module to the FlowVR daemon
  if (!(pFlowVRModule = flowvr::initModule(ports)))
  {
    std::cerr << "Can't initialize flowVR module !" << std::endl;
    return -1;
  }

  return 0;
}

/**
 * @brief Clean up properly FlowVR module, output port, stampList...
 */
void CleanFlowVR()
{
  // Release FlowVR module handler :
  if (pFlowVRModule)
  {
    pFlowVRModule->close();

    delete pFlowVRModule;
    pFlowVRModule = 0;
    
    delete pPortFreq;
    pPortFreq = 0;
    
    delete pButton;
    pButton = 0;
    
    delete StampTypeFreq;
    StampTypeFreq = 0;
    
    delete StampValueFreq;
    StampValueFreq = 0;
  }
}


// --------------------------------------------------------------------
// OpenGL functions 
// --------------------------------------------------------------------

/**
 * @brief Initialize display list which is used to obtain an OpenGL progess bar
 */
void InitDisplayList()
{
	/* on cree un objet et on recupere son identifiant */
    list = glGenLists(1);
        
    /* remplissage de notre liste */
    glNewList(list, GL_COMPILE); /* start */
      glPushMatrix();
      glPushAttrib(GL_LINE_BIT ); // to save "Line width"
    
    	 glLineWidth(10.0);
    
    	 glBegin(GL_LINES);
            glVertex2f(0.0f, -1.0f);
            glVertex2f(0.0f,  1.0f);
         glEnd();
    
      glPopAttrib();    	
      glPopMatrix();
    glEndList(); /* stop */
 
} /// afficher avec : glCallList(list); supprimer avec : glDeleteList(list,1); (1 car une seul list a supprimer) 

/**
 * @brief Clean up properly display list
 */
void DeleteDisplayList()
{
	glDeleteLists(list,1);
}

/**
 * @brief Initialize display, projection, clean up buffers...
 * @param width is the width of the window
 * @param height is the height of the window
 */
void InitDisplay(int width, int height)
{
  InitDisplayList();
 
  // Orthogonal projection used here
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glOrtho(-0.1,10+0.1,-1.0,1.0,-0.1,0.1);
  
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity(); // RAZ modelview matrix
    
  // RAZ de la couleur
  glClearColor(0.8f, 0.8f, 0.8f, 1.0f); // background color
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); // clean up buffers
  glutSwapBuffers();
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
  glutSwapBuffers();
}



// --------------------------------------------------------------------
// Glut functions 
// --------------------------------------------------------------------

/**
* @brief reshape callback
* @param width is the new width of the window
* @param height is the new height of the window
*/
void myGlutReshape(int width, int height)
{
  //~ int tx, ty, tw, th;
  //~ GLUI_Master.get_viewport_area (&tx, &ty, &tw, &th );                          
  //~ glViewport( tx, ty, tw, th );

  // ou
  GLUI_Master.auto_set_viewport();
}


/**
* @brief display callback
*/
void myGlutDisplay()
{
	glClear(GL_COLOR_BUFFER_BIT |  GL_DEPTH_BUFFER_BIT); // clean up buffer pixel/depth buffers 	
  	glLoadIdentity(); // RAZ

	glPushMatrix();

	// according to the current frequency (freq), we calculate the percentage of the progress bar
	float percentage = (( (freq-freq_min) / (freq_max-freq_min) ) * 100); // x100 (to obtain %)
	int per = static_cast<int>(percentage); // used in loop FOR
	
	float R = 0.0f;
	float G = 1.0f;
	float B = 0.0f;
	
	// from 0% to percentage%
	for(int i=0; i<= per; i++)
	{
		glColor3f(R,G,B);
		glCallList(list);
		glTranslatef(0.1f,0.0f,0.0f);		
		
		// to obtain a gradation of color (green->yellow->orange->red)
		G = (R>=0.98f and G-0.02f>=0.0f) ? G-0.02f : G;
		R = (G==1.0f and R+0.02f<=1.0f) ? R+0.02f : R;
	}
	
  glPopMatrix();
	
  // Swap Opengl buffers :
  glutSwapBuffers();
}

// timer glut callback -> me permet d'envoyer des msg sur la nvl fréquence toutes les X ms
// afin de surchager le lien vers le synchroniseur (en mode FIFO)
/**
 * @brief timer callback used to send the new message at a frequency of WAIT_TIMER
 * @param value
 */
void myGlutTimer(int value)
{ 
  if(sendMsg) // send a new message ?
    {
      std::string sFreq;
		
      if(type == NEW)
	{
	  sFreq = toString<float>(freq); // convert string to float
	  		
	  mOut.stamps.write(*StampTypeFreq, "NEW");
	  mOut.stamps.write(*StampValueFreq, sFreq);
	  pFlowVRModule->put(pPortFreq , mOut); // send msg

	  sendMsg = false;
	}
      else
	if (type == MIN)
	  {
	    sFreq = toString<float>(freq_min);
	    mOut.stamps.write(*StampTypeFreq, "MIN");
	    mOut.stamps.write(*StampValueFreq, sFreq);
 
	    pFlowVRModule->put(pPortFreq , mOut);	
		  
	    sendMsg = false;
	  }
	else
	  if (type == MAX)
	    {
	      sFreq = toString<float>(freq_max);
	      mOut.stamps.write(*StampTypeFreq, "MAX");
	      mOut.stamps.write(*StampValueFreq, sFreq);
 
	      pFlowVRModule->put(pPortFreq , mOut);	
		  
	      sendMsg = false;
	    }
    }
	
  glutTimerFunc( WAIT_TIMER, myGlutTimer, 1);
}

/**
 * @brief idle callback used to redisplay window when nothing is done
 */
void myGlutIdle () 
{	

  if (!pFlowVRModule->wait())
    {
      // Clean up and exit :
      DeleteDisplayList();
      CleanFlowVR();
      GLUI_Master.close_all();
      exit(0);
    }
  // else, enter into a new iteration
  	
  //  glutSetWindow(main_window);
  //  glutPostRedisplay();

  // std::cout << pButton->isConnected() << std::endl;
  if (pButton->isConnected()) 
  {
      std::cout << "isConnected" << std::endl;
      pFlowVRModule->get(pButton,MsgButtonW);
      for (ftl::ChunkIterator it = ftl::chunkBegin(MsgButtonW); it != ftl::chunkEnd(MsgButtonW) ; it++) 
	  {
    	  const ftl::Chunk* c = (const ftl::Chunk*) it; 	
	      if ((c->type & 0x0F) == ftl::ChunkEvent::BUTTON) 
	      {	
 		    const ftl::ChunkEventButton* button = (const ftl::ChunkEventButton*) c;	  
	      	int btn_key = button->key;
	      	// std::cout << "btn_key:"<< btn_key << std::endl;	  
	      	switch(btn_key) 
			{
				case 2: // button "B" to increase 100 times faster (100 times slower not programming)
					if ( button->val==1) // pressed ?
						speed_wiimote = 0.010f; // 1%
					else // released
						speed_wiimote = 0.001f; // 0.1%
					break;
					
				case 4:  // button "-"
					buttonMinusPressed = button->val;
					break;
				
				case 12: // button "+"
					buttonPlusPressed = button->val;
					break; 
			}			
	      	
	      	float percentage = (( (freq-freq_min) / (freq_max-freq_min) ) * 100); // *100 (to obtain %)
	      	std::string per = "Run to : [ "+toString<float>(percentage)+" % ]";
	      	static_text_percent->set_text(per.c_str());
    					
	      	// When timer will be launched, it will send the new message toward synchronizor
	      	type = NEW;
	      	sendMsg = true;
	       }
	  }
	  
	  if ( buttonMinusPressed )
              //			spinner_freq->set_float_val (spinner_freq->get_float_val() - ((freq_max-freq_min)*speed_wiimote) );
			spinner_freq->set_float_val (spinner_freq->get_float_val() - ((freq_max-freq_min)*speed_wiimote) );
	  if ( buttonPlusPressed )
			spinner_freq->set_float_val (spinner_freq->get_float_val() + ((freq_max-freq_min)*speed_wiimote) );
   }
  
  usleep(200);
}


/**
 * @brief Initialize Glut function/callback
 * @param posx,posy are the position of the window on the screen
 * @param width,height are the width and heigth of the window
 * @param info_synch is a title which is display on the title bar
 */
void SetupGlut(int posx, int posy, int width, int height, std::string info_synch)
{
  std::string  title = "Connected to [ " + info_synch + " ]";

  glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE);
  
  glutInitWindowPosition(posx, posy);
  glutInitWindowSize(width+title.length()*2, height);
  
  
  main_window = glutCreateWindow(title.c_str());
  

  InitDisplay(width, height);

  glutDisplayFunc(myGlutDisplay);
  glutTimerFunc(WAIT_TIMER, myGlutTimer, 1);
  //~ glutReshapeFunc(ReshapeFunc); // via GLUI_MASTER
  //~ glutIdleFunc(IdleFunc); // via glui_window
}



// --------------------------------------------------------------------
// GLui functions
// --------------------------------------------------------------------

/**
 * @brief Glui callback
 * @param control_id is an identifiant to know which control has been modified
 */
void glui_callback (int control_id)
{
  std::string per;
  float percentage;

  switch (control_id)
  {
  	//  new frequency
  	case SPINNER_FREQ:
		
    	// update percentage
      	percentage = (( (freq-freq_min) / (freq_max-freq_min) ) * 100); // *100 (to obtain %)
      	per = "Run to : [ "+toString<float>(percentage)+" % ]";
      	static_text_percent->set_text(per.c_str());
    					
      	// When timer will be launched, it will send the new message toward synchronizor
      	type = NEW;
      	sendMsg = true;
    					
      	break;
						
    // new lower bound 
    case ROLLOUT_MIN_FREQ:
    	// on test si valeur ok + change les valeurs min du spinner + nouveau % + envoie vers synchroniseur a la fin   					
						
      	// Test if values are correct
      	edit_text_min_freq->set_float_limits(0.1f,freq_max-1.0f); // set limits of editTexts
      	edit_text_max_freq->set_float_limits(freq_min+1.0f, freq_max*100.0f);
		
      	// bring back frequency "freq" between bounds if necessary
      	// i.e if the new lower bound frequency is > current frequency, bring back frequency at the current lower bound frequency
      	if(freq < freq_min)	
		{
	  		freq = freq_min;
	  		spinner_freq->set_float_val (freq);	
		}
      	// set freq_min and freq_max
      	spinner_freq->set_float_limits(freq_min, freq_max);
      	// update percentage
      	percentage = (( (freq-freq_min) / (freq_max-freq_min) ) * 100); // *100 (to obtain %)
      	per = "Run to : [ "+toString<float>(percentage)+" % ]";
      	static_text_percent->set_text(per.c_str());
    					
      	// When timer will be launched, it will send the new message toward synchronizor
      	type = MIN;
      	sendMsg = true;

      	break;
						
    // new upper bound
    case ROLLOUT_MAX_FREQ:
      	// on test si valeur ok +  change les valeurs max du spinner + nouveau % + envoie vers synchroniseur
						
      	// test if values are correct
      	edit_text_min_freq->set_float_limits(0.1f,freq_max-1.0f); // set limits of editTexts
      	edit_text_max_freq->set_float_limits(freq_min+1.0f, freq_max*100.0f);
	
      	// bring back frequency "freq" between bounds if necessary
      	// i.e if the new upper bound frequency is < current frequency, bring back frequency at the current upper bound frequency
      	if(freq > freq_max)	
		{
	  		freq = freq_max;
	  		spinner_freq->set_float_val (freq);
		}
      	// set variables freq_min and freq_max
      	spinner_freq->set_float_limits(freq_min, freq_max);
      	// udpate percentage
      	percentage = (( (freq-freq_min) / (freq_max-freq_min) ) * 100); // *100 (to obtain %)
      	per = "Run to : [ "+toString<float>(percentage)+" % ]";
      	static_text_percent->set_text(per.c_str());
    					
      	// When timer will be launched, it will send the new message toward synchronizor    					 
      	type = MAX;
      	sendMsg = true;
    					
      	break;
											
    default:
      break;
   }
}

/**
 * @brief Initialize Glui interface
 */
void SetupGlui()
{
  glui_subwin = GLUI_Master.create_glui_subwindow (main_window, GLUI_SUBWINDOW_TOP );

  // ----------------------------------------------------------------
  // add panel ------------------------------------------------------
  // ----------------------------------------------------------------

  GLUI_Panel *obj_panel = glui_subwin->add_panel ("Frequency (Hertz)");

  // add text (espace) ----------------------------------------------
  GLUI_StaticText *static_text = glui_subwin->add_statictext_to_panel(obj_panel,"");
		
	
  // add spinner ----------------------------------------------------
  spinner_freq = glui_subwin->add_spinner_to_panel( obj_panel, "Cur. freq.",GLUI_SPINNER_FLOAT, &freq, SPINNER_FREQ, glui_callback);
  // set limits of spinner (in Hertz)   
  spinner_freq->set_float_limits(freq_min, freq_max);
  // set step when you click on arrow
  spinner_freq->set_speed(speed_freq); // sinon, ça va bcp trop vite
  //set width / height
  spinner_freq->set_w(145);
  //~ spinner_freq->set_h(10);
  // set value per default
  spinner_freq->set_float_val (DEFAULT_FREQ);
  // set alignment of spinner
  spinner_freq->set_alignment(GLUI_ALIGN_LEFT);
	
  // add separator
  glui_subwin->add_separator_to_panel(obj_panel);
	
  // calcul du premier pourcentage ----------------------------------
  float percentage = (( (DEFAULT_FREQ-freq_min) / (freq_max-freq_min) ) * 100); // *100 (to obtain %)
  std::string per = "Run to : [ "+toString<float>(percentage)+" % ]";
	
  // add static text to display percentage
  static_text_percent = glui_subwin->add_statictext_to_panel(obj_panel, per.c_str());
	
	
  // ----------------------------------------------------------------
  // add separator (vertical) - séparateur non visible (visible -> mettre true)
  // ----------------------------------------------------------------
  glui_subwin->add_column(false);
	
	
  // ----------------------------------------------------------------
  // add Rollout to control Min/Max frequency -----------------------
  // ----------------------------------------------------------------
	
  rollout_freq = glui_subwin->add_rollout ("Set frequency (Hertz)");
	
  // add min/max edit text  -----------------------------------------
  edit_text_min_freq = glui_subwin->add_edittext_to_panel(rollout_freq, "Min freq", GLUI_EDITTEXT_FLOAT, &freq_min, ROLLOUT_MIN_FREQ, glui_callback);                			
  edit_text_max_freq = glui_subwin->add_edittext_to_panel(rollout_freq, "Max freq", GLUI_EDITTEXT_FLOAT, &freq_max, ROLLOUT_MAX_FREQ, glui_callback);                			
	
  edit_text_min_freq->set_w(145); // set width of editText
  edit_text_max_freq->set_w(145);
	
  edit_text_min_freq->set_float_limits(0.1f,freq_max-1.0f); // set limits of editTexts
  edit_text_max_freq->set_float_limits(freq_min+1.0f, freq_max*100.0f);
		
		
  // ----------------------------------------------------------------
  // Set callbacks and others functions -----------------------------
  // ----------------------------------------------------------------
	
  // synchronize all live variables
  GLUI_Master.sync_live_all();
  //  Set idle function
  GLUI_Master.set_glutIdleFunc (myGlutIdle);
  // set others glut callbacks
  GLUI_Master.set_glutReshapeFunc (myGlutReshape);
	

  /** Tell the new subwindow which graphics window it should send redisplay events to **/
  glui_subwin->set_main_gfx_window( main_window );
}


// --------------------------------------------------------------------
// Initialize parameters "Freq" and "Title"
// --------------------------------------------------------------------

void initParametersFromFile(flowvr::xml::DOMElement* xmlRoot)
{
  
  // freqHz parameter reading ...
  flowvr::xml::DOMNodeList* lfreq = xmlRoot->getElementsByTagName("freqHz");
  if (lfreq != NULL && lfreq->getLength()>=1)
    {
      std::string fr = lfreq->item(0)->getTextContent();
      if (!(fr.empty()))
	DEFAULT_FREQ = atof(fr.c_str());
      delete lfreq;
	  
      // test : freq_min <= DEFAULT_FREQ <= freq_max ?
      if (DEFAULT_FREQ > freq_max || DEFAULT_FREQ < freq_min )
	{
	  // incorrect value for frequency in hertz
	  DEFAULT_FREQ = freq_max/2.0f;
	  std::cerr<<"Frequency parameter is not correct - use of default value ("<<DEFAULT_FREQ<<")"<<std::endl;
	}
    }
  else
    {
      // incorrect value for frequency in hertz
      DEFAULT_FREQ = freq_max/2.0f;
      std::cerr<<"Frequency parameter is not correct - use of default value ("<<DEFAULT_FREQ<<")"<<std::endl;
    }
  
  // titleHz parameter reading...
  flowvr::xml::DOMNodeList* ltitle = xmlRoot->getElementsByTagName("titleHz");
  if (ltitle != NULL && ltitle->getLength()>=1)
    {
      std::string t = ltitle->item(0)->getTextContent();
      if (!(t.empty()))
	title = t;
	  
      delete ltitle;
    }
  else
    {
      // incorrect value for title
      std::cerr<<"Title parameter is not correct - use of default value ("<<title<<")"<<std::endl;
    }
}

// search the file which contains values of parameters : frequency - title of the window
void initParameters()
{
  title = "Unidentified module";
  DEFAULT_FREQ = freq_max/2.0f;
  
  // retrieve "FLOWVR_MODNAME" which contains "name_application/name_module/..."
  std::string modulename = readEnv("FLOWVR_MODNAME");
    
  // extract the name of the application lauched
  std::string filename, filepath, command;
  for(int i=0; i<modulename.length(); i++)
    {
      if(modulename[i] != '/')
	filename += modulename[i];
      else
	break;
    }
  std::cout<<"filename :"<<filename<<std::endl;
  filename +=".net.xml"; // add the extension ".net.xml"
  command = "locate -i "+filename; // to locate the file path via system command
  
  // search the path of the file
  system(command.c_str());
  
  // load parameters 
  flowvr::xml::DOMParser *xmlDoc = new flowvr::xml::DOMParser();
  xmlDoc->parse(filename.c_str()); // open file
  
  initParametersFromFile(xmlDoc->getDocument()->RootElement()); // load parameters
}

// --------------------------------------------------------------------
// Main routine
// --------------------------------------------------------------------

int main(int argc, char ** argv)
{
  
  // Load parameters --------------------------------------------------
  
  initParameters();
    
  // GUI --------------------------------------------------------------
   
  // Init FlowVR environment :
  if (SetupFlowVR() != 0)
    return -1;
 
  // Initialize GLUT :
  glutInit(&argc, argv);
    
  // Initialize window :
  SetupGlut(900, 0, WIDTH_WIN, HEIGHT_WIN, title);
  
  // Initialize GLUI :
  SetupGlui();
  
  // Main execution loop :
  glutMainLoop();

  return 0;
}
