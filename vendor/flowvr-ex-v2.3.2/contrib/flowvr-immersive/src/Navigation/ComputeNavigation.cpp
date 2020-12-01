#include <stdio.h>
#include <math.h>
#include <iostream>
#include <string>
#include <sstream>

//FlowVR Imports
#include <flowvr/moduleapi.h>
#include <flowvr/xml.h>
#include <flowvr_vrpn_stamps.h>
#include <vrpn_Button.h>
#include <vrpn_Analog.h>

using namespace std;
using namespace flowvr;

// To check ExaViz successful initialization
const char* envpath;

// FlowVR part
ModuleAPI * FlowVRModule = 0;
InputPort * pInNav = new InputPort("ana_button"); // Absolute position and associated quaternion of the user sent by VRPN
InputPort * pInRenduMat = new InputPort("rendu_mat"); // Modelview matrix from rendering module

OutputPort pOutModelMat("modelview_mat");

StampInfo *TypeMsgStamp = new StampInfo("TypeMsg",TypeInt::create());

Message mInNav;
Message mInrenduMat;
MessageWrite mOutModelMat;

vrpn_ANALOGCB a;
vrpn_BUTTONCB b={0,0,0};

float xyz_move[3];
float quat[4];
int c = 0;

float modelview_mat[16];
float model_mat[16];
float tmp_mat[16];

float navigation_info[132];

bool render_info = false;

char * device;

// Information received from VRPN (buttons events)
bool button_pressed;
bool analog_used;
int button[2];
// Information received from VRPN (analog events)
float gravity[3];
float old_gravity[3];
bool calibrated = false;

int typemsg;

float rotation_mat[16] = {1.0f,0.0f,0.0f,0.0f,
              0.0f,1.0f,0.0f,0.0f,
              0.0f,0.0f,1.0f,0.0f,
              0.0f,0.0f,0.0f,1.0f};

float move_mat[16] = {1.0f,0.0f,0.0f,0.0f,
              0.0f,1.0f,0.0f,0.0f,
              0.0f,0.0f,1.0f,0.0f,
              0.0f,0.0f,0.0f,1.0f};

float transformation_mat[16] = {1.0f,0.0f,0.0f,0.0f,
              0.0f,1.0f,0.0f,0.0f,
              0.0f,0.0f,1.0f,0.0f,
              0.0f,0.0f,0.0f,1.0f};

void Quat2RotationMatrix(float quat[4], float rotation_mat[16])
{
    float n = 1.0f/sqrt(quat[0]*quat[0]+quat[1]*quat[1]+quat[2]*quat[2]+quat[3]*quat[3]);
    #ifdef DEBUG_MESSAGE
        for (int i=0; i<4; i++)
        {
            cout << quat[i] << " " << n << endl;
            quat[i]*= n;
            cout << quat[i] << endl;
        }
    #endif
    float qx = quat[0];
    float qy = quat[1];
    float qz = quat[2];
    float qw = quat[3];

    
    rotation_mat[0] = 1.0f-2.0*qy*qy-2.0*qz*qz;
    rotation_mat[1] = 2.0*qx*qy-2.0*qz*qw;
    rotation_mat[2] = 2.0*qx*qz+2.0*qy*qw;
    rotation_mat[3] = 0.0f;
    rotation_mat[4] = 2.0*qx*qy+2.0*qz*qw;
    rotation_mat[5] = 1.0f-2.0*qx*qx-2.0*qz*qz;
    rotation_mat[6] = 2.0*qy*qz-2.0*qx*qw;
    rotation_mat[7] = 0.0f;
    rotation_mat[8] = 2.0*qx*qz-2.0*qy*qw;
    rotation_mat[9] = 2*qy*qz+2.0*qx*qw;
    rotation_mat[10] = 1.0f-2.0*qx*qx-2.0*qy*qy;
    rotation_mat[11] = 0.0f;
    rotation_mat[12] = 0.0f;
    rotation_mat[13] = 0.0f;
    rotation_mat[14] = 0.0f;
    rotation_mat[15] = 1.0f;
    
    // cout << "Rot_Mat: " << endl;
    // printf("%f\t%f\t%f\t%f \n", rot_mat[0], rot_mat[1], rot_mat[2], rot_mat[3]);
    // printf("%f\t%f\t%f\t%f \n", rot_mat[4], rot_mat[5], rot_mat[6], rot_mat[7]);
    // printf("%f\t%f\t%f\t%f \n", rot_mat[8], rot_mat[9], rot_mat[10], rot_mat[11]);
    // printf("%f\t%f\t%f\t%f", rot_mat[12], rot_mat[13], rot_mat[14], rot_mat[15]);      
    // printf("\n");
}

void FromVec2Mat(float eyes_vec[3], float eyes_mat[16])
{
    eyes_mat[3] = eyes_vec[0];
    eyes_mat[7] = eyes_vec[1];
    eyes_mat[11] = eyes_vec[2];
}

void MultMatrix(float *matleft,float *matright, float *mat){

    //float* mat = (float*) malloc(sizeof(float)*16);
    
    printf("%s\n", "MatLeft");
    for (int i = 0; i < 16; i++) 
    {
        printf("%f ", matleft[i]);
    }
    printf("\n");

    for (int i = 0; i < 4; i++)
    {
        for (int j = 0; j < 4; j++)
        {
            float sum = 0.0f;
            for (int k = 0; k < 4; k++)
            {
          //                sum = sum + ( floorf(matleft[i * columnsa + k] * 1000 +0.5) /1000 ) * ( floorf(matright[k * columnsb + j] *1000 + 0.5) /1000);
                sum = sum + ( matleft[i * 4 + k] ) * ( matright[k * 4 + j] );
            }
            mat[i * 4 +j] = sum ;
        }
    }
}

void TransposeMat(float *input, float *output)
{

    for(int i=0; i<4; i++)
    {
        for(int j=0; j<4; j++)
        {
            output[j*4 + i] = input[i*4 + j];
        }
    }
}

void SetIdentity(float *mat)
{
    for(int i=0; i<16; i++)
    {
        mat[i] = 0.0f;
    }
    mat[0] = 1.0f;
    mat[5] = 1.0f;
    mat[10] = 1.0f;
    mat[15] = 1.0f;       
}

float* getModelViewMat(float transformation_mat[16])
{
    Quat2RotationMatrix(quat, rotation_mat);
    FromVec2Mat(xyz_move, move_mat);
    MultMatrix(move_mat, rotation_mat, transformation_mat);

}

void MoveLeft(float *mat)
{
    //SetIdentity(mat);
    mat[3] = 0.1f;
}

void MoveRight(float *mat)
{
    //SetIdentity(mat);
    mat[3] = -0.1f;
}

void MoveUp(float *mat)
{
    //SetIdentity(mat);
    mat[7] = -0.1f;
}

void MoveDown(float *mat)
{
    //SetIdentity(mat);
    mat[7] = 0.1f;
}

void Zoom(float *mat)
{
    //SetIdentity(mat);
    float delta = old_gravity[2] - gravity[2];
    if (delta > 0.1 || delta < -0.1){
        mat[11] = delta * 10;
    }
}

float* Wiimote_button(float transformation_mat[16])
{
    if (button[0] == 7 and button[1] == 1)
    {
        MoveLeft(transformation_mat);
        button_pressed = true;
    }
    else if(button[0] == 7 and button[1] == 0)
    {
        button_pressed = false;
        SetIdentity(transformation_mat);
    }
    else if (button[0] == 8 and button[1] == 1)
    {
        MoveRight(transformation_mat);
        button_pressed = true;
    }
    else if(button[0] == 8 and button[1] == 0)
    {
        button_pressed = false;
        SetIdentity(transformation_mat);
    }
    else if (button[0] == 9 and button[1] == 1)
    {
        MoveDown(transformation_mat);
        button_pressed = true;
    }
    else if(button[0] == 9 and button[1] == 0)
    {
        button_pressed = false;
        SetIdentity(transformation_mat);
    }
    else if (button[0] == 10 and button[1] == 1)
    {
        MoveUp(transformation_mat);
        button_pressed = true;
    }
    else if(button[0] == 10 and button[1] == 0)
    {
        button_pressed = false;
        SetIdentity(transformation_mat);
    }
}

float* Wiimote_analog(float transformation_mat[16])
{
    Zoom(transformation_mat);
}

int check_zero_mat(const float a[], int n)
{   
    while(--n>0 && a[n]==a[0]);
    return n!=0;
}

void getNavInfo()
{

    cout << "Entered in getNavInfo" << endl;
    while (FlowVRModule->wait())
    {
	   // Check modules connection
        cout << "Entered in FlowVR loop" << endl;
	    if (pInNav->isConnected())
        {
            cout << "Button and Analog connected" << endl;
            FlowVRModule->get(pInNav, mInNav);
            mInNav.stamps.read((*TypeMsgStamp),typemsg);
            cout << "Type message: " << typemsg << endl;
            // Get button and analog information
            if (mInNav.data.getSize() > 0)
            {
                cout << "Receiving VRPN events" << endl;
                if (typemsg==flowvr_vrpn_BUTTONCB) {

                    memcpy((void*)(&b),mInNav.data.readAccess(),mInNav.data.getSize());
                    
                    button[0] = b.button;
                    button[1] = b.state;
                    button_pressed = true;	
                    
                    cout << "Button "<< button[0] << " is on state " << button[1] << endl;           
                }
                else if (typemsg==flowvr_vrpn_ANALOGCB){
                    memcpy((void*)(&a),mInNav.data.readAccess(),mInNav.data.getSize());
                    
                    if(!calibrated){
                        old_gravity[0] = a.channel[1];
                        old_gravity[1] = a.channel[2];
                        old_gravity[2] = a.channel[3];
                        calibrated = true;
                    }
                    else{
                        old_gravity[0] = gravity[0];
                        old_gravity[1] = gravity[1];
                        old_gravity[2] = gravity[2];
                    }
                    
                    gravity[0] = a.channel[1];
                    gravity[1] = a.channel[2];
                    gravity[2] = a.channel[3];
                    analog_used = true;
                    
                    cout << "Gravity vector: " << gravity[0] << " " << gravity[1] << " " << gravity[2] << endl;
                }
                else{
                    continue;
                }
                
                if(strcmp(device,"wiimote") == 0){
                    cout << "WiiMote used" << endl;
                    Wiimote_button(transformation_mat);
                    Wiimote_analog(transformation_mat);
                }
                else if(device == "mouse"){
                }

//                float* nav = (float*)mInNav.data.readAccess();
//                std::copy(nav, nav+16, navigation_info);
//                int taille = sizeof(navigation_info);
//                cout << "Navigation information: " << nav[0] << " " << nav[1] << endl;
//                xyz_move[0] = nav[0];
//                xyz_move[1] = nav[1];
//                xyz_move[2] = nav[2];
//                quat[0] = nav[3];
//                quat[1] = nav[4];
//                quat[2] = nav[5];
//                quat[3] = nav[6];
                //user_id = nav[7];
                
//                cout << "ABS = x: " << xyz_move[0] << " y: " << xyz_move[1] << " z: " << xyz_move[2] << endl;
//                cout << "QUAT = X: " << quat[0] << " Y: " << quat[1] << " Z: " << quat[2] << " W: " << quat[3] << endl;

            }

        }
        else 
        {
            cout << "Automatic generation of navigation events" << endl;
            if(c == 0)
            {
                xyz_move[0]= 0.0;
                xyz_move[1]= 0.0;
                xyz_move[2]= 0.0;

                quat[0] = 0.0;
                quat[1] = 0.0;
                quat[2] = 0.0;
                quat[3] = 1.0;
            }
            else
            {
                xyz_move[0] = 0.1;
            }
            c+=1;
            
            getModelViewMat(transformation_mat); // Need xyz_move and quat --> transformation_mat
            
        }
        
        printf("%s\n", "Transformation matrix");
        for (int i = 0; i < 16; i++) 
        {
            printf("%f ", transformation_mat[i]);
        }
        printf("\n");

        render_info = false;
        
        if(pInRenduMat->isConnected())
        {
            cout << "Port for modelview from rendering connected..." << endl;
            FlowVRModule->get(pInRenduMat, mInrenduMat);
            // Get modelview matrix from running rendering context
            if (mInrenduMat.data.getSize() > 0)
            {
                float* rendu_mat = (float*)mInrenduMat.data.readAccess();
                //std::copy(rendu_mat, rendu_mat+16, model_mat);
                
                if(rendu_mat[0] != 0.0 || rendu_mat[3] != 0.0 || rendu_mat[5] != 0.0 || rendu_mat[7] != 0.0 || rendu_mat[10] != 0.0 || rendu_mat[11] != 0.0)
                {
                    printf("%s\n", "ModelMat from rendering");
                    for (int i = 0; i < 16; i++) 
                    {
                        printf("%f ", rendu_mat[i]);
                    }
                    printf("\n");
                    
                    TransposeMat(rendu_mat, model_mat);
                    
                    printf("%s\n", "TransposeMat from rendering");
                    for (int i = 0; i < 16; i++) 
                    {
                        printf("%f ", model_mat[i]);
                    }
                    printf("\n");

                    render_info = true;

                }
            }
        }

        if(render_info)
        {
            MultMatrix(model_mat, transformation_mat, modelview_mat);
            TransposeMat(modelview_mat, tmp_mat);
            std::copy(tmp_mat, tmp_mat+16, modelview_mat);
            
            printf("%s\n", "Modelview matrix");
            for (int i = 0; i < 16; i++) 
            {
                printf("%f ", modelview_mat[i]);
            }
            printf("\n");

            int taille = sizeof(modelview_mat);
            cout << "Taille message envoye (modelview_mat) : " << taille << endl;
            mOutModelMat.data = FlowVRModule->alloc(taille);
            memcpy(mOutModelMat.data.writeAccess(),(void*)(modelview_mat),taille);

            FlowVRModule->put(&pOutModelMat,mOutModelMat);
            
            render_info = false;
        }
            
    }
    cout << "end of while loop" << endl;
}


int main(int argc, char ** argv)
{
    cout << "Entered in ComputeNavigation..." << endl;
    // Good ExaViz configuration checking
    envpath = getenv("FLOWVR_PREFIX");
    if(envpath == NULL)
    {
        std::cout << "FLOWVR_PREFIX has not been set, please add flowvr-suite-config.sh to your bashrc and bash_profile" << std::endl;
        return 1;
    }
    
    pInNav->setNonBlockingFlag(true);
    
    pInNav->stamps->add(TypeMsgStamp);

    // Open in and out ports
    std::vector <flowvr::Port*> ports;
    ports.push_back(pInRenduMat);
    if(strcmp(argv[1],"NO") == 0)
    {
        cout << "Open port for VRPN events" << endl;
        ports.push_back(pInNav);
        device = argv[2];
    }
    ports.push_back(&pOutModelMat);
    
    // FlowVR initialization
    if (!(FlowVRModule = initModule(ports)))
        return -1;
    
    getNavInfo();

    return 0;
}



