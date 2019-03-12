/* Module computing tracking position and quaternion information 
   from VRPN server and outputing eyes positions to the OpenGL
   instances (via 1toN filter in case of multi-screen)
*/


#include <stdio.h>
#include <math.h>
#include <iostream>
#include <string>
#include <sstream>
#include <unistd.h>

//FlowVR Imports
#include <flowvr/moduleapi.h>
#include <flowvr/xml.h>
#include <flowvr_vrpn_stamps.h>
#include <parseXML.h>
#include <vrpn_Tracker.h>
//#include <wrap/gui/trackball.h>

// To check ExaViz successful initialization
const char* envpath;
const float EPSILON=0.00000000001f; // In order to avoid bad precision around 0.0f

using namespace std;
using namespace flowvr;

StampInfo *TypeMsgStamp = new StampInfo("TypeMsg",TypeInt::create());
StampInfo *WhichEye = new StampInfo("WhichEye", TypeInt::create());
StampInfo *WhichUser = new StampInfo("WhichUser", TypeInt::create());
StampInfo stampId("IdTracking", TypeInt::create());

// FlowVR part
ModuleAPI * FlowVRModule = 0;
InputPort * pInPos = new InputPort("posquat"); // Absolute position and associated quaternion of the user sent by VRPN

OutputPort pOutEyesPos("eyes_pos");

Message mInPos;
MessageWrite mOutEyesPos;

TrackerParser * tracker_parser;
vrpn_TRACKERCB t;


float eyes_mat_merged[32] = {1.0f,0.0f,0.0f,0.0f,
                      0.0f,1.0f,0.0f,0.0f,
                      0.0f,0.0f,1.0f,0.0f,
                      0.0f,0.0f,0.0f,1.0f,
                      1.0f,0.0f,0.0f,0.0f,
                      0.0f,1.0f,0.0f,0.0f,
                      0.0f,0.0f,1.0f,0.0f,
                      0.0f,0.0f,0.0f,1.0f};

float eyes_mat[16] = {1.0f,0.0f,0.0f,0.0f,
                          0.0f,1.0f,0.0f,0.0f,
                          0.0f,0.0f,1.0f,0.0f,
                          0.0f,0.0f,0.0f,1.0f};

float eyes_mat_left[16] = {1.0f,0.0f,0.0f,0.0f,
                          0.0f,1.0f,0.0f,0.0f,
                          0.0f,0.0f,1.0f,0.0f,
                          0.0f,0.0f,0.0f,1.0f};

float eyes_mat_right[16] = {1.0f,0.0f,0.0f,0.0f,
                          0.0f,1.0f,0.0f,0.0f,
                          0.0f,0.0f,1.0f,0.0f,
                          0.0f,0.0f,0.0f,1.0f};

float tmp_mat[16];
float pre_mat_inverse[16];

float frustum_mat[16] = {1.0f,0.0f,0.0f,0.0f,
              0.0f,1.0f,0.0f,0.0f,
              0.0f,0.0f,1.0f,0.0f,
              0.0f,0.0f,0.0f,1.0f};

int user_id;
int c=0;
int iteration;
int typemsg;

float   eyes_position[3]; // User absolute position
float   quat[4]; // Quaternion from tracking information

float rotation_mat[16] = {1.0f,0.0f,0.0f,0.0f,
	          0.0f,1.0f,0.0f,0.0f,
	          0.0f,0.0f,1.0f,0.0f,
	          0.0f,0.0f,0.0f,1.0f};


void PrintMatrix(float mat[16])
{
    cout << "mat :" << endl;
    printf("%f\t%f\t%f\t%f", mat[0], mat[1], mat[2], mat[3]);
    printf("\n");
    printf("%f\t%f\t%f\t%f", mat[4], mat[5], mat[6], mat[7]);
    printf("\n");
    printf("%f\t%f\t%f\t%f", mat[8], mat[9], mat[10], mat[11]);
    printf("\n");
    printf("%f\t%f\t%f\t%f", mat[12], mat[13], mat[14], mat[15]);
    printf("\n");
}

void MultMatrix(float *matleft,float *matright, float *mat){

    //float* mat = (float*) malloc(sizeof(float)*16);

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

// Broadly used method to transform a quaternion into a rotation matrix
void Quat2RotationMatrix(float quat[4])
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

void InverseMat(float* m, float* invOut)
{

    float inv[16], det;
    int i;

    inv[0] = m[5]  * m[10] * m[15] - 
             m[5]  * m[11] * m[14] - 
             m[9]  * m[6]  * m[15] + 
             m[9]  * m[7]  * m[14] +
             m[13] * m[6]  * m[11] - 
             m[13] * m[7]  * m[10];

    inv[4] = -m[4]  * m[10] * m[15] + 
              m[4]  * m[11] * m[14] + 
              m[8]  * m[6]  * m[15] - 
              m[8]  * m[7]  * m[14] - 
              m[12] * m[6]  * m[11] + 
              m[12] * m[7]  * m[10];

    inv[8] = m[4]  * m[9] * m[15] - 
             m[4]  * m[11] * m[13] - 
             m[8]  * m[5] * m[15] + 
             m[8]  * m[7] * m[13] + 
             m[12] * m[5] * m[11] - 
             m[12] * m[7] * m[9];

    inv[12] = -m[4]  * m[9] * m[14] + 
               m[4]  * m[10] * m[13] +
               m[8]  * m[5] * m[14] - 
               m[8]  * m[6] * m[13] - 
               m[12] * m[5] * m[10] + 
               m[12] * m[6] * m[9];

    inv[1] = -m[1]  * m[10] * m[15] + 
              m[1]  * m[11] * m[14] + 
              m[9]  * m[2] * m[15] - 
              m[9]  * m[3] * m[14] - 
              m[13] * m[2] * m[11] + 
              m[13] * m[3] * m[10];

    inv[5] = m[0]  * m[10] * m[15] - 
             m[0]  * m[11] * m[14] - 
             m[8]  * m[2] * m[15] + 
             m[8]  * m[3] * m[14] + 
             m[12] * m[2] * m[11] - 
             m[12] * m[3] * m[10];

    inv[9] = -m[0]  * m[9] * m[15] + 
              m[0]  * m[11] * m[13] + 
              m[8]  * m[1] * m[15] - 
              m[8]  * m[3] * m[13] - 
              m[12] * m[1] * m[11] + 
              m[12] * m[3] * m[9];

    inv[13] = m[0]  * m[9] * m[14] - 
              m[0]  * m[10] * m[13] - 
              m[8]  * m[1] * m[14] + 
              m[8]  * m[2] * m[13] + 
              m[12] * m[1] * m[10] - 
              m[12] * m[2] * m[9];

    inv[2] = m[1]  * m[6] * m[15] - 
             m[1]  * m[7] * m[14] - 
             m[5]  * m[2] * m[15] + 
             m[5]  * m[3] * m[14] + 
             m[13] * m[2] * m[7] - 
             m[13] * m[3] * m[6];

    inv[6] = -m[0]  * m[6] * m[15] + 
              m[0]  * m[7] * m[14] + 
              m[4]  * m[2] * m[15] - 
              m[4]  * m[3] * m[14] - 
              m[12] * m[2] * m[7] + 
              m[12] * m[3] * m[6];

    inv[10] = m[0]  * m[5] * m[15] - 
              m[0]  * m[7] * m[13] - 
              m[4]  * m[1] * m[15] + 
              m[4]  * m[3] * m[13] + 
              m[12] * m[1] * m[7] - 
              m[12] * m[3] * m[5];

    inv[14] = -m[0]  * m[5] * m[14] + 
               m[0]  * m[6] * m[13] + 
               m[4]  * m[1] * m[14] - 
               m[4]  * m[2] * m[13] - 
               m[12] * m[1] * m[6] + 
               m[12] * m[2] * m[5];

    inv[3] = -m[1] * m[6] * m[11] + 
              m[1] * m[7] * m[10] + 
              m[5] * m[2] * m[11] - 
              m[5] * m[3] * m[10] - 
              m[9] * m[2] * m[7] + 
              m[9] * m[3] * m[6];

    inv[7] = m[0] * m[6] * m[11] - 
             m[0] * m[7] * m[10] - 
             m[4] * m[2] * m[11] + 
             m[4] * m[3] * m[10] + 
             m[8] * m[2] * m[7] - 
             m[8] * m[3] * m[6];

    inv[11] = -m[0] * m[5] * m[11] + 
               m[0] * m[7] * m[9] + 
               m[4] * m[1] * m[11] - 
               m[4] * m[3] * m[9] - 
               m[8] * m[1] * m[7] + 
               m[8] * m[3] * m[5];

    inv[15] = m[0] * m[5] * m[10] - 
              m[0] * m[6] * m[9] - 
              m[4] * m[1] * m[10] + 
              m[4] * m[2] * m[9] + 
              m[8] * m[1] * m[6] - 
              m[8] * m[2] * m[5];

    det = m[0] * inv[0] + m[1] * inv[4] + m[2] * inv[8] + m[3] * inv[12];

    if (det == 0)
        cout<< "DETERMINANT NULL" << endl;

    det = 1.0 / det;

    for (i = 0; i < 16; i++)
        invOut[i] = inv[i] * det;
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

/************************************************/

// Main function to wrap the transition between vrpn and OpenGL rendering module
void getEyesPos()
{

    cout << "Entered in Tracking2Matrix" << endl;
    while (FlowVRModule->wait())
    {
	   // Check modules connection
        cout << "Entered in FlowVR loop" << endl;
	    if (pInPos->isConnected())
        {
//            cout << "Button and Analog connected" << endl;
            FlowVRModule->get(pInPos, mInPos);
            // Get button and analog information
            if (mInPos.data.getSize() > 0)
            {
            
                mInPos.stamps.read((*pInPos).stamps->it,iteration);
                mInPos.stamps.read((*TypeMsgStamp),typemsg);
                
                if (typemsg==flowvr_vrpn_TRACKERCB) 
                {      
                    memcpy((void*)(&t),mInPos.data.readAccess(),mInPos.data.getSize());
                    
                    for(int i=0; i<3; i++)
                        eyes_position[i]=static_cast<float>(t.pos[i]);
                    
                    for(int i=0;i<4;i++)
                        quat[i]=static_cast<float>(t.quat[i]);
                    
                    user_id = t.sensor;
                }
                else{
                    continue;
                }
                
//                float* pos = (float*)mInPos.data.readAccess();
//                eyes_position[0] = pos[0];
//                eyes_position[1] = pos[1];
//                eyes_position[2] = pos[2];
//                quat[0] = pos[3];
//                quat[1] = pos[4];
//                quat[2] = pos[5];
//                quat[3] = pos[6];
//                user_id = pos[7];
                
                cout << "ABS = x: " << eyes_position[0] << " y: " << eyes_position[1] << " z: " << eyes_position[2] << endl;
                cout << "QUAT = X: " << quat[0] << " Y: " << quat[1] << " Z: " << quat[2] << " W: " << quat[3] << endl;

                if(fabs(eyes_position[0]) > EPSILON)
                {
                    // Set the eyes matrix to identity
                    SetIdentity(eyes_mat);
                    // Transform the body tracking position into a translation matrix for further calculations
                    FromVec2Mat(eyes_position, eyes_mat); // -> eyes_mat

                    // Transform body tracking position from tracker referential to CAVE referential
                    MultMatrix(tracker_parser->getTrackerInfo()->pre_mat, eyes_mat, tmp_mat);
                    std::copy(tmp_mat, tmp_mat+16, eyes_mat);

                    // Add the rotation information given by the quaternion
                    Quat2RotationMatrix(quat); // -> rotation_mat
                    MultMatrix(eyes_mat, rotation_mat, tmp_mat);
                    std::copy(tmp_mat, tmp_mat+16, eyes_mat);

                    // Transform the body position to the left eye position
                    MultMatrix(eyes_mat, tracker_parser->getTrackerInfo()->post_mat_left, tmp_mat);
                    std::copy(tmp_mat, tmp_mat+16, eyes_mat_left);

                    // Transform the body position to the right eye position
                    MultMatrix(eyes_mat, tracker_parser->getTrackerInfo()->post_mat_right, tmp_mat);
                    std::copy(tmp_mat, tmp_mat+16, eyes_mat_right);
                    
                    // To get proper stereo, multiplication by pre_mat inverse
                    InverseMat(tracker_parser->getTrackerInfo()->pre_mat, tmp_mat);
                    std::copy(tmp_mat, tmp_mat+16, pre_mat_inverse);
                    
                    MultMatrix(eyes_mat_left, pre_mat_inverse, tmp_mat);
                    std::copy(tmp_mat, tmp_mat+16, eyes_mat_left);
                    MultMatrix(eyes_mat_right, pre_mat_inverse, tmp_mat);
                    std::copy(tmp_mat, tmp_mat+16, eyes_mat_right);

                    std::copy(eyes_mat_left, eyes_mat_left+16, eyes_mat_merged);
                    std::copy(eyes_mat_right, eyes_mat_right+16, eyes_mat_merged+16);
                    
		    //      		    for (int i = 0; i < 16; i++) 
           		    //{
           		    //  printf("%f ", tracker_parser->getTrackerInfo()->post_mat_left[i]);
           		    //}
           		    //printf("\n");
           		    //for (int i = 0; i < 16; i++) 
           		    //{
           		    //  printf("%f ", tracker_parser->getTrackerInfo()->post_mat_right[i]);
           		    //}
           		    printf("Merged Matrix left \n");
           		    for (int i = 0; i < 16; i++) 
           		    {
           		      printf("%f ", eyes_mat_left[i]);
           		    }
           		    printf("\n");

           		    printf("Merged Matrix right \n");
           		    for (int i = 0; i < 16; i++) 
           		    {
           		      printf("%f ", eyes_mat_right[i]);
           		    }
           		    printf("\n");


                    int taille = sizeof(eyes_mat_merged);
                    cout << "Taille message envoye (eyes_mat_merged) : " << taille << endl;
                    mOutEyesPos.data = FlowVRModule->alloc(taille);
                    memcpy(mOutEyesPos.data.writeAccess(),(void*)(eyes_mat_merged),taille);

                    mOutEyesPos.stamps.write(*WhichUser, user_id);
                    cout << "Message sent" << endl;
                    FlowVRModule->put(&pOutEyesPos,mOutEyesPos);
                }
            }
        }
        else
        {
            if(c == 0)
            {
                eyes_position[0] = 0.0;
                eyes_position[1] = 0.0;
                eyes_position[2] = 30.0;
            }
            else
            {
                eyes_position[0] += 0.0;
                eyes_position[1] -= 0.0;
                eyes_position[2] += 0.0;
            }

            c += 1;

            //////////////// DESKTOP ///////////////////
            // if(c == 0)
            // {
            //     eyes_position[0] = -2.0;
            //     eyes_position[1] = 0.0;
            //     eyes_position[2] = 0.0;
            // }
            // else
            // {
            //     eyes_position[0] += 0.005;
            //     eyes_position[1] = 0.0;
            //     eyes_position[2] = 0.0;
            // }

            // c += 1;
            // eyes_position[0] = 1.95;
            // eyes_position[1] = -0.25;
            // eyes_position[2] = 1.44;

            // printf("Pre_mat: ");
            // for (int i = 0; i < 16; i++) 
            // {
            //     printf("%f ", tracker_parser->getTrackerInfo()->pre_mat[i]);
            // }
            // printf("\n");

            // printf("Post_mat: ");
            // for (int i = 0; i < 16; i++) 
            // {
            //     printf("%f ", tracker_parser->getTrackerInfo()->post_mat[i]);
            // }
            // printf("\n");

            // Set the eyes matrix to identity
            SetIdentity(eyes_mat);
            // Transform the body tracking position into a translation matrix for further calculations
            FromVec2Mat(eyes_position, eyes_mat); // -> eyes_mat

            // Transform body tracking position from tracker referential to CAVE referential
            MultMatrix(tracker_parser->getTrackerInfo()->pre_mat, eyes_mat, tmp_mat);
            std::copy(tmp_mat, tmp_mat+16, eyes_mat);

            // Transform the body position to the eye position
            MultMatrix(eyes_mat, tracker_parser->getTrackerInfo()->post_mat_right, tmp_mat);
            std::copy(tmp_mat, tmp_mat+16, eyes_mat);

            //#ifdef DEBUG_MESSAGE
                printf("eyes_mat: ");
                for (int i = 0; i < 16; i++) 
                {
                    printf("%f ", eyes_mat[i]);
                }
                printf("\n");
            //#endif

            std::copy(eyes_mat, eyes_mat+16, eyes_mat_merged);
            std::copy(eyes_mat, eyes_mat+16, eyes_mat_merged+16);

            int taille = sizeof(eyes_mat_merged);
            cout << "Taille message envoye (eyes_mat_merged) : " << taille << endl;
            mOutEyesPos.data = FlowVRModule->alloc(taille);
            memcpy(mOutEyesPos.data.writeAccess(),(void*)(eyes_mat_merged),taille);

            mOutEyesPos.stamps.write(*WhichUser, user_id);
            
            FlowVRModule->put(&pOutEyesPos,mOutEyesPos);
            usleep(33000);
        }
	usleep(100000);
    }
    FlowVRModule->close();
}

float* normalize(float vec[3])
{
    float magnitude = sqrt(vec[0]*vec[0] + vec[1]*vec[1] + vec[2]*vec[2]);
    float* norm = (float*) malloc(sizeof(float)*3);
    norm[0] = vec[0] / magnitude;
    norm[1] = vec[1] / magnitude;
    norm[2] = vec[2] / magnitude;
    return norm;
}


// Parser for the XML tracking system parameters file
void parseParams(std::string filename)
{
    cout << "Entered in parseParams..." << endl;
 
    tracker_parser = new TrackerParser();
    tracker_parser->parseFile(filename);
}
    
int main(int argc, char ** argv)
{
    cout << "ComputeTracking entrance..." << endl;
	// Good ExaViz configuration checking
	envpath = getenv("FLOWVR_PREFIX");
    if(envpath == NULL)
    {
        std::cout << "FLOWVR_PREFIX has not been set, please add flowvr-suite-config.sh to your bashrc and bash_profile" << std::endl;
        return 1;
    }
    
    // Open in and out ports
    std::vector <flowvr::Port*> ports;
    ports.push_back(pInPos);
    ports.push_back(&pOutEyesPos);
    
    pOutEyesPos.stamps->add(WhichUser);
    pOutEyesPos.stamps->add(WhichEye);
    pInPos->stamps->add(TypeMsgStamp);

    cout << "FlowVR ports opened..." << endl;

    // FlowVR initialization
    if (!(FlowVRModule = initModule(ports)))
        return -1;


    // Get the parameters from XML (body2eye matrix + tracker2cave matrix)
    parseParams((std::string) argv[1]);

    // Transform the eyes position/quaternin got from VRPN into real coordinates position
    getEyesPos();
    
    return 0;
    
}
