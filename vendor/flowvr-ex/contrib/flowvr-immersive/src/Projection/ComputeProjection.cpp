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
#include <parseXML.h>
//#include <wrap/gui/trackball.h>

// To check ExaViz successful initialization
const char* envpath;
const float EPSILON=0.00000000001f; // In order to avoid bad precision around 0.0f

using namespace std;
using namespace flowvr;

StampInfo *TypeMsg = new StampInfo("TypeMsg",TypeInt::create());

StampInfo *WhichEyeOut = new StampInfo("WhichEye", TypeInt::create());
StampInfo *WhichUserOut = new StampInfo("WhichUser", TypeInt::create());
StampInfo *WhichScreenOut = new StampInfo("WhichScreen", TypeInt::create());
StampInfo *Id = new StampInfo("Id", TypeInt::create());
//StampInfo stampId("IdTracking", TypeInt::create());

// FlowVR part
ModuleAPI * FlowVRModule = 0;
InputPort * pInPos = new InputPort("eyes_pos"); // Eyes positions in CAVE referential coordinates
OutputPort pOutProjMat("projection_mat");
// For active double stereo
OutputPort pOutProjMatLeft("projection_mat_left");
OutputPort pOutProjMatRight("projection_mat_right"); 

Message mInPos;
MessageWrite mOutProjMat;
// For active double stereo
MessageWrite mOutProjMatLeft;
MessageWrite mOutProjMatRight;

ScreenParser * screen_parser;


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

float frustum_mat[16] = {1.0f,0.0f,0.0f,0.0f,
              0.0f,1.0f,0.0f,0.0f,
              0.0f,0.0f,1.0f,0.0f,
              0.0f,0.0f,0.0f,1.0f};
float projection_mat[16];
float projection_mat_left[16];
float projection_mat_right[16];
float projection_mat_merged[32];
float projection_mat_transpose[16];

// For active double stereo
float projection_mat_merged_left[32];
float projection_mat_merged_right[32];

char* eye;
int screen_id;
int user_id;
int c=0;
int Iteration;
bool double_stereo = false;

float   eyes_position[3]; // User absolute position

float translation_mat[16] = {1.0f,0.0f,0.0f,0.0f,
	          0.0f,1.0f,0.0f,0.0f,
	          0.0f,0.0f,1.0f,0.0f,
	          0.0f,0.0f,0.0f,1.0f};
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

/***************************************************************
Temporary matrix utilities for ComputeProjectionMatrix function
***************************************************************/

void normalize(float res[3], float src[3])
{
    float norminv = 1.0/((float)sqrt(src[0]*src[0]+src[1]*src[1]+src[2]*src[2]));
    
    res[0] = src[0]*norminv;
    res[1] = src[1]*norminv;
    res[2] = src[2]*norminv;
}


void substract( float res[3],float a[3],float b[3])
{
    res[0]=a[0]-b[0]; 
    res[1]=a[1]-b[1]; 
    res[2]=a[2]-b[2]; 
}


float dotproduct( float a[3],float b[3])
{
    return a[0]*b[0]+a[1]*b[1]+a[2]*b[2]; 
}

void crossproduct( float res[3],float a[3],float b[3])
{
    res[0]=a[1]*b[2]-a[2]*b[1]; 
    res[1]=a[2]*b[0]-a[0]*b[2]; 
    res[2]=a[0]*b[1]-a[1]*b[0]; 
}

/************************************************/

void ComputeFrustum(float l, float r, float b, float t, float n, float f)
{
    float aa = 2*n/(r-l);
    float bb = 2*n/(t-b);
    float A = (r+l)/(r-l);
    float B = (t+b)/(t-b);
    float C = -((f+n)/(f-n));
    float D = -((2*f*n)/(f-n));
    float tmp[16] = {aa,  0.0f,   A,      0.0f,
                  0.0f, bb,     B,      0.0f,
                  0.0f, 0.0f,   C,      D,
                  0.0f, 0.0f,   -1.0f,  0.0f};
    std::copy(tmp, tmp+16, frustum_mat);
}

float* ComputeProjectionMatrix(float eyes[16], float result[16]){

    // Beginning of CAVE projection given eye and screen positions
    float va[3],vb[3],vc[3];
    float vr[3],vu[3],vn[3];
    
    float l,r,b,t,d; 
    float M[16],V[18];
    
    // Screen corners positions
    float pa[3]={screen_parser->getScreenInfo()->bottomleft[0], screen_parser->getScreenInfo()->bottomleft[1], screen_parser->getScreenInfo()->bottomleft[2]};
    float pb[3]={screen_parser->getScreenInfo()->bottomright[0], screen_parser->getScreenInfo()->bottomright[1], screen_parser->getScreenInfo()->bottomright[2]};
    float pc[3]={screen_parser->getScreenInfo()->topleft[0], screen_parser->getScreenInfo()->topleft[1], screen_parser->getScreenInfo()->topleft[2]};
    // User eyes positions
    float pe[3]={eyes[3], eyes[7], eyes[11]};
    printf("Eyes position: %f %f %f \n", pe[0], pe[1], pe[2]);

    // Near and Far planes
    float n=0.1;
    float f=100.0;

    #ifdef DEBUG_MESSAGE
        printf("pa : %f %f %f\n",pa[0],pa[1],pa[2]);
        printf("pb : %f %f %f\n",pb[0],pb[1],pb[2]);
        printf("pc : %f %f %f\n",pc[0],pc[1],pc[2]);
        printf("pe : %f %f %f\n",pe[0],pe[1],pe[2]);
    #endif
    
    #ifdef DEBUG_MESSAGE
        printf("********************************************************************\n");

        printf("server %d\n",server);
        printf("eyes %d\n",eye);        

        printf("pe : %f %f %f\n",pe[0],pe[1],pe[2]);
    #endif

    substract(vr,pb,pa);
    substract(vu,pc,pa);

    normalize(vr,vr);
    normalize(vu,vu);   
    crossproduct(vn,vr,vu);
    normalize(vn,vn);   
    #ifdef DEBUG_MESSAGE
        printf("vr : %f %f %f\n",vr[0],vr[1],vr[2]);
        printf("vu : %f %f %f\n",vu[0],vu[1],vu[2]);
        printf("vn : %f %f %f\n",vn[0],vn[1],vn[2]);
    #endif
    

    
    substract(va,pa,pe);
    substract(vb,pb,pe);
    substract(vc,pc,pe);

    #ifdef DEBUG_MESSAGE
        printf("va : %f %f %f\n",va[0],va[1],va[2]);
        printf("vb : %f %f %f\n",vb[0],vb[1],vb[2]);
        printf("vc : %f %f %f\n",vc[0],vc[1],vc[2]);
    #endif
    
    d = - dotproduct(vn,va);

    printf("dist :%f\n",d);

    
    l = dotproduct(vr,va)*n/d;
    r = dotproduct(vr,vb)*n/d;
    b = dotproduct(vu,va)*n/d;
    t = dotproduct(vu,vc)*n/d;
    
    #ifdef DEBUG_MESSAGE
        printf("frustum : %f %f %f %f %f %f\n",l,r,b,t,n,f);
    #endif
    
    // glMatrixMode(GL_PROJECTION);
    // //    printf("MatrixMode PROJECTION 3 \n");
    // glLoadIdentity();
    
    ComputeFrustum(l,r,b,t,n,f); // -> frustum_mat
    //  fprintf(stderr, "Frustum OK \n");
    // glGetFloatv( GL_PROJECTION_MATRIX, M );
    
    #ifdef DEBUG_MESSAGE
        printf("frustum matrix:\n");
        int i;
        for (i = 0; i < 16; i++) 
            {
            printf("%f ", frustum_mat[i]);
            if ((i+1) % 4 == 0)
                printf("\n");
            }
        printf("\n");
    #endif

    memset(M,0,16*sizeof(float));

    M[0]=vr[0];M[4]=vu[0];M[8]=vn[0];
    M[1]=vr[1];M[5]=vu[1];M[9]=vn[1];
    M[2]=vr[2];M[6]=vu[2];M[10]=vn[2];

    M[15]=1.0f;
    
    #ifdef DEBUG_MESSAGE
        printf("rotated frustum matrix:\n");

        for (int i = 0; i < 16; i++) 
            {
            printf("%f ", M[i]);
            if ((i+1) % 4 == 0)
                printf("\n");
            }
        printf("\n");
    #endif
    // Multiplication of the frustum matrix by the transformation mat (P*M^t)
    MultMatrix(frustum_mat, M, projection_mat);
    //std::copy(tmp_mat, tmp_mat+16, projection_mat);

    // Multiplication by the eye position (P*M^t*T)
    float eye_trans_mat[16] = {1.0f,0.0f,0.0f,-pe[0],
                              0.0f,1.0f,0.0f,-pe[1],
                              0.0f,0.0f,1.0f,-pe[2],
                              0.0f,0.0f,0.0f,1.0f};
    MultMatrix(projection_mat, eye_trans_mat, tmp_mat);
    // std::copy(tmp_mat, tmp_mat+16, projection_mat);
    // glTranslatef(-pe[0],-pe[1],-pe[2]);
    std::copy(tmp_mat, tmp_mat+16, result);
}


// Main function to wrap the transition between vrpn and OpenGL rendering module
void computeProjection()
{

    cout << "Entered in computeProjection" << endl;
    while (FlowVRModule->wait())
    {
	   // Check modules connection
        cout << "Entered in FlowVR loop" << endl;
        if (pInPos->isConnected())
        {
            cout << "Eyes_pos port connected" << endl;
            FlowVRModule->get(pInPos, mInPos);
            // Get button and analog information
            if (mInPos.data.getSize() > 0)
            {
                mInPos.stamps.read((*pInPos).stamps->it,Iteration);
                cout << "Eyes position received for iteration " << Iteration << endl;
                float* pos = (float*)mInPos.data.readAccess();
                std::copy(pos, pos+16, eyes_mat_left);
                std::copy(pos+16, pos+32, eyes_mat_right);

                cout << "POS = x: " << eyes_mat_left[3] << " y: " << eyes_mat_left[7] << " z: " << eyes_mat_left[11] << endl;

                int id_user;
                StampInfo *WhichUserIn = (*(pInPos->stamps))[std::string("WhichUser")];
                mInPos.stamps.read((*WhichUserIn), id_user);

                int id_eye;
                StampInfo *WhichEyeIn = (*(pInPos->stamps))[std::string("WhichEye")];
                mInPos.stamps.read((*WhichEyeIn), id_eye);

                cout << "ID USER: " << id_user << endl;

                // if(fabs(eyes_mat[0]) > EPSILON)
                // {
                // Calculate the projection matrix with respect to the eye position and screen corners coordinates
                ComputeProjectionMatrix(eyes_mat_left, projection_mat_left);
                ComputeProjectionMatrix(eyes_mat_right, projection_mat_right);

                if(double_stereo)
                {
                    cout << "CAVE MODE - DOUBLE STEREO" << endl;
                    if(id_user == 1)
                    {
                        TransposeMat(projection_mat_left, projection_mat_transpose);
                        std::copy(projection_mat_transpose, projection_mat_transpose+16, projection_mat_merged_right);
                        TransposeMat(projection_mat_right, projection_mat_transpose);
                        std::copy(projection_mat_transpose, projection_mat_transpose+16, projection_mat_merged_left);
                    }
                    else if (id_user == 2)
                    {
                        TransposeMat(projection_mat_left, projection_mat_transpose);
                        std::copy(projection_mat_transpose, projection_mat_transpose+16, projection_mat_merged_right+16);
                        TransposeMat(projection_mat_right, projection_mat_transpose);
                        std::copy(projection_mat_transpose, projection_mat_transpose+16, projection_mat_merged_left+16);
                    }
                    printf("%s\n", "Left projectionMat");
                    for (int i = 0; i < 16; i++) 
                    {
                        printf("%f ", projection_mat_merged_left[i]);
                    }
                    printf("\n");

                    printf("%s\n", "Right projectionMat");
                    for (int i = 0; i < 16; i++) 
                    {
                        printf("%f ", projection_mat_merged_right[i]);
                    }
                    printf("\n");

                    int taille = sizeof(projection_mat_merged_left);
                    cout << "Taille message envoye (projection_mat_left) : " << taille << endl;
                    mOutProjMatLeft.data = FlowVRModule->alloc(taille);
                    memcpy(mOutProjMatLeft.data.writeAccess(),(void*)(projection_mat_merged_left),taille);

                    taille = sizeof(projection_mat_merged_right);
                    cout << "Taille message envoye (projection_mat_right) : " << taille << endl;
                    mOutProjMatRight.data = FlowVRModule->alloc(taille);
                    memcpy(mOutProjMatRight.data.writeAccess(),(void*)(projection_mat_merged_right),taille);

                    FlowVRModule->put(&pOutProjMatLeft,mOutProjMatLeft);
                    FlowVRModule->put(&pOutProjMatRight,mOutProjMatRight);
                }
                else
                {
                    TransposeMat(projection_mat_left, projection_mat_transpose);
                    std::copy(projection_mat_transpose, projection_mat_transpose+16, projection_mat_merged);
                    TransposeMat(projection_mat_right, projection_mat_transpose);
                    std::copy(projection_mat_transpose, projection_mat_transpose+16, projection_mat_merged+16); 

                    printf("%s\n", "TransposeMat");
                    for (int i = 0; i < 32; i++) 
                    {
                        printf("%f ", projection_mat_merged[i]);
                        if (i == 15)
                            printf("\n");
                    }
                    printf("\n");
                    

                    int taille = sizeof(projection_mat_merged);
                    cout << "Taille message envoye (projection_mat) : " << taille << endl;
                    mOutProjMat.data = FlowVRModule->alloc(taille);
                    memcpy(mOutProjMat.data.writeAccess(),(void*)(projection_mat_merged),taille);

                    mOutProjMat.stamps.write(*WhichUserOut,id_user);
                    mOutProjMat.stamps.write(*WhichScreenOut, screen_parser->getScreenInfo()->id);
                    mOutProjMat.stamps.write(*Id, c);
                    FlowVRModule->put(&pOutProjMat,mOutProjMat);
                }                
                // }
            }
        }
        else
        {
            if(double_stereo)
            {
                cout << "DESKTOP MODE - DOUBLE STEREO" << endl;
                eyes_position[0] = 10.0;
                eyes_position[1] = 10.0;
                eyes_position[2] = 60.0;

                FromVec2Mat(eyes_position, eyes_mat);

                // Calculate the projection matrix with respect to the eye position and screen corners coordinates
                ComputeProjectionMatrix(eyes_mat, projection_mat);

                // Print the projection matrix for debugging
                PrintMatrix(projection_mat);

                TransposeMat(projection_mat, projection_mat_transpose);

                std::copy(projection_mat_transpose, projection_mat_transpose+16, projection_mat_merged_left);
                std::copy(projection_mat_transpose, projection_mat_transpose+16, projection_mat_merged_left+16);

                eyes_position[0] = 0.0;
                eyes_position[1] = 0.0;
                eyes_position[2] = 60.0;

                FromVec2Mat(eyes_position, eyes_mat);

                // Calculate the projection matrix with respect to the eye position and screen corners coordinates
                ComputeProjectionMatrix(eyes_mat, projection_mat);

                // Print the projection matrix for debugging
                PrintMatrix(projection_mat);

                TransposeMat(projection_mat, projection_mat_transpose);

                std::copy(projection_mat_transpose, projection_mat_transpose+16, projection_mat_merged_right);
                std::copy(projection_mat_transpose, projection_mat_transpose+16, projection_mat_merged_right+16);

                int taille = sizeof(projection_mat_merged_left);
                cout << "Taille message envoye (projection_mat) : " << taille << endl;
                mOutProjMatLeft.data = FlowVRModule->alloc(taille);
                memcpy(mOutProjMatLeft.data.writeAccess(),(void*)(projection_mat_merged_left),taille);

                taille = sizeof(projection_mat_merged_right);
                cout << "Taille message envoye (projection_mat) : " << taille << endl;
                mOutProjMatRight.data = FlowVRModule->alloc(taille);
                memcpy(mOutProjMatRight.data.writeAccess(),(void*)(projection_mat_merged_right),taille);

                FlowVRModule->put(&pOutProjMatLeft,mOutProjMatLeft);
                FlowVRModule->put(&pOutProjMatRight,mOutProjMatRight);
            }
            else
            {
                cout << "DESKTOP MODE - MONO STEREO" << endl;
                if(c == 0)
                {
                    eyes_position[0] = 0.0;
                    eyes_position[1] = 0.0;
                    eyes_position[2] = 0.0;
                }
                else
                {
                    eyes_position[0] -= 0.001;
                    eyes_position[1] -= 0.0;
                    eyes_position[2] += 0.0;
                }

                c += 1;

                FromVec2Mat(eyes_position, eyes_mat);

                // Calculate the projection matrix with respect to the eye position and screen corners coordinates
                ComputeProjectionMatrix(eyes_mat, projection_mat);

                // Print the projection matrix for debugging
                PrintMatrix(projection_mat);

                // DEBUG //
                #ifdef DEBUG_MESSAGE
                    printf("%s\n", "Mat");
                    for (int i = 0; i < 16; i++) 
                    {
                        printf("%f ", projection_mat[i]);
                    }
                    printf("\n");
                #endif

                TransposeMat(projection_mat, projection_mat_transpose);

                // DEBUG //
                // #ifdef DEBUG_MESSAGE
                printf("%s\n", "TransposeMat");
                for (int i = 0; i < 16; i++) 
                {
                    printf("%f ", projection_mat_transpose[i]);
                }
                printf("\n");
            
                std::copy(projection_mat_transpose, projection_mat_transpose+16, projection_mat_merged);
                std::copy(projection_mat_transpose, projection_mat_transpose+16, projection_mat_merged+16);

                printf("%s\n", "MergedMat");
                for (int i = 0; i < 32; i++) 
                {
                    printf("%f ", projection_mat_merged[i]);
                }
                printf("\n");

                int taille = sizeof(projection_mat_merged);
                cout << "Taille message envoye (projection_mat_merged) : " << taille << endl;
                mOutProjMat.data = FlowVRModule->alloc(taille);
                memcpy(mOutProjMat.data.writeAccess(),(void*)(projection_mat_merged),taille);
                mOutProjMat.stamps.write(*Id, c);
                FlowVRModule->put(&pOutProjMat,mOutProjMat);
            }            

        }
        usleep(33000);
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

    screen_parser = new ScreenParser();
    screen_parser->parseFile(filename, screen_id);
}
    
int main(int argc, char ** argv)
{
    cout << "Entered in ComputeProjection..." << endl;
	// Good ExaViz configuration checking
	envpath = getenv("FLOWVR_PREFIX");
    if(envpath == NULL)
    {
        std::cout << "FLOWVR_PREFIX has not been set, please add flowvr-suite-config.sh to your bashrc and bash_profile" << std::endl;
        return 1;
    }

    // Get the parameters from arguments (which eye + screen id)
    screen_id = atoi(argv[1]);
    if ( atoi(argv[2]) == 1)
    {
        double_stereo = true;
    }
    
    cout << "Arguments: " << screen_id << " " << double_stereo << " " << (std::string) argv[3] << endl;

    //pInPos.stamps->add(&WhichEyeIn);
    //pInPos.stamps->add(&    WhichUserIn);

    pOutProjMat.stamps->add(WhichEyeOut);
    pOutProjMat.stamps->add(WhichUserOut);
    pOutProjMat.stamps->add(WhichScreenOut);
    pOutProjMat.stamps->add(Id);

    
    // Open in and out ports
    std::vector <flowvr::Port*> ports;
    ports.push_back(pInPos);
    if(double_stereo)
    {
        ports.push_back(&pOutProjMatLeft);
        ports.push_back(&pOutProjMatRight);
    }
    else
    {
        ports.push_back(&pOutProjMat);
    }
    
    
    cout << "FlowVR ports opened..." << endl;

    // FlowVR initialization
    if (!(FlowVRModule = initModule(ports)))
        return -1;

    // Get the parameters from XML (screen dimensions)
    parseParams((std::string) argv[3]);

    // Create the projection matrix sent to the rendering module with respect to the user position and system parameters
    computeProjection();
    
    return 0;
    
}
