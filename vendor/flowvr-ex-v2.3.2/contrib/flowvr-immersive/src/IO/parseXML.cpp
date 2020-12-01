#include <stdio.h>
#include <iostream>
#include <math.h>
#include <string>
#include <sstream>

#include <flowvr/xml.h>
#include <parseXML.h>


using namespace flowvr;
using namespace std;


float pre_mat[16];
float post_mat_left[16];
float post_mat_right[16];
float temp_mat[16];
float rot_mat[16] = {1.0f,0.0f,0.0f,0.0f,
					0.0f,1.0f,0.0f,0.0f,
					0.0f,0.0f,1.0f,0.0f,
					0.0f,0.0f,0.0f,1.0f};
float trans_mat[16] = {1.0f,0.0f,0.0f,0.0f,
					0.0f,1.0f,0.0f,0.0f,
					0.0f,0.0f,1.0f,0.0f,
					0.0f,0.0f,0.0f,1.0f};
float identity_mat[16] = {1.0f,0.0f,0.0f,0.0f,
						0.0f,1.0f,0.0f,0.0f,
						0.0f,0.0f,1.0f,0.0f,
						0.0f,0.0f,0.0f,1.0f};

// Vector normalization
void norm(float res[3], float src[3])
{
    float norminv = 1.0/((float)sqrt(src[0]*src[0]+src[1]*src[1]+src[2]*src[2]));
    
    res[0] = src[0]*norminv;
    res[1] = src[1]*norminv;
    res[2] = src[2]*norminv;
}

void computeTranslationMat(float trans_vec[3], float * translation_mat)
{
    //float* trans_mat = (float*) malloc(sizeof(float)*16);
    translation_mat[0] = 1.0;
    translation_mat[1] = 0.0;
    translation_mat[2] = 0.0;
    translation_mat[3] = trans_vec[0];
    translation_mat[4] = 0.0;
    translation_mat[5] = 1.0;
    translation_mat[6] = 0.0;
    translation_mat[7] = trans_vec[1];
    translation_mat[8] = 0.0;
    translation_mat[9] = 0.0;
    translation_mat[10] = 1.0;
    translation_mat[11] = trans_vec[2];
    translation_mat[12] = 0.0;
    translation_mat[13] = 0.0;
    translation_mat[14] = 0.0;
    translation_mat[15] = 1.0;
    //return trans_mat;
}

// Get rotation matrix for a rotation of a certain angle around an arbitraty axis
void computeRotationMat(float rotation_axis[3], float angle, float* rotation_mat)
{
    //float* rotation_mat = (float*) malloc(sizeof(float)*16);
    rotation_mat[0] = cos(angle) + rotation_axis[0]*rotation_axis[0] * (1 - cos(angle));
    rotation_mat[1] = rotation_axis[0] * rotation_axis[1] * (1 - cos(angle)) - rotation_axis[2]*sin(angle);
    rotation_mat[2] = rotation_axis[1] * sin(angle) + rotation_axis[0] * rotation_axis[2] * (1 - cos(angle));
    rotation_mat[3] = 0.0;
    rotation_mat[4] = rotation_axis[2] * sin(angle) + rotation_axis[0] * rotation_axis[1] * (1 - cos(angle));
    rotation_mat[5] = cos(angle) + rotation_axis[1]*rotation_axis[1] * (1 - cos(angle));
    rotation_mat[6] = -rotation_axis[0] * sin(angle) + rotation_axis[1] * rotation_axis[2] * (1 - cos(angle));
    rotation_mat[7] = 0.0;
    rotation_mat[8] = -rotation_axis[1] * sin(angle) + rotation_axis[0] * rotation_axis[2] * (1 - cos(angle));
    rotation_mat[9] = rotation_axis[0] * sin(angle) + rotation_axis[1] * rotation_axis[2] * (1 - cos(angle));
    rotation_mat[10] = cos(angle) + rotation_axis[2]*rotation_axis[2] * (1 - cos(angle));
    rotation_mat[11] = 0.0;
    rotation_mat[12] = 0.0;
    rotation_mat[13] = 0.0;
    rotation_mat[14] = 0.0;
    rotation_mat[15] = 1.0;
    //return rotation_mat;
}

// Multiplication of two 4*4 matrices
void MultiplicationMatrix(float *matleft,float *matright, float *mat){

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


screen_info * ScreenParser::getScreenInfo() const
{
	return _screen;
}

int ScreenParser::getWidth()
{
	if(_screen->width)
	{
		return _screen->width;
	}
	else
	{
		cerr << "The width is not defined yet, please parse your parameter file before.";
	}
}

int ScreenParser::getHeight()
{
	if(_screen->height)
	{
		return _screen->height;
	}
	else
	{
		cerr << "The height is not defined yet, please parse your parameter file before.";
	}
}

// Parser for the screen specifications from the XML file
void ScreenParser::parseFile(std::string filename, int id_screen)
{
    cout << "Entered in parseFile..." << endl;

    flowvr::xml::DOMNode * rootnode;
    flowvr::xml::DOMNode * node1;
    flowvr::xml::DOMNode * node2;
    flowvr::xml::DOMNode * node3;
    flowvr::xml::DOMDocument * file;

    std::string name;
    std::string id1;
    std::string id2;

    cout << "Variables declared..." << endl;

    cout << "File name: " << filename << endl;

    file = new flowvr::xml::DOMDocument(filename);
    if (!file->LoadFile())
    {
        cerr << "Loading Error..." << endl;
        cerr << "error #" << file->ErrorId() << " : " << file->ErrorDesc() << endl;
        exit(1);
    }


    rootnode= file->FirstChildElement("device");
    name=rootnode->ToElement()->Attribute("name");

    // going to the input element
    node1= rootnode->FirstChildElement("screen");

    cout << "Id screen: " << id_screen << endl;

    // Declare storage variable
    _screen = new screen_info();

    _screen->id = id_screen;

    cout << "Id screen: " << _screen->id << endl;

    // Check the screen parameters we need according the screen id
    while(node1)
    {
    	cout << "Entered in while loop" << endl;
        if(atoi(node1->ToElement()->Attribute("id")) == id_screen)
        {
        	node2 = node1->FirstChildElement("definition");
        	_screen->width = atoi(node2->ToElement()->Attribute("width"));
        	_screen->height = atoi(node2->ToElement()->Attribute("height"));
            node2 = node1->FirstChildElement("bottomleft");
            _screen->bottomleft[0] = atof(node2->ToElement()->Attribute("x"));
            _screen->bottomleft[1] = atof(node2->ToElement()->Attribute("y"));
            _screen->bottomleft[2] = atof(node2->ToElement()->Attribute("z"));
            node2 = node1->FirstChildElement("bottomright");
            _screen->bottomright[0] = atof(node2->ToElement()->Attribute("x"));
            _screen->bottomright[1] = atof(node2->ToElement()->Attribute("y"));
            _screen->bottomright[2] = atof(node2->ToElement()->Attribute("z"));
            node2 = node1->FirstChildElement("topleft");
            _screen->topleft[0] = atof(node2->ToElement()->Attribute("x"));
            _screen->topleft[1] = atof(node2->ToElement()->Attribute("y"));
            _screen->topleft[2] = atof(node2->ToElement()->Attribute("z"));
            if(node2 = node1->FirstChildElement("window_pos"))
            {
            	_screen->window_pos[0] = atoi(node2->ToElement()->Attribute("x"));
            	_screen->window_pos[1] = atoi(node2->ToElement()->Attribute("y"));
            }
            
            break;
        }
        else
        {
            node1 = node1->NextSiblingElement("screen");
        }
    }
}

tracker_info * TrackerParser::getTrackerInfo() const
{
	return _tracker;
}

float * TrackerParser::getPreMat()
{
	if(_tracker->pre_mat)
	{
		return _tracker->pre_mat;
	}
	else
	{
		cerr<< "The matrix is not defined yet, please parse your parameter file before.";
	}
}

float * TrackerParser::getPostMatLeft()
{
    if(_tracker->post_mat_left)
    {
        return _tracker->post_mat_left;
    }
    else
    {
        cerr<< "The height is not defined yet, please parse your parameter file before.";
    }
}

float * TrackerParser::getPostMatRight()
{
	if(_tracker->post_mat_right)
	{
		return _tracker->post_mat_right;
	}
	else
	{
		cerr<< "The height is not defined yet, please parse your parameter file before.";
	}
}

// Parser for the tracker modifications from the XML file
void TrackerParser::parseFile(std::string filename)
{

	flowvr::xml::DOMNode * rootnode;
    flowvr::xml::DOMNode * node1;
    flowvr::xml::DOMNode * node2;
    flowvr::xml::DOMNode * node3;
    flowvr::xml::DOMDocument * file;


    file = new flowvr::xml::DOMDocument(filename);
    if (!file->LoadFile())
    {
        cerr << "Loading Error..." << endl;
        cerr << "error #" << file->ErrorId() << " : " << file->ErrorDesc() << endl;
        exit(1);
    }

    // Declare storage variable for tracker information
    _tracker = new tracker_info();

    rootnode= file->FirstChildElement("device");

    // Get the transformation matrix allowing to transform tracking system coordinates to world coordinates
    if(node1 = rootnode->FirstChildElement("tracker2cave_mat"))
    {
        node2 = node1->FirstChildElement("matrix");
        if(!(node2->ToElement()->Attribute("values")))
        {
            cout << "No matrix values, parsing rotation and translation data..." << endl;

            cout << "TRANSLATION DATA" << endl;
            node2= node1->FirstChildElement("translation");
            float translation[3];
            translation[0] = atof(node2->ToElement()->Attribute("x"));
            translation[1] = atof(node2->ToElement()->Attribute("y"));
            translation[2] = atof(node2->ToElement()->Attribute("z"));
            node2 = node1->FirstChildElement("rotation");

            while(node2)
            {
                cout << "ROTATION DATA" << endl;
                node3 = node2->FirstChildElement("axis");
                float rotation_axis[3];
                float angle;
                if(node3->ToElement()->Attribute("canonical"))
                {
                    std::string canonical_axis = node3->ToElement()->Attribute("canonical");
                    if(canonical_axis == "X")
                    {
                        cout << "X" << endl;
                        rotation_axis[0] = 1.0;
                        rotation_axis[1] = 0.0;
                        rotation_axis[2] = 0.0;
                    }
                    else if(canonical_axis == "Y")
                    {
                        cout << "Y" << endl;
                        rotation_axis[0] = 0.0;
                        rotation_axis[1] = 1.0;
                        rotation_axis[2] = 0.0;
                    }
                    else if(canonical_axis == "Z")
                    {
                        cout << "Z" << endl;
                        rotation_axis[0] = 0.0;
                        rotation_axis[1] = 0.0;
                        rotation_axis[2] = 1.0;
                    }
                }
                else
                {
                    rotation_axis[0] = atof(node3->ToElement()->Attribute("x"));
                    rotation_axis[1] = atof(node3->ToElement()->Attribute("y"));
                    rotation_axis[2] = atof(node3->ToElement()->Attribute("z"));
                }
                node3 = node2->FirstChildElement("angle");
                if(node3->ToElement()->Attribute("radian"))
                {
                    angle = atof(node3->ToElement()->Attribute("radian"));
                }
                else
                {
                    angle = atof(node3->ToElement()->Attribute("degree")) * M_PI / 180.0f;
                }
                printf("%f \n",angle);
                float rotation_axis_norm[3];
                norm(rotation_axis_norm, rotation_axis);
                printf("%f %f %f \n", rotation_axis_norm[0], rotation_axis_norm[1], rotation_axis_norm[2]);
                //float rot_mat[16];
                computeRotationMat(rotation_axis_norm, angle, temp_mat); // -> rot_mat

                #ifdef DEBUG_MESSAGE
                    for (int i = 0; i < 16; i++) 
                    {
                        printf("%f ", temp_mat[i]);
                    }
                    printf("\n");
                #endif

                MultiplicationMatrix(rot_mat, temp_mat, pre_mat);
                std::copy(pre_mat, pre_mat+16, rot_mat);
                node2 = node2->NextSiblingElement("rotation");
            }
            
            computeTranslationMat(translation, trans_mat); // -> trans_mat
            MultiplicationMatrix(rot_mat, trans_mat, pre_mat); // -> pre_mat
            //_tracker->pre_mat = new float[16];
	    //_tracker->pre_mat = pre_mat;
	    std::copy(pre_mat, pre_mat+16,_tracker->pre_mat );
        }
        else
        {
            std::string values = node2->ToElement()->Attribute("values");
            std::istringstream iss(values);
            std::string token;
            int i = 0;

            while(getline(iss, token, ','))
            {
                pre_mat[i] = atof(token.c_str());
                i+=1;
            }
	    std::copy(pre_mat, pre_mat+16, _tracker->pre_mat);
        }
    } 
    else
    {
    	std::copy(identity_mat, identity_mat+16, _tracker->pre_mat);;
    }   

    node1 = rootnode->FirstChildElement("post_mat");

    /////// RIGHT EYE /////////
    node2 = node1->FirstChildElement("right");
    
    std::string values = node2->ToElement()->Attribute("values");
    std::istringstream iss(values);
    std::string token;
    int i = 0;


    while(getline(iss, token, ','))
    {
        post_mat_right[i] = atof(token.c_str());
        i+=1;
//        cout << " " << post_mat[i] << endl;
    }

    std::copy(post_mat_right, post_mat_right+16, _tracker->post_mat_right);

    /////// LEFT EYE /////////
    node2 = node1->FirstChildElement("left");
    
    values = node2->ToElement()->Attribute("values");
    std::istringstream lss(values);
    i = 0;

    while(getline(lss, token, ','))
    {
        post_mat_left[i] = atof(token.c_str());
        i+=1;
//        cout << " " << post_mat[i] << endl;
    }

    std::copy(post_mat_left, post_mat_left+16, _tracker->post_mat_left);


//    #ifdef DEBUG_MESSAGE
        for (int i = 0; i < 16; i++) 
        {
            printf("%f ", _tracker->pre_mat[i]);
        }
        printf("\n");

        for (int i = 0; i < 16; i++) 
        {
            printf("%f ", _tracker->post_mat_right[i]);
        }
        printf("\n");
//    #endif
}
