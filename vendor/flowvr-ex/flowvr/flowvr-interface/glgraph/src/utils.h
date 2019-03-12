/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                    Graph Visualization Tool                     *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA                                                           *
*  ALL RIGHTS RESERVED.                                           *
*                                                                 *
* This source is covered by the GNU GPL, please refer to the      *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Thomas Arcila,                                               *
*    Antoine Meler                                                *
*    Clement Meniers,                                             *
*    Bruno Raffin,                                                *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: ./src/utils.h                                             *
*                                                                 *
* Contacts: Antoine Méler <antoine.meler@ensimag.imag.fr>         *
*                                                                 *
******************************************************************/
#ifndef UTILS_H
#define UTILS_H

#include <math.h>
#include <string>
#include <sstream>

#include "adltree.h"

#define FONT GLUT_STROKE_ROMAN
#define FONT_SIZE 0.09f

using std::string;
using std::istringstream;

struct vec3f {
	float pos[3];
};
typedef struct vec3f vec3f;

float string2float(string & str);
float getTextWidth(string* str, float size);
void renderStrokeLine(float x, float y, float z, char* line, float size);
void renderStrokeLineFit(float x, float y, float z, char* line, float width, float height, bool forCluster=false);
void renderStrokeString(float x, float y, float z, std::string str, float size);
void renderStrokeStringAdl(float x, float y, float z, std::string str, float size);

float mkcolor(float color, bool visible);

float abs1f(float f);

vec3f addVectorToPos(float x, float y, float z, vec3f vec, float multiply);


class ColorGenerator {
public:
	float **P;

	float distance(float* p1, float* p2) {
		float dx = p2[0] - p1[0];
		float dy = p2[1] - p1[1];
		float dz = p2[2] - p1[2];
		return sqrt(dx * dx + dy * dy + dz * dz);
	}
	void normalise(float* p) {
		p[0] /= sqrt(p[0] * p[0] + p[1] * p[1] + p[2] * p[2]);
		p[1] /= sqrt(p[0] * p[0] + p[1] * p[1] + p[2] * p[2]);
		p[2] /= sqrt(p[0] * p[0] + p[1] * p[1] + p[2] * p[2]);
	}

	ColorGenerator(int nb_colors) {
		srand(1);
		int ho = 0;
		P = (float**) malloc(sizeof(float*) * nb_colors);
		for (ho = 0; ho < nb_colors; ho++) {
			P[ho] = (float*) malloc(sizeof(float) * 3);
		}
		for (ho = 0; ho < nb_colors; ho++) {
			P[ho][0] = (rand() % 100) * 0.009;
			P[ho][1] = (rand() % 100) * 0.009;
			P[ho][2] = (rand() % 100) * 0.009;
			normalise(P[ho]);
		}
		for (int round = 0; round < 20000; round++) {
			float min_dist = 100000.0;
			int hm1 = 0;
			int hm2 = 0;
			for (int h1 = 0; h1 < nb_colors - 1; h1++) {
				for (int h2 = h1 + 1; h2 < nb_colors; h2++) {
					float d = distance(P[h1], P[h2]);
					if (d < min_dist) {
						min_dist = d;
						hm1 = h1;
						hm2 = h2;
					}
				}
			}

			for (int coul = 0; coul < 3; coul++) {
				P[hm1][coul] += 0.01f * (P[hm1][coul] - P[hm2][coul]);
				P[hm2][coul] += 0.01f * (P[hm2][coul] - P[hm1][coul]);
				if (P[hm1][coul] < 0.0f)
					P[hm1][coul] = 0.0f + (rand() % 500) / 10000.0f;
				if (P[hm2][coul] < 0.0f)
					P[hm2][coul] = 0.0f + (rand() % 500) / 10000.0f;
			}
			normalise(P[hm1]);
			normalise(P[hm2]);

		}

	}
	~ColorGenerator() {

	}
	float get(int n, int c) {
		return P[n][c];
	}
};

#endif


