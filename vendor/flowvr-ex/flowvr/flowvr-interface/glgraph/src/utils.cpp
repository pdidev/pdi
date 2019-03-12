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
 * File: ./src/utils.cpp                                           *
 *                                                                 *
 * Contacts: Antoine M�ler <antoine.meler@ensimag.imag.fr>         *
 *                                                                 *
 ******************************************************************/
#include <string>
#include <sstream>
# ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif

#include <cstdlib>

#include "utils.h"

float string2float(string & str) {
	for (unsigned int c = 0; c < str.length(); c++) {
		if (str[c] == ',')
			str[c] = '.';
	}
	istringstream iss(str);
	float result;
	iss >> result;
	return result;
}

#define FONT GLUT_STROKE_ROMAN
#define FONT_SIZE 0.09f

float getTextWidth(string* str, float size) {
	int width = 0;
	for (unsigned int c = 0; c < str->length(); c++) {
		width += glutStrokeWidth(FONT, (*str)[c]);
	}
	return (float) width * size;
}

void renderStrokeLine(float x, float y, float z, char* line, float size) {
	string string_line(line);
	glPushMatrix();
	glTranslatef(x - getTextWidth(&string_line, size) * 0.5f, y - (36.0f*size), z);
	//glRasterPos3f(x - getTextWidth(&string_line, size) * 0.5f, y - (36.0f*size), z);
	glScalef(size, size, 1.0f);
	for (unsigned int c = 0; line[c] != 0; c++) {
		glutStrokeCharacter(GLUT_STROKE_ROMAN, line[c]);
		//glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, line[c]);
	}
	glPopMatrix();
}

void renderStrokeLineFit(float x, float y, float z, char* line, float width, float height, bool forCluster) {
	string string_line = "";
	if (line != NULL) string_line += line;

	if (string_line != "") {

		float scalability;
		if (forCluster)
			scalability = 100;
		else
			scalability = 1;

		float textWidth;

		float size = 0.05f;
		bool fits = false;
		while (!fits && size < (0.18f*scalability)) {
			textWidth = getTextWidth(&string_line, size);
			if (textWidth > (width*0.9))
				fits = true;
			else
				size += 0.01f;
		}
		// Call renderStroke
		//renderStrokeLine(x, y, z, line, size);
		renderStrokeStringAdl(x, y, z, string_line, size);
	}
}

void renderStrokeString(float x, float y, float z, string str, float size) {

	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable( GL_BLEND);
	glEnable( GL_LINE_SMOOTH);
	glLineWidth(1.0);

	// Comptage du nobre de lignes

	float nb_lignes = 1;
	for (unsigned int c = 0; c < str.length() - 1; c++) {
		if (str[c] == '\\' && str[c + 1] == 'n')
			nb_lignes++;
	}

	// Affichage de chaque ligne

	float espacement_vertical = 15.0f;
	char* line = (char*) malloc(sizeof(char) * (str.length() + 1));
	unsigned int c2 = 0;
	float l = 0.0f;
	for (unsigned int c1 = 0; c1 < str.length(); c1++) {
		if (str[c1] != '\\') {
			line[c2++] = str[c1];
		} else {
			line[c2] = 0;
			renderStrokeLine(
					x,
					y - espacement_vertical * l + 0.5f * (nb_lignes - 1)
							* espacement_vertical, z, line, size);
			l++;
			c2 = 0;
			c1 += 1;
		}
	}
	line[c2] = 0;
	renderStrokeLine(
			x,
			y - espacement_vertical * l + 0.5f * (nb_lignes - 1)
					* espacement_vertical, z, line, size);
	l++;

	free(line);
}

void renderStrokeStringAdl(float x, float y, float z, string str, float size) {

	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable( GL_BLEND);
	glEnable( GL_LINE_SMOOTH);
	glLineWidth(1.0);

	glDisable(GL_DEPTH_TEST);

	// Comptage du nobre de lignes

	float nb_lignes = 1;
	for (unsigned int c = 0; c < str.length() - 1; c++) {
		if (str[c] == '\\' && str[c + 1] == 'n')
			nb_lignes++;
	}

	// Affichage de chaque ligne

	float espacement_vertical = 15.0f;
	char* line = (char*) malloc(sizeof(char) * (str.length() + 1));
	unsigned int c2 = 0;
	float l = 0.0f;
	for (unsigned int c1 = 0; c1 < str.length(); c1++) {
		if (str[c1] != '\\') {
			line[c2++] = str[c1];
		} else {
			line[c2] = 0;
			renderStrokeLine(
					x,
					y - espacement_vertical * l + 0.5f * (nb_lignes - 1)
							* espacement_vertical, z, line, size);
			l++;
			c2 = 0;
			c1 += 1;
		}
	}
	line[c2] = 0;
	renderStrokeLine(
			x,
			y - espacement_vertical * l + 0.5f * (nb_lignes - 1)
					* espacement_vertical, z, line, size);
	l++;

	free(line);

	glDisable( GL_BLEND);
	glDisable( GL_LINE_SMOOTH);

	glEnable(GL_DEPTH_TEST);
}

float abs1f(float f) {
	if (f < 0.0f)
		return -f;
	else
		return f;
}

vec3f addVectorToPos(float x, float y, float z, vec3f vec, float multiply) {
	x = x + vec.pos[0]*multiply;
	y = y + vec.pos[1]*multiply;
	z = z + vec.pos[2]*multiply;
	vec3f result;
	result.pos[0] = x;
	result.pos[1] = y;
	result.pos[2] = z;
	return result;
}
