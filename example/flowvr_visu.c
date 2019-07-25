/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * * Neither the name of CEA nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <paraconf.h>

#include <GL/glut.h>

#include "pdi.h"

int scale = 3;
int width = 0;
int height = 0;
double* main_field;

void display()
{
	double min_value = 0.0;
	double max_value = 1000000.0;
	double mid_value = (max_value + min_value) / 2.0;
	glPointSize(1.0f * scale);
	glClear(GL_COLOR_BUFFER_BIT);
	glBegin(GL_POINTS);
	int i = 0;
	for (i = 0; i < height; i++) {
		int j = 0;
		for (j = 0; j < width; j++) {
			double value = main_field[i * width + j];
			float r = (float) (value / mid_value - 1.0);
			float b = (float) (1.0 - value / mid_value);
			if (r < 0.0f) r = 0.0f;
			if (b < 0.0f) b = 0.0f;
			float g = 1.0f - b - r;

			glColor3f(r, g, b);
			glVertex2f(j, i);
		}
	}
	glEnd();
	glFlush();
}

void wait()
{
	int wait;
	PDI_expose("wait", &wait, PDI_IN);
	if (!wait) {
		free(main_field);
		PDI_finalize();
		exit(0);
	}
}

void idle_func()
{
	wait();
	PDI_expose("main_field", main_field, PDI_IN);
	glutPostRedisplay();
}

void glutStart()
{
	glutInitDisplayMode(GLUT_SINGLE | GLUT_RGB);
	glutInitWindowPosition(50, 50);
	glutInitWindowSize(width * scale, height * scale);
	glutCreateWindow("PDI example");

	glClear(GL_COLOR_BUFFER_BIT);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluOrtho2D( 0.0, (GLdouble)width, (GLdouble)height ,0.0 );

	glutDisplayFunc(display);
	glutIdleFunc (idle_func);	
}

void first_wait()
{
	wait();
	int global_size[2];
	PDI_expose("global_size", global_size, PDI_IN);
	width = global_size[0];
	height = global_size[1];

	main_field = malloc(sizeof(double) * height * width);
	PDI_expose("main_field", main_field, PDI_IN);
}

void print_pixel(double value) {
	char character;
	if (value < 200000.0) {
		printf("\033[0;34mO\033[0m");
		character = 'O';
	} else if (value < 400000.0) {
		printf("\033[0;36m#\033[0m");
		character = '#';
	} else if (value < 600000.0) {
		printf("\033[0;32mX\033[0m");
		character = 'X';
	} else if (value < 800000.0) {
		printf("\033[0;33m$\033[0m");
		character = '$';
	} else {
		printf("\033[0;31m@\033[0m");
		character = '@';
	}
}

void console_visu() {
	int counter = 0;
	while (1) {
		if (counter % 500 == 0) {
			int i = 0;
			for (i = 0; i < height; i++) {
				int j = 0;
				for (j = 0; j < width; j++) {
					print_pixel(main_field[i * width + j]);
				}
				printf("\n");
			}
			printf("\n");
		}
		wait();
		PDI_expose("main_field", main_field, PDI_IN);
		counter++;
	}
}

int main(int argc, char* argv[])
{
	PC_tree_t conf = PC_parse_path(argv[1]);
	PDI_init(conf);
	first_wait();

	int openGL_flag = 0;
	if (argc == 3) {
		openGL_flag = atoi(argv[2]);
	}

	if (openGL_flag) {
		glutInit(&argc, argv);
		glutStart();
		glutMainLoop();	
	} else {
		console_visu();
	}

	return 0;
}
