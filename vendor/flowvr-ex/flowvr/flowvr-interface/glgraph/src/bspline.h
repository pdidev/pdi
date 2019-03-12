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
 * File: ./src/bspline.h                                           *
 *                                                                 *
 * Contacts: Antoine M�ler <antoine.meler@ensimag.imag.fr>         *
 *                                                                 *
 ******************************************************************/
/*********************************************************************

 Simple b-spline curve algorithm

 Copyright 1994 by Keith Vertanen (vertankd@cda.mrs.umn.edu)

 Released to the public domain (your mileage may vary)

 **********************************************************************/

#include <stdlib.h>

#ifndef BSPLINE
#define BSPLINE

struct point_spline {
	double x;
	double y;
	double z;
};

int set_graph(void);
void compute_intervals(int *u, int n, int t);
double blend(int k, int t, int *u, double v);
void compute_point(int *u, int n, int t, double v, point_spline *control,
		point_spline *output);
void bspline(int n, int t, point_spline *control, point_spline *output,
		int num_output);

#endif

