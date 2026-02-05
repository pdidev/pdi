/*
 * SPDX-FileCopyrightText: 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef SERIALIZE_TESTS_SERIALIZE_TEST_H_
#define SERIALIZE_TESTS_SERIALIZE_TEST_H_

#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

/*
 * Structures from ParFlow (DO NOT CHANGE -> The same values are in CONFIG YAML)
 */
#define SubregionArray_size 64
#define Grid_size 32
#define Subvector_data_size 16
#define Vector_data_size 8
#define Vector_size 4

typedef struct {
	int ix, iy, iz; /* Bottom-lower-left corner in index-space */
	int nx, ny, nz; /* Size */
	int sx, sy, sz; /* Striding factors */
	int rx, ry, rz; /* Refinement over the background grid */
	int level; /* Refinement level = rx + ry + rz */
	int process; /* Process containing this subgrid */
} Subregion;

typedef struct {
	Subregion** subregions; /* Array of pointers to subregions */
	int size; /* Size of subgregion array */
} SubregionArray;

typedef struct {
	SubregionArray* subgrids; /* Array of subgrids in this process */
	SubregionArray* all_subgrids; /* Array of all subgrids in the grid */
	SubregionArray* neighbors; /* Array of nearest neighbor subgrids */
	int size; /* Total number of grid points */
} Grid;

typedef struct {
	double* data; /* Pointer to subvector data */
	int allocated; /* Was this data allocated? */
	Subregion* data_space; /* Pointer to data space */
	int data_size; /* Number of elements in vector, includes ghost points */
} Subvector;

typedef struct _Vector {
	Subvector** subvectors; /* Array of pointers to subvectors */
	int data_size; /* Number of elements in vector. All subvectors. includes ghost points */
	Grid* grid; /* Grid that this vector is on */
	SubregionArray* data_space; /* Description of Vector data */
	int size; /* Total number of coefficients */
} Vector;

// Serialized types:
typedef struct {
	Subregion subregions[SubregionArray_size]; /* Array of pointers to subregions */
	int size; /* Size of subgregion array */
} SubregionArraySerialized;

typedef struct {
	SubregionArraySerialized subgrids[Grid_size];
	SubregionArraySerialized all_subgrids[Grid_size];
	SubregionArraySerialized neighbors[Grid_size];
	int size;
} GridSerialized;

typedef struct {
	double data[Subvector_data_size]; /* Pointer to subvector data */
	int allocated; /* Was this data allocated? */
	Subregion data_space; /* Pointer to data space */
	int data_size; /* Number of elements in vector, includes ghost points */
} SubvectorSerialized;

typedef struct {
	SubvectorSerialized subvectors[Vector_data_size]; /* Array of pointers to subvectors */
	int data_size; /* Number of elements in vector. All subvectors. includes ghost points */
	GridSerialized grid; /* Grid that this vector is on */
	SubregionArraySerialized data_space; /* Description of Vector data */
	int size; /* Total number of coefficients */
} VectorSerialized;

static void init_subregion(Subregion* subregion, int value)
{
	subregion->ix = value;
	subregion->iy = value;
	subregion->iz = value;
	subregion->nx = value;
	subregion->ny = value;
	subregion->nz = value;
	subregion->sx = value;
	subregion->sy = value;
	subregion->sz = value;
	subregion->rx = value;
	subregion->ry = value;
	subregion->rz = value;
	subregion->level = value;
	subregion->process = value;
}

static void init_subregion_array(SubregionArray* subregion_array, int size, int value)
{
	for (int i = 0; i < size; i++) {
		init_subregion(subregion_array->subregions[i], i + value);
	}
	subregion_array->size = size;
}

static void init_grid(Grid* grid)
{
	for (int i = 0; i < Grid_size; i++) {
		init_subregion_array(&grid->subgrids[i], SubregionArray_size, 10);
		init_subregion_array(&grid->all_subgrids[i], SubregionArray_size, 100);
		init_subregion_array(&grid->neighbors[i], SubregionArray_size, 200);
	}
	grid->size = Grid_size;
}

static void init_subvector(Subvector* subvector)
{
	for (int i = 0; i < Subvector_data_size; i++) {
		subvector->data[i] = (double)i;
	}
	subvector->allocated = 1;
	init_subregion(subvector->data_space, 42);
	subvector->data_size = Subvector_data_size;
}

static void init_vector(Vector* vector)
{
	for (int i = 0; i < Vector_data_size; i++) {
		init_subvector(vector->subvectors[i]);
	}
	vector->data_size = Vector_data_size;
	init_grid(vector->grid);
	init_subregion_array(vector->data_space, SubregionArray_size, 0);
	vector->size = Vector_size;
}

// #################### SUBREGION ####################

void assert_eq_subregion(const Subregion* s1, const Subregion* s2)
{
	printf("i: %d ?== %d\n", s1->ix, s2->ix);
	printf("%d ?== %d\n", s1->iy, s2->iy);
	printf("%d ?== %d\n", s1->iz, s2->iz);
	printf("n: %d ?== %d\n", s1->nx, s2->nx);
	printf("%d ?== %d\n", s1->ny, s2->ny);
	printf("%d ?== %d\n", s1->nz, s2->nz);
	printf("s: %d ?== %d\n", s1->sx, s2->sx);
	printf("%d ?== %d\n", s1->sy, s2->sy);
	printf("%d ?== %d\n", s1->sz, s2->sz);
	printf("r: %d ?== %d\n", s1->rx, s2->rx);
	printf("%d ?== %d\n", s1->ry, s2->ry);
	printf("%d ?== %d\n", s1->rz, s2->rz);
	printf("l: %d ?== %d\n", s1->level, s2->level);
	printf("p: %d ?== %d\n", s1->process, s2->process);
	fflush(stdout);

	assert(s1->ix == s2->ix);
	assert(s1->iy == s2->iy);
	assert(s1->iz == s2->iz);
	assert(s1->nx == s2->nx);
	assert(s1->ny == s2->ny);
	assert(s1->nz == s2->nz);
	assert(s1->sx == s2->sx);
	assert(s1->sy == s2->sy);
	assert(s1->sz == s2->sz);
	assert(s1->rx == s2->rx);
	assert(s1->ry == s2->ry);
	assert(s1->rz == s2->rz);
	assert(s1->level == s2->level);
	assert(s1->process == s2->process);
}

void print_subregion(const Subregion* subregion)
{
	printf("    Subregion address: %p\n", subregion);
	printf("    ix: %d\n", subregion->ix);
	printf("    iy: %d\n", subregion->iy);
	printf("    iz: %d\n", subregion->iz);
	printf("    nx: %d\n", subregion->ix);
	printf("    ny: %d\n", subregion->iy);
	printf("    nz: %d\n", subregion->iz);
	printf("    sx: %d\n", subregion->ix);
	printf("    sy: %d\n", subregion->iy);
	printf("    sz: %d\n", subregion->iz);
	printf("    rx: %d\n", subregion->ix);
	printf("    ry: %d\n", subregion->iy);
	printf("    rz: %d\n", subregion->iz);
	printf("    level: %d\n", subregion->level);
	printf("    process: %d\n", subregion->process);
}

// #################### SUBREGION ARRAY ####################

void assert_eq_subregion_array(const SubregionArray* s1, const SubregionArray* s2)
{
	for (int i = 0; i < SubregionArray_size; i++) {
		assert_eq_subregion(s1->subregions[i], s2->subregions[i]);
	}
	assert(s1->size == s2->size);
}

void assert_eq_subregion_array_serialized(const SubregionArray* subregion_referenced, const SubregionArraySerialized* subregion_serialized)
{
	for (int i = 0; i < SubregionArray_size; i++) {
		assert_eq_subregion(subregion_referenced->subregions[i], &subregion_serialized->subregions[i]);
	}
	assert(subregion_referenced->size == subregion_serialized->size);
}

void print_subregion_array(const SubregionArray* subregion_array)
{
	printf("  Subregion size: %d\n", subregion_array->size);
	for (int i = 0; i < SubregionArray_size; i++) {
		printf("  Subregion[%d]: ", i);
		print_subregion(subregion_array->subregions[i]);
	}
}

void alloc_subregion_array(SubregionArray* subregion_array)
{
	subregion_array->subregions = (Subregion**)malloc(sizeof(Subregion*) * SubregionArray_size);
	for (int i = 0; i < SubregionArray_size; i++) {
		subregion_array->subregions[i] = (Subregion*)malloc(sizeof(Subregion));
	}
}

void free_subregion_array(SubregionArray* subregion_array)
{
	for (int i = 0; i < SubregionArray_size; i++) {
		free(subregion_array->subregions[i]);
	}
	free(subregion_array->subregions);
}

// #################### GRID ####################

void assert_eq_grid(const Grid* g1, const Grid* g2)
{
	for (int i = 0; i < Grid_size; i++) {
		assert_eq_subregion_array(&g1->subgrids[i], &g2->subgrids[i]);
		assert_eq_subregion_array(&g1->all_subgrids[i], &g2->all_subgrids[i]);
		assert_eq_subregion_array(&g1->neighbors[i], &g2->neighbors[i]);
	}
	assert(g1->size == g2->size);
}

void assert_eq_grid_serialized(const Grid* grid_referenced, const GridSerialized* grid_serialized)
{
	for (int i = 0; i < Grid_size; i++) {
		assert_eq_subregion_array_serialized(&grid_referenced->subgrids[i], &grid_serialized->subgrids[i]);
		assert_eq_subregion_array_serialized(&grid_referenced->all_subgrids[i], &grid_serialized->all_subgrids[i]);
		assert_eq_subregion_array_serialized(&grid_referenced->neighbors[i], &grid_serialized->neighbors[i]);
	}
	assert(grid_referenced->size == grid_serialized->size);
}

void print_grid(const Grid* grid)
{
	printf("Grid address: %p\n", grid);
	printf("Subgrinds size: %d\n", grid->size);
	for (int i = 0; i < Grid_size; i++) {
		printf("Subgrinds[%d]: ", i);
		print_subregion_array(&grid->subgrids[i]);
	}
}

void alloc_grid(Grid* grid)
{
	grid->subgrids = (SubregionArray*)malloc(sizeof(SubregionArray) * Grid_size);
	for (int i = 0; i < Grid_size; i++) {
		alloc_subregion_array(&grid->subgrids[i]);
		// grid->subgrids[i].subregions = (Subregion**) malloc(sizeof(Subregion) * SubregionArray_size);
		// for (int j = 0; j < SubregionArray_size; j++) {
		//  grid->subgrids[i].subregions[j] = (Subregion*) malloc(sizeof(Subregion));
		// }
	}
	grid->all_subgrids = (SubregionArray*)malloc(sizeof(SubregionArray) * Grid_size);
	for (int i = 0; i < Grid_size; i++) {
		alloc_subregion_array(&grid->all_subgrids[i]);
	}
	grid->neighbors = (SubregionArray*)malloc(sizeof(SubregionArray) * Grid_size);
	for (int i = 0; i < Grid_size; i++) {
		alloc_subregion_array(&grid->neighbors[i]);
	}
}

void free_grid(Grid* grid)
{
	for (int i = 0; i < Grid_size; i++) {
		free_subregion_array(&grid->subgrids[i]);
	}
	free(grid->subgrids);

	for (int i = 0; i < Grid_size; i++) {
		free_subregion_array(&grid->all_subgrids[i]);
	}
	free(grid->all_subgrids);

	for (int i = 0; i < Grid_size; i++) {
		free_subregion_array(&grid->neighbors[i]);
	}
	free(grid->neighbors);
}

// #################### SUBVECTOR ####################

void assert_eq_subvector(const Subvector* s1, const Subvector* s2)
{
	for (int i = 0; i < Subvector_data_size; i++) {
		assert(s1->data[i] == s2->data[i]);
	}
	assert(s1->allocated == s2->allocated);
	assert_eq_subregion(s1->data_space, s2->data_space);
	assert(s1->data_size == s2->data_size);
}

void assert_eq_subvector_serialized(const Subvector* subvector_referenced, const SubvectorSerialized* subvector_serialized)
{
	for (int i = 0; i < Subvector_data_size; i++) {
		assert(subvector_referenced->data[i] == subvector_serialized->data[i]);
	}
	assert(subvector_referenced->allocated == subvector_serialized->allocated);
	assert_eq_subregion(subvector_referenced->data_space, &subvector_serialized->data_space);
	assert(subvector_referenced->data_size == subvector_serialized->data_size);
}

void print_subvector(const Subvector* s1)
{
	printf("Subvector address: %p\n", s1);
	printf("Subgrinds size: %d\n", s1->data_size);
	printf("Data:\n");
	for (int i = 0; i < Subvector_data_size; i++) {
		printf("%f ", s1->data[i]);
	}
	printf("\nAllocated: %d\n", s1->allocated);
	printf("Data_space:\n");
	print_subregion(s1->data_space);
}

void alloc_subvector(Subvector* s1)
{
	s1->data = (double*)calloc(Subvector_data_size, sizeof(double));
	s1->data_space = (Subregion*)malloc(sizeof(Subregion));
}

void free_subvector(Subvector* s1)
{
	free(s1->data);
	free(s1->data_space);
}

// #################### VECTOR ####################

void assert_eq_vector(const Vector* v1, const Vector* v2)
{
	assert(v1->data_size == v2->data_size);
	assert(v1->size == v2->size);
	assert_eq_grid(v1->grid, v2->grid);
	assert_eq_subregion_array(v1->data_space, v2->data_space);
}

void assert_eq_vector_serialized(const Vector* vector_referenced, const VectorSerialized* vector_serialized)
{
	assert(vector_referenced->data_size == vector_serialized->data_size);
	assert(vector_referenced->size == vector_serialized->size);
	assert_eq_grid_serialized(vector_referenced->grid, &vector_serialized->grid);
	assert_eq_subregion_array_serialized(vector_referenced->data_space, &vector_serialized->data_space);
}

void print_vector(const Vector* v1)
{
	printf("Vector address: %p\n", v1);
	printf("Subgrinds size: %d\n", v1->data_size);
	printf("subvectors:\n");
	for (int i = 0; i < Vector_data_size; i++) {
		print_subvector(v1->subvectors[i]);
	}
	printf("\nData_size: %d\n", v1->data_size);
	printf("Grid:\n");
	print_grid(v1->grid);
	printf("Data_space:\n");
	print_subregion_array(v1->data_space);
	printf("Size: %d\n", v1->size);
}

void alloc_vector(Vector* v1)
{
	v1->subvectors = (Subvector**)malloc(sizeof(Subvector) * Vector_data_size);
	for (int i = 0; i < Vector_data_size; i++) {
		v1->subvectors[i] = (Subvector*)malloc(sizeof(Subvector));
		alloc_subvector(v1->subvectors[i]);
	}
	v1->grid = (Grid*)malloc(sizeof(Grid));
	alloc_grid(v1->grid);
	v1->data_space = (SubregionArray*)malloc(sizeof(SubregionArray));
	alloc_subregion_array(v1->data_space);
}

void free_vector(Vector* v1)
{
	for (int i = 0; i < Vector_data_size; i++) {
		free_subvector(v1->subvectors[i]);
		free(v1->subvectors[i]);
	}
	free(v1->subvectors);

	free_grid(v1->grid);
	free(v1->grid);

	free_subregion_array(v1->data_space);
	free(v1->data_space);
}

#endif // SERIALIZE_TESTS_SERIALIZE_TEST_H_
