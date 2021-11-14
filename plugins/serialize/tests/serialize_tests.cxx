/*******************************************************************************
 * Copyright (C) 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <pdi.h>
#include <stdlib.h> 
#include <gtest/gtest.h>

#include <stddef.h>
#include <stdio.h>

#define SUBREGIONARRAY_SIZE 64
#define GRID_SIZE 32
#define SUBVECTOR_DATA_SIZE 16
#define VECTOR_DATA_SIZE 8
#define VECTOR_SIZE 4

typedef struct {
	int ix, iy, iz;        /* Bottom-lower-left corner in index-space */
	int nx, ny, nz;        /* Size */
	int sx, sy, sz;        /* Striding factors */
	int rx, ry, rz;        /* Refinement over the background grid */
	int level;             /* Refinement level = rx + ry + rz */
	int process;           /* Process containing this subgrid */
} Subregion;

typedef struct {
	Subregion**   subregions;    /* Array of pointers to subregions */
	int           size;          /* Size of subgregion array */
} SubregionArray;

typedef struct {
	SubregionArray* subgrids;      /* Array of subgrids in this process */
	SubregionArray* all_subgrids;  /* Array of all subgrids in the grid */
	SubregionArray* neighbors;     /* Array of nearest neighbor subgrids */
	int           size;          /* Total number of grid points */
} Grid;

typedef struct {
	double*       data;          /* Pointer to subvector data */
	int           allocated;     /* Was this data allocated? */
	Subregion*      data_space;    /* Pointer to data space */
	int           data_size;     /* Number of elements in vector, includes ghost points */
} Subvector;

typedef struct _Vector {
	Subvector**   subvectors;    /* Array of pointers to subvectors */
	int           data_size;     /* Number of elements in vector. All subvectors. includes ghost points */
	Grid*         grid;          /* Grid that this vector is on */
	SubregionArray* data_space;    /* Description of Vector data */
	int           size;          /* Total number of coefficients */
} Vector;

// Serialized types:
typedef struct {
	Subregion   subregions[SUBREGIONARRAY_SIZE];    /* Array of pointers to subregions */
	int         size;          /* Size of subgregion array */
} SubregionArraySerialized;

typedef struct {
	SubregionArraySerialized subgrids[GRID_SIZE];
	SubregionArraySerialized all_subgrids[GRID_SIZE];
	SubregionArraySerialized neighbors[GRID_SIZE];
	int size;
} GridSerialized;

typedef struct {
	double                         data[SUBVECTOR_DATA_SIZE];          /* Pointer to subvector data */
	int                            allocated;     /* Was this data allocated? */
	Subregion                      data_space;    /* Pointer to data space */
	int                            data_size;     /* Number of elements in vector, includes ghost points */
} SubvectorSerialized;

typedef struct {
	SubvectorSerialized      subvectors[VECTOR_DATA_SIZE];    /* Array of pointers to subvectors */
	int                        data_size;     /* Number of elements in vector. All subvectors. includes ghost points */
	GridSerialized           grid;          /* Grid that this vector is on */
	SubregionArraySerialized data_space;    /* Description of Vector data */
	int                        size;          /* Total number of coefficients */
} VectorSerialized;


static void init_subregion(Subregion* subregion, int value)
{
    subregion->ix = value; subregion->iy = value; subregion->iz = value;
    subregion->nx = value; subregion->ny = value; subregion->nz = value;
    subregion->sx = value; subregion->sy = value; subregion->sz = value;
    subregion->rx = value; subregion->ry = value; subregion->rz = value;
    subregion->level = value; subregion->process = value;
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
    for (int i = 0; i < GRID_SIZE; i++) {
        init_subregion_array(&grid->subgrids[i], SUBREGIONARRAY_SIZE, 10);
        init_subregion_array(&grid->all_subgrids[i], SUBREGIONARRAY_SIZE, 100);
        init_subregion_array(&grid->neighbors[i], SUBREGIONARRAY_SIZE, 200);
    }
    grid->size = GRID_SIZE;
}

static void init_subvector(Subvector* subvector)
{
    for (int i = 0; i < SUBVECTOR_DATA_SIZE; i++) {
        subvector->data[i] = (double) i;
    }
    subvector->allocated = 1;
    init_subregion(subvector->data_space, 42);
    subvector->data_size = SUBVECTOR_DATA_SIZE;
}

static void init_vector(Vector* vector)
{
    for (int i = 0; i < VECTOR_DATA_SIZE; i++) {
        init_subvector(vector->subvectors[i]);
    }
    vector->data_size = VECTOR_DATA_SIZE;
    init_grid(vector->grid);
    init_subregion_array(vector->data_space, SUBREGIONARRAY_SIZE, 0);
    vector->size = VECTOR_SIZE;
}

// #################### SUBREGION ####################

void expect_eq_subregion(const Subregion* s1, const Subregion* s2)
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

	EXPECT_EQ(s1->ix, s2->ix);
	EXPECT_EQ(s1->iy, s2->iy);
	EXPECT_EQ(s1->iz, s2->iz);
	EXPECT_EQ(s1->nx, s2->nx);
	EXPECT_EQ(s1->ny, s2->ny);
	EXPECT_EQ(s1->nz, s2->nz);
	EXPECT_EQ(s1->sx, s2->sx);
	EXPECT_EQ(s1->sy, s2->sy);
	EXPECT_EQ(s1->sz, s2->sz);
	EXPECT_EQ(s1->rx, s2->rx);
	EXPECT_EQ(s1->ry, s2->ry);
	EXPECT_EQ(s1->rz, s2->rz);
	EXPECT_EQ(s1->level, s2->level);
	EXPECT_EQ(s1->process, s2->process);
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

void expect_eq_subregion_array(const SubregionArray* s1, const SubregionArray* s2)
{
	for (int i = 0; i < SUBREGIONARRAY_SIZE; i++) {
		expect_eq_subregion(s1->subregions[i], s2->subregions[i]);
	}
	EXPECT_EQ(s1->size, s2->size);
}

void expect_eq_subregion_array_serialized(const SubregionArray* subregion_referenced, const SubregionArraySerialized* subregion_serialized)
{
	for (int i = 0; i < SUBREGIONARRAY_SIZE; i++) {
		expect_eq_subregion(subregion_referenced->subregions[i], &subregion_serialized->subregions[i]);
	}
	EXPECT_EQ(subregion_referenced->size, subregion_serialized->size);
}

void print_subregion_array(const SubregionArray* subregion_array)
{
	printf("  Subregion size: %d\n", subregion_array->size);
	for (int i = 0; i < SUBREGIONARRAY_SIZE; i++) {
		printf("  Subregion[%d]: ", i);
		print_subregion(subregion_array->subregions[i]);
	}
}

void alloc_subregion_array(SubregionArray* subregion_array)
{
	subregion_array->subregions = (Subregion**) malloc(sizeof(Subregion*) * SUBREGIONARRAY_SIZE);
	for (int i = 0; i < SUBREGIONARRAY_SIZE; i++) {
		subregion_array->subregions[i] = (Subregion*) malloc(sizeof(Subregion));
	}
}

void free_subregion_array(SubregionArray* subregion_array)
{
	for (int i = 0; i < SUBREGIONARRAY_SIZE; i++) {
		free(subregion_array->subregions[i]);
	}
	free(subregion_array->subregions);
}

// #################### GRID ####################

void expect_eq_grid(const Grid* g1, const Grid* g2)
{
	for (int i = 0; i < GRID_SIZE; i++) {
		expect_eq_subregion_array(&g1->subgrids[i], &g2->subgrids[i]);
		expect_eq_subregion_array(&g1->all_subgrids[i], &g2->all_subgrids[i]);
		expect_eq_subregion_array(&g1->neighbors[i], &g2->neighbors[i]);
	}
	EXPECT_EQ(g1->size, g2->size);
}

void expect_eq_grid_serialized(const Grid* grid_referenced, const GridSerialized* grid_serialized)
{
	for (int i = 0; i < GRID_SIZE; i++) {
		expect_eq_subregion_array_serialized(&grid_referenced->subgrids[i], &grid_serialized->subgrids[i]);
		expect_eq_subregion_array_serialized(&grid_referenced->all_subgrids[i], &grid_serialized->all_subgrids[i]);
		expect_eq_subregion_array_serialized(&grid_referenced->neighbors[i], &grid_serialized->neighbors[i]);
	}
	EXPECT_EQ(grid_referenced->size, grid_serialized->size);
}

void print_grid(const Grid* grid)
{
	printf("Grid address: %p\n", grid);
	printf("Subgrinds size: %d\n", grid->size);
	for (int i = 0; i < GRID_SIZE; i++) {
		printf("Subgrinds[%d]: ", i);
		print_subregion_array(&grid->subgrids[i]);
	}
}

void alloc_grid(Grid* grid)
{
	grid->subgrids = (SubregionArray*) malloc(sizeof(SubregionArray) * GRID_SIZE);
	for (int i = 0; i < GRID_SIZE; i++) {
		alloc_subregion_array(&grid->subgrids[i]);
		// grid->subgrids[i].subregions = (Subregion**) malloc(sizeof(Subregion) * SUBREGIONARRAY_SIZE);
		// for (int j = 0; j < SUBREGIONARRAY_SIZE; j++) {
		// 	grid->subgrids[i].subregions[j] = (Subregion*) malloc(sizeof(Subregion));
		// }
	}
	grid->all_subgrids = (SubregionArray*) malloc(sizeof(SubregionArray) * GRID_SIZE);
	for (int i = 0; i < GRID_SIZE; i++) {
		alloc_subregion_array(&grid->all_subgrids[i]);
	}
	grid->neighbors = (SubregionArray*) malloc(sizeof(SubregionArray) * GRID_SIZE);
	for (int i = 0; i < GRID_SIZE; i++) {
		alloc_subregion_array(&grid->neighbors[i]);
	}
}

void free_grid(Grid* grid)
{
	for (int i = 0; i < GRID_SIZE; i++) {
		free_subregion_array(&grid->subgrids[i]);
	}
	free(grid->subgrids);
	
	for (int i = 0; i < GRID_SIZE; i++) {
		free_subregion_array(&grid->all_subgrids[i]);
	}
	free(grid->all_subgrids);
	
	for (int i = 0; i < GRID_SIZE; i++) {
		free_subregion_array(&grid->neighbors[i]);
	}
	free(grid->neighbors);
}

// #################### SUBVECTOR ####################

void expect_eq_subvector(const Subvector* s1, const Subvector* s2)
{
	for (int i = 0; i < SUBVECTOR_DATA_SIZE; i++) {
		EXPECT_EQ(s1->data[i], s2->data[i]);
	}
	EXPECT_EQ(s1->allocated, s2->allocated);
	expect_eq_subregion(s1->data_space,s2->data_space);
	EXPECT_EQ(s1->data_size, s2->data_size);
}

void expect_eq_subvector_serialized(const Subvector* subvector_referenced, const SubvectorSerialized* subvector_serialized)
{
	for (int i = 0; i < SUBVECTOR_DATA_SIZE; i++) {
		EXPECT_EQ(subvector_referenced->data[i], subvector_serialized->data[i]);
	}
	EXPECT_EQ(subvector_referenced->allocated, subvector_serialized->allocated);
	expect_eq_subregion(subvector_referenced->data_space, &subvector_serialized->data_space);
	EXPECT_EQ(subvector_referenced->data_size, subvector_serialized->data_size);
}

void print_subvector(const Subvector* s1)
{
	printf("Subvector address: %p\n", s1);
	printf("Subgrinds size: %d\n", s1->data_size);
	printf("Data:\n");
	for (int i = 0; i < SUBVECTOR_DATA_SIZE; i++) {
		printf("%f ", s1->data[i]);
	}
	printf("\nAllocated: %d\n", s1->allocated);
	printf("Data_space:\n");
	print_subregion(s1->data_space);
}

void alloc_subvector(Subvector* s1)
{
	s1->data = (double*) calloc(SUBVECTOR_DATA_SIZE, sizeof(double));
	s1->data_space = (Subregion*) malloc(sizeof(Subregion));
}

void free_subvector(Subvector* s1)
{
	free(s1->data);
	free(s1->data_space);
}

// #################### VECTOR ####################

void expect_eq_vector(const Vector* v1, const Vector* v2)
{
	EXPECT_EQ(v1->data_size, v2->data_size);
	EXPECT_EQ(v1->size, v2->size);
	expect_eq_grid(v1->grid, v2->grid);
	expect_eq_subregion_array(v1->data_space, v2->data_space);
}

void expect_eq_vector_serialized(const Vector* vector_referenced, const VectorSerialized* vector_serialized)
{
	EXPECT_EQ(vector_referenced->data_size, vector_serialized->data_size);
	EXPECT_EQ(vector_referenced->size, vector_serialized->size);
	expect_eq_grid_serialized(vector_referenced->grid, &vector_serialized->grid);
	expect_eq_subregion_array_serialized(vector_referenced->data_space, &vector_serialized->data_space);
}

void print_vector(const Vector* v1)
{
	printf("Vector address: %p\n", v1);
	printf("Subgrinds size: %d\n", v1->data_size);
	printf("subvectors:\n");
	for (int i = 0; i < VECTOR_DATA_SIZE; i++) {
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
	v1->subvectors = (Subvector**) malloc(sizeof(Subvector) * VECTOR_DATA_SIZE);
	for (int i = 0; i < VECTOR_DATA_SIZE; i++) {
		v1->subvectors[i] = (Subvector*) malloc(sizeof(Subvector));
		alloc_subvector(v1->subvectors[i]);
	}
	v1->grid = (Grid*) malloc(sizeof(Grid));
	alloc_grid(v1->grid);
	v1->data_space = (SubregionArray*) malloc(sizeof(SubregionArray));
	alloc_subregion_array(v1->data_space);
}

void free_vector(Vector* v1)
{
	for (int i = 0; i < VECTOR_DATA_SIZE; i++) {
		free_subvector(v1->subvectors[i]);
		free(v1->subvectors[i]);
		
	}
	free(v1->subvectors);
	
	free_grid(v1->grid);
	free(v1->grid);
	
	free_subregion_array(v1->data_space);
	free(v1->data_space);
}

/*
 * Name:                serialize_test.01
 *
 * Description:         scalar serialization
 */
TEST(serialize_test, 01) { 

	const char* CONFIG_YAML =
	"logging: trace                         \n"
	"data:                                  \n"
	"  int_scalar: int                      \n"
	"  double_scalar: double                \n"
	"plugins:                               \n"
	"  serialize:                           \n"
	"    int_scalar: int_serialized         \n"
	"    double_scalar: double_serialized   \n"
	;

	PDI_init(PC_parse_string(CONFIG_YAML));
	
	int int_scalar = 42;
	double double_scalar = 42.42;
	
	PDI_share("int_scalar", &int_scalar, PDI_OUT);
	PDI_share("double_scalar", &double_scalar, PDI_OUT);
	
	int* int_serialized;
	double* double_serialized;
	PDI_access("int_serialized", (void**)&int_serialized, PDI_IN);
	PDI_access("double_serialized", (void**)&double_serialized, PDI_IN);
	
	printf("%d ?= %d\n", int_scalar, *int_serialized);
	EXPECT_EQ(int_scalar, *int_serialized);
	
	printf("%f ?= %f\n", double_scalar, *double_serialized);
	EXPECT_EQ(double_scalar, *double_serialized);
	
	PDI_release("int_serialized");
	PDI_release("double_serialized");
	
	PDI_reclaim("int_scalar");
	PDI_reclaim("double_scalar");
	
	PDI_errhandler(PDI_NULL_HANDLER);
	PDI_status_t status = PDI_access("int_serialized", (void**)&int_serialized, PDI_IN);
	EXPECT_NE(status, PDI_OK) << "Serialized data was not released";
	status = PDI_access("double_serialized", (void**)&double_serialized, PDI_IN);
	EXPECT_NE(status, PDI_OK) << "Serialized data was not released";
	
	PDI_finalize();
}

/*
 * Name:                serialize_test.02
 *
 * Description:         array serialization
 */
TEST(serialize_test, 02) { 

	const char* CONFIG_YAML =
	"logging: trace                               \n"
	"data:                                        \n"
	"  sparse_int_array:                          \n"
	"    type: array                              \n"
	"    subtype: int                             \n"
	"    size: [8, 4]                             \n"
	"    start: [2, 1]                            \n"
	"    subsize: [4, 2]                          \n"
	"  sparse_double_array:                       \n"
	"    type: array                              \n"
	"    subtype: double                          \n"
	"    size: [8, 4]                             \n"
	"    start: [2, 1]                            \n"
	"    subsize: [4, 2]                          \n"
	"plugins:                                     \n"
	"  serialize:                                 \n"
	"    sparse_int_array: dense_int_array        \n"
	"    sparse_double_array: dense_double_array  \n"
	;

	PDI_init(PC_parse_string(CONFIG_YAML));
	
	int sparse_int_array[8][4];
	double sparse_double_array[8][4];
	
	for (int i = 0; i < 8; i++) {
		for (int j = 0; j < 4; j++) {
			sparse_int_array[i][j] = i*4 + j;
			sparse_double_array[i][j] = 1.23 + i*4 + j;
		}
	}
	
	PDI_share("sparse_int_array", sparse_int_array, PDI_OUT);
	PDI_share("sparse_double_array", sparse_double_array, PDI_OUT);
	
	int* dense_int_array;
	double* dense_double_array;
	PDI_access("dense_int_array", (void**)&dense_int_array, PDI_IN);
	PDI_access("dense_double_array", (void**)&dense_double_array, PDI_IN);
	
	for (int i = 0; i < 4; i++) {
		for (int j = 0; j < 2; j++) {
			printf("s_i_a[%d][%d] = %d ?= d_i_a[%d][%d] = %d\n", i+2, j+1, sparse_int_array[i+2][j+1], i, j, dense_int_array[i*2 + j]);
			EXPECT_EQ(sparse_int_array[i+2][j+1], dense_int_array[i*2 + j]);
			printf("s_d_a[%d][%d] = %f ?= d_d_a[%d][%d] = %f\n", i+2, j+1, sparse_double_array[i+2][j+1], i, j, dense_double_array[i*2 + j]);
			EXPECT_EQ(sparse_double_array[i+2][j+1], dense_double_array[i*2 + j]);
		}
	}
	
	PDI_release("dense_int_array");
	PDI_release("dense_double_array");
	
	PDI_reclaim("sparse_int_array");
	PDI_reclaim("sparse_double_array");
	
	PDI_finalize();
}

/*
 * Name:                serialize_test.03
 *
 * Description:         pointer serialization
 */
TEST(serialize_test, 03) { 

	const char* CONFIG_YAML =
	"logging: trace                                     \n"
	"data:                                              \n"
	"  pointer:                                         \n"
	"    type: pointer                                  \n"
	"    subtype: double                                \n"
	"  pointer_to_array:                                \n"
	"    type: pointer                                  \n"
	"    subtype:                                       \n"
	"      type: array                                  \n"
	"      subtype: int                                 \n"
	"      size: 8                                      \n"
	"  pointer_array:                                   \n"
	"    type: array                                    \n"
	"    subtype:                                       \n"
	"      type: pointer                                \n"
	"      subtype: int                                 \n"
	"    size: 8                                        \n"
	"plugins:                                           \n"
	"  serialize:                                       \n"
	"    pointer: pointer_serialized                    \n"
	"    pointer_to_array: pointer_to_array_serialized  \n"
	"    pointer_array: pointer_array_serialized        \n"
	;

	PDI_init(PC_parse_string(CONFIG_YAML));

	// allocate memory
	std::unique_ptr<double> pointer {new double};
	std::unique_ptr<int[]> array {new int[8]};
	int* array_ptr = array.get();
	int** pointer_to_array = &array_ptr;
	std::unique_ptr<int*[]> pointer_array {new int*[8]};
	printf("pointer_to_array: %p\n", pointer_to_array);
	for (int i = 0; i < 8; i++) {
		pointer_array[i] = new int;
		printf("pointer_to_array[%d]: %p\n", i, &array[i]);
	}
	
	// initialize data
	*pointer = 1.234;
	for (int i = 0; i < 8; i++) {
		array[i] = 42 + i;
	}
	for (int i = 0; i < 8; i++) {
		*(pointer_array[i]) = 42 + i;
	}
	
	// share
	PDI_share("pointer", &pointer, PDI_OUT);
	PDI_share("pointer_to_array", pointer_to_array, PDI_OUT);
	PDI_share("pointer_array", pointer_array.get(), PDI_OUT);
	
	double* pointer_serialized;
	int* pointer_to_array_serialized;
	int* pointer_array_serialized;
	PDI_access("pointer_serialized", (void**)&pointer_serialized, PDI_IN);
	PDI_access("pointer_to_array_serialized", (void**)&pointer_to_array_serialized, PDI_IN);
	PDI_access("pointer_array_serialized", (void**)&pointer_array_serialized, PDI_IN);
	
	printf("%f ?== %f\n", *pointer, *pointer_serialized);
	EXPECT_EQ(*pointer, *pointer_serialized);
	for (int i = 0; i < 8; i++) {
		printf("[%d] %d ?== %d\n", i, array[i], pointer_to_array_serialized[i]);
		EXPECT_EQ(array[i], pointer_to_array_serialized[i]);
	}
	for (int i = 0; i < 8; i++) {
		printf("[%d] %d ?== %d\n", i, *(pointer_array[i]), pointer_array_serialized[i]);
		EXPECT_EQ(*(pointer_array[i]), pointer_array_serialized[i]);
	}
	
	PDI_release("pointer_serialized");
	PDI_release("pointer_to_array_serialized");
	PDI_release("pointer_array_serialized");
	
	PDI_reclaim("pointer");
	PDI_reclaim("pointer_to_array");
	PDI_reclaim("pointer_array");
	
	for (int i = 0; i < 8; i++) {
		delete pointer_array[i];
	}
	
	PDI_finalize();
}

/*
 * Name:                serialize_test.04
 *
 * Description:         record serialization
 */
TEST(serialize_test, 04) { 

	const char* CONFIG_YAML =
	"logging: trace                   \n"
	"data:                            \n"
	"  record:                        \n"
	"    type: record                 \n"
	"    buffersize: 80               \n"
	"    members:                     \n"
	"      scalar_member:             \n"
	"        disp: 0                  \n"
	"        type: int                \n"
	"      array_member:              \n"
	"        disp: 8                  \n"
	"        type: array              \n"
	"        subtype: double          \n"
	"        size: 8                  \n"
	"        start: 2                 \n"
	"        subsize: 4               \n"
	"      pointer_member:            \n"
	"        disp: 72                 \n"
	"        type: pointer            \n"
	"        subtype: int             \n"
	"plugins:                         \n"
	"  serialize:                     \n"
	"    record: record_serialized    \n"
	;

	struct Record {
		int scalar_member;
		double array_member[8];
		int* pointer_member;
	} typedef Record;

	struct Record_serialized {
		int scalar_member;
		double array_member[4];
		int pointer_member;
	} typedef Record_serialized;

	PDI_init(PC_parse_string(CONFIG_YAML));
	
	// initialize data
	Record record;
	record.scalar_member = 42;
	for (int i = 0; i < 8; i++) {
		record.array_member[i] = 42.123 + i;
	}
	int pointed_scalar = 50;
	record.pointer_member = &pointed_scalar;
	
	// share
	PDI_share("record", &record, PDI_OUT);
	
	Record_serialized* record_serialized;
	PDI_access("record_serialized", (void**)&record_serialized, PDI_IN);
	
	printf("%d ?== %d\n", record.scalar_member, record_serialized->scalar_member);
	EXPECT_EQ(record.scalar_member, record_serialized->scalar_member);
	for (int i = 0; i < 4; i++) {
		printf("[%d] %f ?== %f\n", i, record.array_member[i + 2], record_serialized->array_member[i]);
		EXPECT_EQ(record.array_member[i + 2], record_serialized->array_member[i]);
	}
	printf("%d ?== %d\n", *record.pointer_member, record_serialized->pointer_member);
	EXPECT_EQ(*record.pointer_member, record_serialized->pointer_member);
	
	PDI_release("record_serialized");
	PDI_reclaim("record");
	
	PDI_finalize();
}

/*
 * Name:                serialize_test.05
 *
 * Description:         complex datatype serialization
 */
TEST(serialize_test, 05) { 

	const char* CONFIG_YAML =
	".types:                                     \n"
	"  - &Subregion                              \n"
	"    type: record                            \n"
	"    buffersize: 56                          \n"
	"    members:                                \n"
	"        ix:      {disp: 0, type: int}       \n"
	"        iy:      {disp: 4, type: int}       \n"
	"        iz:      {disp: 8, type: int}       \n"
	"        nx:      {disp: 12, type: int}      \n"
	"        ny:      {disp: 16, type: int}      \n"
	"        nz:      {disp: 20, type: int}      \n"
	"        sx:      {disp: 24, type: int}      \n"
	"        sy:      {disp: 28, type: int}      \n"
	"        sz:      {disp: 32, type: int}      \n"
	"        rx:      {disp: 36, type: int}      \n"
	"        ry:      {disp: 40, type: int}      \n"
	"        rz:      {disp: 44, type: int}      \n"
	"        level:   {disp: 48, type: int}      \n"
	"        process: {disp: 52, type: int}      \n"
	"  - &Subvector                              \n"
	"    type: record                            \n"
	"    buffersize: 32                          \n"
	"    members:                                \n"
	"        data:                               \n"
	"          disp: 0                           \n"
	"          type: pointer                     \n"
	"          subtype:                          \n"
	"              type: array                   \n"
	"              size: $subvector_data_size    \n"
	"              subtype: double               \n"
	"        allocated:                          \n"
	"          disp: 8                           \n"
	"          type: int                         \n"
	"        data_space:                         \n"
	"          disp: 16                          \n"
	"          type: pointer                     \n"
	"          subtype: *Subregion               \n"
	"        data_size:                          \n"
	"          disp: 24                          \n"
	"          type: int                         \n"
	"  - &SubregionArray                         \n"
	"    type: record                            \n"
	"    buffersize: 16                          \n"
	"    members:                                \n"
	"      subregions:                           \n"
	"        disp: 0                             \n"
	"        type: pointer                       \n"
	"        subtype:                            \n"
	"          type: array                       \n"
	"          size: $subregionarray_size        \n"
	"          subtype:                          \n"
	"            type: pointer                   \n"
	"            subtype: *Subregion             \n"
	"      size:                                 \n"
	"        disp: 8                             \n"
	"        type: int                           \n"
	"  - &Grid                                   \n"
	"    type: record                            \n"
	"    buffersize: 32                          \n"
	"    members:                                \n"
	"      subgrids:                             \n"
	"        disp: 0                             \n"
	"        type: pointer                       \n"
	"        subtype:                            \n"
	"            type: array                     \n"
	"            size: $grid_size                \n"
	"            subtype: *SubregionArray        \n"
	"      all_subgrids:                         \n"
	"          disp: 8                           \n"
	"          type: pointer                     \n"
	"          subtype:                          \n"
	"            type: array                     \n"
	"            size: $grid_size                \n"
	"            subtype: *SubregionArray        \n"
	"      neighbors:                            \n"
	"          disp: 16                          \n"
	"          type: pointer                     \n"
	"          subtype:                          \n"
	"            type: array                     \n"
	"            size: $grid_size                \n"
	"            subtype: *SubregionArray        \n"
	"      size:                                 \n"
	"        disp: 24                            \n"
	"        type: int                           \n"
	"                                            \n"
	"logging: trace                              \n"
	"metadata:                                   \n"
	"  subregionarray_size: int                  \n"
	"  grid_size: int                            \n"
	"  subvector_data_size: int                  \n"
	"  vector_data_size: int                     \n"
	"data:                                       \n"
	"  vector_data:                              \n"
	"    type: record                            \n"
	"    buffersize: 40                          \n"
	"    members:                                \n"
	"      subvectors:                           \n"
	"        disp: 0                             \n"
	"        type: pointer                       \n"
	"        subtype:                            \n"
	"          type: array                       \n"
	"          size: $vector_data_size           \n"
	"          subtype:                          \n"
	"            type: pointer                   \n"
	"            subtype: *Subvector             \n"
	"      data_size:                            \n"
	"        disp: 8                             \n"
	"        type: int                           \n"
	"      grid:                                 \n"
	"        disp: 16                            \n"
	"        type: pointer                       \n"
	"        subtype: *Grid                      \n"
	"      data_space:                           \n"
	"        disp: 24                            \n"
	"        type: pointer                       \n"
	"        subtype: *SubregionArray            \n"
	"      size:                                 \n"
	"        disp: 32                            \n"
	"        type: int                           \n"
	"plugins:                                    \n"
	"  serialize:                                \n"
	"    vector_data: vector_data_serialized     \n"
	;

	PDI_init(PC_parse_string(CONFIG_YAML));
	
	int subregionarray_size = SUBREGIONARRAY_SIZE;
	int grid_size = GRID_SIZE;
	int subvector_data_size = SUBVECTOR_DATA_SIZE;
	int vector_data_size = VECTOR_DATA_SIZE;
	
	PDI_expose("subregionarray_size", &subregionarray_size, PDI_OUT);
	PDI_expose("grid_size", &grid_size, PDI_OUT);
	PDI_expose("subvector_data_size", &subvector_data_size, PDI_OUT);
	PDI_expose("vector_data_size", &vector_data_size, PDI_OUT);
	
	Vector vector;
	alloc_vector(&vector);
	init_vector(&vector);
	
	print_vector(&vector);
	
	PDI_share("vector_data", &vector, PDI_OUT);
	
	VectorSerialized* vector_serialized;
	PDI_access("vector_data_serialized", (void**)&vector_serialized, PDI_IN);
	
	expect_eq_vector_serialized(&vector, vector_serialized);
	
	PDI_release("vector_data_serialized");
	
	PDI_reclaim("vector_data");
	
	free_vector(&vector);
	
	PDI_finalize();
}

/*
 * Name:                serialize_test.06
 *
 * Description:         tuple serialization
 */
TEST(serialize_test, 06) { 

	const char* CONFIG_YAML =
	"logging: trace                   \n"
	"data:                            \n"
	"  tuple:                         \n"
	"    type: tuple                  \n"
	"    elements:                    \n"
	"      - int                      \n"
	"      - type: array              \n"
	"        subtype: double          \n"
	"        size: 8                  \n"
	"        start: 2                 \n"
	"        subsize: 4               \n"
	"      - type: pointer            \n"
	"        subtype: int             \n"
	"plugins:                         \n"
	"  serialize:                     \n"
	"    tuple: tuple_serialized      \n"
	;

	struct Record {
		int scalar_member;
		double array_member[8];
		int* pointer_member;
	} typedef Record;

	struct Record_serialized {
		int scalar_member;
		double array_member[4];
		int pointer_member;
	} typedef Record_serialized;

	PDI_init(PC_parse_string(CONFIG_YAML));
	
	// initialize data
	Record record;
	record.scalar_member = 42;
	for (int i = 0; i < 8; i++) {
		record.array_member[i] = 42.123 + i;
	}
	int pointed_scalar = 50;
	record.pointer_member = &pointed_scalar;
	
	// share
	PDI_share("tuple", &record, PDI_OUT);
	
	Record_serialized* record_serialized;
	PDI_access("tuple_serialized", (void**)&record_serialized, PDI_IN);
	
	printf("%d ?== %d\n", record.scalar_member, record_serialized->scalar_member);
	EXPECT_EQ(record.scalar_member, record_serialized->scalar_member);
	for (int i = 0; i < 4; i++) {
		printf("[%d] %f ?== %f\n", i, record.array_member[i + 2], record_serialized->array_member[i]);
		EXPECT_EQ(record.array_member[i + 2], record_serialized->array_member[i]);
	}
	printf("%d ?== %d\n", *record.pointer_member, record_serialized->pointer_member);
	EXPECT_EQ(*record.pointer_member, record_serialized->pointer_member);
	
	PDI_release("tuple_serialized");
	PDI_reclaim("tuple");
	
	PDI_finalize();
}

/*
 * Name:                serialize_test.07
 *
 * Description:         complex tuple datatype serialization
 */
TEST(serialize_test, 07) { 

	const char* CONFIG_YAML =
	".types:                                     \n"
	"  - &Subregion                              \n"
	"    type: record                            \n"
	"    buffersize: 56                          \n"
	"    members:                                \n"
	"        ix:      {disp: 0, type: int}       \n"
	"        iy:      {disp: 4, type: int}       \n"
	"        iz:      {disp: 8, type: int}       \n"
	"        nx:      {disp: 12, type: int}      \n"
	"        ny:      {disp: 16, type: int}      \n"
	"        nz:      {disp: 20, type: int}      \n"
	"        sx:      {disp: 24, type: int}      \n"
	"        sy:      {disp: 28, type: int}      \n"
	"        sz:      {disp: 32, type: int}      \n"
	"        rx:      {disp: 36, type: int}      \n"
	"        ry:      {disp: 40, type: int}      \n"
	"        rz:      {disp: 44, type: int}      \n"
	"        level:   {disp: 48, type: int}      \n"
	"        process: {disp: 52, type: int}      \n"
	"  - &Subvector                              \n"
	"    type: record                            \n"
	"    buffersize: 32                          \n"
	"    members:                                \n"
	"        data:                               \n"
	"          disp: 0                           \n"
	"          type: pointer                     \n"
	"          subtype:                          \n"
	"              type: array                   \n"
	"              size: $subvector_data_size    \n"
	"              subtype: double               \n"
	"        allocated:                          \n"
	"          disp: 8                           \n"
	"          type: int                         \n"
	"        data_space:                         \n"
	"          disp: 16                          \n"
	"          type: pointer                     \n"
	"          subtype: *Subregion               \n"
	"        data_size:                          \n"
	"          disp: 24                          \n"
	"          type: int                         \n"
	"  - &SubregionArray                         \n"
	"    type: record                            \n"
	"    buffersize: 16                          \n"
	"    members:                                \n"
	"      subregions:                           \n"
	"        disp: 0                             \n"
	"        type: pointer                       \n"
	"        subtype:                            \n"
	"          type: array                       \n"
	"          size: $subregionarray_size        \n"
	"          subtype:                          \n"
	"            type: pointer                   \n"
	"            subtype: *Subregion             \n"
	"      size:                                 \n"
	"        disp: 8                             \n"
	"        type: int                           \n"
	"  - &Grid                                   \n"
	"    type: record                            \n"
	"    buffersize: 32                          \n"
	"    members:                                \n"
	"      subgrids:                             \n"
	"        disp: 0                             \n"
	"        type: pointer                       \n"
	"        subtype:                            \n"
	"            type: array                     \n"
	"            size: $grid_size                \n"
	"            subtype: *SubregionArray        \n"
	"      all_subgrids:                         \n"
	"          disp: 8                           \n"
	"          type: pointer                     \n"
	"          subtype:                          \n"
	"            type: array                     \n"
	"            size: $grid_size                \n"
	"            subtype: *SubregionArray        \n"
	"      neighbors:                            \n"
	"          disp: 16                          \n"
	"          type: pointer                     \n"
	"          subtype:                          \n"
	"            type: array                     \n"
	"            size: $grid_size                \n"
	"            subtype: *SubregionArray        \n"
	"      size:                                 \n"
	"        disp: 24                            \n"
	"        type: int                           \n"
	"                                            \n"
	"logging: trace                              \n"
	"metadata:                                   \n"
	"  subregionarray_size: int                  \n"
	"  grid_size: int                            \n"
	"  subvector_data_size: int                  \n"
	"  vector_data_size: int                     \n"
	"data:                                       \n"
	"  vector_data:                              \n"
	"    type: tuple                             \n"
	"    elements:                               \n"
	"      - type: pointer                       \n"
	"        subtype:                            \n"
	"          type: array                       \n"
	"          size: $vector_data_size           \n"
	"          subtype:                          \n"
	"            type: pointer                   \n"
	"            subtype: *Subvector             \n"
	"      - int                                 \n"
	"      - type: pointer                       \n"
	"        subtype: *Grid                      \n"
	"      - type: pointer                       \n"
	"        subtype: *SubregionArray            \n"
	"      - int                                 \n"
	"plugins:                                    \n"
	"  serialize:                                \n"
	"    vector_data: vector_data_serialized     \n"
	;

	PDI_init(PC_parse_string(CONFIG_YAML));
	
	int subregionarray_size = SUBREGIONARRAY_SIZE;
	int grid_size = GRID_SIZE;
	int subvector_data_size = SUBVECTOR_DATA_SIZE;
	int vector_data_size = VECTOR_DATA_SIZE;
	
	PDI_expose("subregionarray_size", &subregionarray_size, PDI_OUT);
	PDI_expose("grid_size", &grid_size, PDI_OUT);
	PDI_expose("subvector_data_size", &subvector_data_size, PDI_OUT);
	PDI_expose("vector_data_size", &vector_data_size, PDI_OUT);
	
	Vector vector;
	alloc_vector(&vector);
	init_vector(&vector);
	
	print_vector(&vector);
	
	PDI_share("vector_data", &vector, PDI_OUT);
	
	VectorSerialized* vector_serialized;
	PDI_access("vector_data_serialized", (void**)&vector_serialized, PDI_IN);
	
	expect_eq_vector_serialized(&vector, vector_serialized);
	
	PDI_release("vector_data_serialized");
	
	PDI_reclaim("vector_data");
	
	free_vector(&vector);
	
	PDI_finalize();
}
