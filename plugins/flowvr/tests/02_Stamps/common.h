#ifndef FLOWVR_STAMPS_COMMON_H
#define FLOWVR_STAMPS_COMMON_H

#include <memory>

#define ITER 10
#define CHAR_ARRAY_SIZE 16
#define INT_ARRAY_SIZE 32
#define FLOAT_ARRAY_SIZE 32

namespace {

int* new_int_stamp() {
	static int value = 0;
	return new int {value++};
}

float* new_float_stamp() {
	static float value = 0;
	return new float {value+=0.123};
}

char* new_string_stamp() {
	static int offset = 0;
	offset++;

	char* array = new char[CHAR_ARRAY_SIZE];
	for (int i = 0; i < CHAR_ARRAY_SIZE; i++) {
		array[i] = 'a' + offset;
	}
	array[CHAR_ARRAY_SIZE - 1] = '\0';
	return array;
}

int* new_int_array_stamp() {
	static int multi = 0.123f;
	multi++;

	int* array = new int[INT_ARRAY_SIZE];
	for (int i = 0; i < INT_ARRAY_SIZE; i++) {
		array[i] = (int)i * multi;
	}
	return array;
}

float* new_float_array_stamp() {
	static float multi = 0.123f;
	multi++;

	float* array = new float[FLOAT_ARRAY_SIZE];
	for (int i = 0; i < FLOAT_ARRAY_SIZE; i++) {
		array[i] = (float)i * multi;
	}
	return array;
}

}

#endif