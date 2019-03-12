#ifndef FLOWVR_SEND_RECV_MSG
#define FLOWVR_SEND_RECV_MSG

#include <memory>

#define ITER 10
#define ARRAY_SIZE 10

namespace {

int* new_int_array() {
	static int multi = 0;
	multi++;

	int* array = new int[ARRAY_SIZE];
	for (int i = 0; i < ARRAY_SIZE; i++) {
		array[i] = i * multi;
	}
	return array;
}

char* new_char_array() {
	static char multi = 0;
	multi++;

	char* array = new char[ARRAY_SIZE];
	for (int i = 0; i < ARRAY_SIZE; i++) {
		array[i] = i * (-multi);
	}
	return array;
}

float* new_float_array() {
	static float multi = 0;
	multi += 1.23;

	float* array = new float[ARRAY_SIZE];
	for (int i = 0; i < ARRAY_SIZE; i++) {
		array[i] = i * multi;
	}
	return array;
}

}

#endif