#ifndef FLOWVR_SEND_RECV_MSG
#define FLOWVR_SEND_RECV_MSG

#include <memory>

#define ITER 10
#define ARRAY_SIZE 32

namespace {

int* new_array() {
	static int multi = 0;
	multi++;

	int* array = new int[ARRAY_SIZE];
	for (int i = 0; i < ARRAY_SIZE; i++) {
		array[i] = i * multi;
	}
	return array;
}

}

#endif