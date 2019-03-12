#ifndef FLOWVR_SEND_RECV_MSG
#define FLOWVR_SEND_RECV_MSG

#include <memory>
#include <unordered_map>
#include <vector>

#define ITER 9

namespace {

float* new_pos() {
	static float pos[2] = {0.0};
	pos[0] += 1.23;
	pos[1] -= 2.34;

	float* new_pos = new float[2];
	new_pos[0] = pos[0];
	new_pos[1] = pos[1];

	return new_pos;
}

std::unordered_map<std::string, int> new_keys() {
	static int move = 0;
	move++;
	
	std::vector<std::string> keys_names {"left_button",
										"middle_button",
										"left_button"};
	
	std::unordered_map<std::string, int> names_value;
	names_value.emplace("left_button", move & 0x01);
	names_value.emplace("middle_button", move & 0x02);
	names_value.emplace("right_button", move & 0x04);

	return names_value;
}

}

#endif