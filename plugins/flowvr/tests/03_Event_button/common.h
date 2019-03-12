#ifndef FLOWVR_SEND_RECV_MSG
#define FLOWVR_SEND_RECV_MSG

#include <memory>
#include <unordered_map>
#include <vector>

#define ITER 7
#define KEYS_SIZE 13

namespace {

std::unordered_map<std::string, int> new_keys() {
	static int divider = 0;
	divider++;
	
	std::vector<std::string> keys_names {"up",
										"down",
										"left",
										"right",
										"f1",
										"f4",
										"f8",
										"f12",
										"page_up",
										"page_down",
										"page_home",
										"end",
										"insert"};
	
	std::unordered_map<std::string, int> names_value;
	for (int i = 0; i < KEYS_SIZE; i++) {
		if (i % divider == 0) {
			names_value.emplace(keys_names[i], 1);
		} else {
			names_value.emplace(keys_names[i], 0);
		}
	}

	return names_value;
}

}

#endif