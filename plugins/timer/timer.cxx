/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <chrono>
#include <fstream>
#include <optional>
#include <string>
#include <unordered_map>
#include <fcntl.h>
#include <sys/file.h>
#include <unistd.h>

#include <pdi/context.h>
#include <pdi/logger.h>
#include <pdi/plugin.h>

namespace {

using namespace PDI;

struct TimerInfo {
	double accumulated_time{0.0};
	std::optional<std::chrono::high_resolution_clock::time_point> start_time{std::nullopt};
};

/** The timer plugin 
*/
class timer_plugin: public PDI::Plugin
{
	// Map of timer's name and info including start time and accumulated time
	std::unordered_map<std::string, TimerInfo> timers;

public:
	timer_plugin(Context& ctx, PC_tree_t spec_tree)
		: Plugin{ctx}
	{
		if (PC_status(spec_tree)) {
			ctx.logger().error("Error in read_config_tree");
			return;
		}

		for (int i = 0; i < len(spec_tree, 0); i++) {
			PC_tree_t timer_item = PC_get(spec_tree, "[%d]", i);
			std::string timer_name = PDI::to_string(PC_get(timer_item, "{0}"));

			PC_tree_t val = PC_get(timer_item, ".%s", timer_name.c_str());
			if (is_map(val)) {
				ctx.logger().debug("Defined timer (map-styled): {}", timer_name);

				auto start_ev = PDI::to_string(PC_get(val, ".start"));
				ctx.callbacks().add_event_callback([this, timer_name](const std::string& event) { startTimer(timer_name); }, start_ev);
				context().logger().debug("event [{}] starts timer {}", start_ev, timer_name);

				auto stop_ev = PDI::to_string(PC_get(val, ".stop"));
				ctx.callbacks().add_event_callback([this, timer_name](const std::string& event) { stopTimer(timer_name); }, stop_ev);
				context().logger().debug("event [{}] stops timer {}", stop_ev, timer_name);
			} else {
				ctx.logger().debug("Defined timer (scalar/list-styled): {}", timer_name);

				opt_each(val, [&](PC_tree_t sub_elem) {
					auto start_ev = PDI::to_string(sub_elem) + "_start_timer";
					ctx.callbacks().add_event_callback([this, timer_name](const std::string& event) { startTimer(timer_name); }, start_ev);
					context().logger().debug("event [{}] starts timer {}", start_ev, timer_name);

					auto stop_ev = PDI::to_string(sub_elem) + "_stop_timer";
					ctx.callbacks().add_event_callback([this, timer_name](const std::string& event) { stopTimer(timer_name); }, stop_ev);
					context().logger().debug("event [{}] stops timer {}", stop_ev, timer_name);
				});
			}
		}
		ctx.logger().info("Plugin loaded successfully");
	}

	~timer_plugin()
	{
		for (const auto& [name, info]: timers) {
			context().logger().info("Total time spent for {} : {} seconds", name, info.accumulated_time);
		}
		save_timer_to_csv();
		context().logger().info("Closing plugin");
	}

	static std::string pretty_name() { return "Timer"; }

private:
	/** Start a timer with given name
	 *
	 * \param name name of the timer to start
	 */
	void startTimer(const std::string& name)
	{
		auto& timer = timers[name];
		if (timer.start_time.has_value()) {
			context().logger().error("Timer for {} is already running. Ignoring the start", name);
		} else {
			timer.start_time = std::chrono::high_resolution_clock::now();
		}
	}

	/** Stop a timer with given name and accumulate the duration of the timer
	 *
	 * \param name name of the timer to stop
	 */
	void stopTimer(const std::string& name)
	{
		auto it = timers.find(name);
		if (it == timers.end() || !it->second.start_time.has_value()) {
			context().logger().error("Cannot end timer for {}  because it was never started.", name);
		} else {
			auto end_time = std::chrono::high_resolution_clock::now();
			std::chrono::duration<double> elapsed = end_time - it->second.start_time.value();
			it->second.accumulated_time += elapsed.count();
			it->second.start_time = std::nullopt;
		}
	}

	void save_timer_to_csv()
	{
		auto filename = "timer.csv";
		std::ofstream file(filename, std::ios::app);

		if (!file.is_open()) {
			context().logger().error("Could not open file to write timer results to {}", filename);
			return;
		}
		int fd = open(filename, O_WRONLY);
		flock(fd, LOCK_EX);

		for (const auto& [name, info]: timers) {
			file << name << "," << info.accumulated_time << "\n";
		}

		file.flush();
		flock(fd, LOCK_UN);
		close(fd);
		file.close();
		context().logger().info("Successfully saved results to {}", filename);
	}
};

} // namespace
PDI_PLUGIN(timer)
