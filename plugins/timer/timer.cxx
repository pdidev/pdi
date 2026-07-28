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
	std::unordered_map<std::string, TimerInfo> m_timers;
	std::string m_output_path = "cout";

public:
	timer_plugin(Logger& log, Context& ctx, PC_tree_t spec_tree)
		: Plugin{log, ctx}
	{
		if (PC_status(spec_tree)) {
			logger().error("Error in read_config_tree");
			return;
		}
		if (is_list(spec_tree)) {
			for (int i = 0; i < len(spec_tree, 0); i++) {
				PC_tree_t timer_item = PC_get(spec_tree, "[%d]", i);
				std::string timer_name = PDI::to_string(PC_get(timer_item, "{0}"));
				if (timer_name == "output_to") {
					m_output_path = PDI::to_string(PC_get(timer_item, ".%s", "output_to"));
					continue;
				}

				PC_tree_t val = PC_get(timer_item, ".%s", timer_name.c_str());
				if (is_map(val)) {
					logger().debug("Defined timer (map-styled): {}", timer_name);

					auto start_ev = PDI::to_string(PC_get(val, ".start"));
					ctx.on_event([this, timer_name](const std::string& event) { startTimer(timer_name); }, start_ev);
					logger().debug("event [{}] starts timer {}", start_ev, timer_name);

					auto stop_ev = PDI::to_string(PC_get(val, ".stop"));
					ctx.on_event([this, timer_name](const std::string& event) { stopTimer(timer_name); }, stop_ev);
					logger().debug("event [{}] stops timer {}", stop_ev, timer_name);
				} else {
					logger().debug("Defined timer (scalar/list-styled): {}", timer_name);

					opt_each(val, [&](PC_tree_t sub_elem) {
						auto start_ev = PDI::to_string(sub_elem) + "_start_timer";
						ctx.on_event([this, timer_name](const std::string& event) { startTimer(timer_name); }, start_ev);
						logger().debug("event [{}] starts timer {}", start_ev, timer_name);

						auto stop_ev = PDI::to_string(sub_elem) + "_stop_timer";
						ctx.on_event([this, timer_name](const std::string& event) { stopTimer(timer_name); }, stop_ev);
						logger().debug("event [{}] stops timer {}", stop_ev, timer_name);
					});
				}
			}
		} else if (is_map(spec_tree)) {
			opt_each(spec_tree, [&](PC_tree_t timer_item) {
				for (int i = 0; i < PDI::len(timer_item, 0); i++) {
					std::string timer_name = PDI::to_string(PC_get(timer_item, "{%d}", i));
					if (timer_name == "output_to") {
						m_output_path = PDI::to_string(PC_get(timer_item, ".%s", "output_to"));
					} else {
						PC_tree_t val = PC_get(timer_item, ".%s", timer_name.c_str());
						if (is_map(val)) {
							logger().debug("Defined timer (map-styled): {}", timer_name);

							auto start_ev = PDI::to_string(PC_get(val, ".start"));
							ctx.on_event([this, timer_name](const std::string& event) { startTimer(timer_name); }, start_ev);
							logger().debug("event [{}] starts timer {}", start_ev, timer_name);

							auto stop_ev = PDI::to_string(PC_get(val, ".stop"));
							ctx.on_event([this, timer_name](const std::string& event) { stopTimer(timer_name); }, stop_ev);
							logger().debug("event [{}] stops timer {}", stop_ev, timer_name);
						} else {
							logger().debug("Defined timer (scalar/list-styled): {}", timer_name);

							opt_each(val, [&](PC_tree_t sub_elem) {
								auto start_ev = PDI::to_string(sub_elem) + "_start_timer";
								ctx.on_event([this, timer_name](const std::string& event) { startTimer(timer_name); }, start_ev);
								logger().debug("event [{}] starts timer {}", start_ev, timer_name);

								auto stop_ev = PDI::to_string(sub_elem) + "_stop_timer";
								ctx.on_event([this, timer_name](const std::string& event) { stopTimer(timer_name); }, stop_ev);
								logger().debug("event [{}] stops timer {}", stop_ev, timer_name);
							});
						}
					}
				}
			});
		}
		logger().info("Plugin loaded successfully");
		logger().debug("Timer output to {}", m_output_path);
	}

	~timer_plugin()
	{
		output_timer();
		logger().info("Closing plugin");
	}

	static std::string pretty_name() { return "Timer"; }

private:
	/** Start a timer with given name
	 *
	 * \param name name of the timer to start
	 */
	void startTimer(const std::string& name)
	{
		auto& timer = m_timers[name];
		if (timer.start_time.has_value()) {
			logger().error("Timer for {} is already running. Ignoring the start", name);
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
		auto it = m_timers.find(name);
		if (it == m_timers.end() || !it->second.start_time.has_value()) {
			logger().error("Cannot end timer for {}  because it was never started.", name);
		} else {
			auto end_time = std::chrono::high_resolution_clock::now();
			std::chrono::duration<double> elapsed = end_time - it->second.start_time.value();
			it->second.accumulated_time += elapsed.count();
			it->second.start_time = std::nullopt;
		}
	}

	void output_timer()
	{
		if (m_output_path == "cout") {
			for (const auto& [name, info]: m_timers) {
				logger().info("Total time spent for {} : {} seconds", name, info.accumulated_time);
			}
			logger().info("Successfully output results to standard output");
		} else {
			auto filename = m_output_path.c_str();
			int fd = open(filename, O_WRONLY | O_CREAT | O_APPEND, 0644);
			if (fd == -1) {
				logger().error("Could not open file {}", filename);
				return;
			}

			if (flock(fd, LOCK_EX) == 0) {
				for (const auto& [name, info]: m_timers) {
					dprintf(fd, "%s,%f\n", name.c_str(), info.accumulated_time);
				}

				fsync(fd);
				flock(fd, LOCK_UN);
			}

			close(fd);
			logger().info("Successfully saved results to {}", filename);
		}
	}
};

} // namespace
PDI_PLUGIN(timer)
