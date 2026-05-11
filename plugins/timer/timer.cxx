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

#include <string>
#include <map>
#include <chrono>

#include <pdi/context.h>
#include <pdi/logger.h>
#include <pdi/plugin.h>

namespace {

using namespace PDI;

/** The timer plugin 
*/
class timer_plugin: public PDI::Plugin
{
	// Map of timer's name and timer's starting point
	std::map<std::string, std::chrono::high_resolution_clock::time_point> start_times;

	// Map of timer's name and timer's duration
	std::map<std::string, double> accumulated_times;

	// Map of start event, and different timers to be started
	std::map<const std::string, std::vector<std::string> > start_events;

	// Map of start event, and different timers to be stopped
	std::map<const std::string, std::vector<std::string> > stop_events;

private:
	Context& timer_context;

public:
	timer_plugin(Context& ctx, PC_tree_t spec_tree)
		: Plugin{ctx}
		, timer_context{ctx}
	{
		read_config_tree(ctx, spec_tree);

		ctx.callbacks().add_event_callback([this](const std::string& name) {
			if (start_events.find(name) != start_events.end()) {
				for (int i = 0; i < start_events[name].size(); i++) {
					auto sub_timer_name = start_events[name][i];
					startTimer(sub_timer_name);
				}
			} else if (stop_events.find(name) != stop_events.end()) {
				for (int i = 0; i < stop_events[name].size(); i++) {
					auto sub_timer_name = stop_events[name][i];
					stopTimer(sub_timer_name);
				}
			} else {
				this->context().logger().info("event {} is not recoreded by any timers. Skipping", name);
			}
		});

		ctx.logger().info("Plugin loaded successfully");
	}

	~timer_plugin()
	{
		printReport();
		context().logger().info("Closing plugin");
	}

	static std::string pretty_name() { return "TIMER"; }

private:
	/** Read the configuration file
	 *
	 * \param logger PDI's logger instance
	 * \param spec_tree the yaml tree
	 */
	void read_config_tree(Context& ctx, PC_tree_t spec_tree)
	{
		if (PC_status(spec_tree)) {
			ctx.logger().error("Error in read_config_tree");
			return;
		}

		for (int i = 0; i < len(spec_tree, 0); i++) {
			PC_tree_t timer_item = PC_get(spec_tree, "[%d]", i);
			std::string timer_name = to_string(PC_get(timer_item, "{0}"));

			PC_tree_t val = PC_get(timer_item, ".%s", timer_name.c_str());
			if (is_map(val)) {
				bool timer_start_defined = false;
				bool timer_stop_defined = false;
				ctx.logger().debug("Defined timer (map-styled): {}", timer_name);
				each(val, [&](PC_tree_t key_tree, PC_tree_t value) {
					if (!PC_status(value)) {
						std::string key = to_string(key_tree);
						if (key == "start") {
							auto st = to_string(value);
							start_events[st].push_back(timer_name);
							ctx.logger().debug("\t start_event = {}", st);
							timer_start_defined = true;
						} else if (key == "stop") {
							auto st = to_string(value);
							ctx.logger().debug("\t stop_event = {}", st);
							stop_events[st].push_back(timer_name);
							timer_stop_defined = true;
						}
					}
				});
				if (!timer_start_defined || !timer_stop_defined) {
					throw Spectree_error{val, "Both start and stop attributs are mandatory for timer {}", timer_name};
				}
			} else if (is_scalar(val)) {
				ctx.logger().debug("Defined timer (scalar-styled): {}", timer_name);
				auto st = to_string(val) + "_start_timer";
				start_events[st].push_back(timer_name);
				ctx.logger().debug("\t start_event = {}", st);
				st = to_string(val) + "_stop_timer";
				stop_events[st].push_back(timer_name);
				ctx.logger().debug("\t stop_event = {}", st);
			} else if (is_list(val)) {
				ctx.logger().debug("Defined timer (list-styled): {}", timer_name);
				int size_list = len(val, 0);
				for (int i = 0; i < size_list; i++) {
					auto st = to_string(PC_get(val, "[%d]", i)) + "_start_timer";
					ctx.logger().debug("\t start_event = {}", st);
					start_events[st].push_back(timer_name);
					st = to_string(PC_get(val, "[%d]", i)) + "_stop_timer";
					ctx.logger().debug("\t stop_event = {}", st);
					stop_events[st].push_back(timer_name);
				}
			} else {
				ctx.logger().warn("timer not scalar, not map, not list");
				throw Spectree_error{val, "Timer not correctly defined"};
			}
		}
		print_timer_property();
	}

	void print_timer_property()
	{
		timer_context.logger().debug("All registered timers: ");
		for (const auto& [key, value]: start_events) {
			timer_context.logger().debug("event [{}] starts timer ", key);
			for (auto n: value) {
				timer_context.logger().debug(" \t\t {} ", n);
			}
		}
		for (const auto& [key, value]: stop_events) {
			timer_context.logger().debug("event [{}] stops timer ", key);
			for (auto n: value) {
				timer_context.logger().debug(" \t\t {} ", n);
			}
		}
	}

	void startTimer(const std::string& name)
	{
		if (start_times.find(name) != start_times.end()) {
			timer_context.logger().error("Timer for {} is already running. Ignoring the start", name);
			return;
		}
		start_times[name] = std::chrono::high_resolution_clock::now();
	}

	// Stop a timer and accumulate the duration
	void stopTimer(const std::string& name)
	{
		auto it = start_times.find(name);
		if (it == start_times.end()) {
			timer_context.logger().error("Error: Cannot end timer for {}  because it was never started.", name);
			return;
		}

		auto end_time = std::chrono::high_resolution_clock::now();
		std::chrono::duration<double> elapsed = end_time - start_times[name];

		accumulated_times[name] += elapsed.count();
		start_times.erase(it);
	}

	// Output the results of all timers
	void printReport() const
	{
		for (const auto& [name, duration]: accumulated_times) {
			timer_context.logger().info("Totale time spent for {} : {} seconds", name, duration);
		}
	}
};

} // namespace
PDI_PLUGIN(timer)
