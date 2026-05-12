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
#include <string>
#include <unordered_map>

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
	std::unordered_map<std::string, std::chrono::high_resolution_clock::time_point> start_times;

	// Map of timer's name and timer's duration
	std::unordered_map<std::string, double> accumulated_times;

	// Map of start event, and different timers to be started
	std::unordered_map<std::string, std::vector<std::string> > start_events;

	// Map of start event, and different timers to be stopped
	std::unordered_map<std::string, std::vector<std::string> > stop_events;

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
				for (const auto& event_name : start_events[name]) {
    				startTimer(event_name);
				}
			} else if (stop_events.find(name) != stop_events.end()) {
				for (const auto& event_name : stop_events[name]) {
    				stopTimer(event_name);
				}
			} else {
				this->context().logger().info("event {} is not recorded by any timers. Skipping", name);
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
				ctx.logger().debug("Defined timer (map-styled): {}", timer_name);
				auto start_ev = to_string(PC_get(val, ".start"));
				auto stop_ev = to_string(PC_get(val, ".stop"));
				register_timer(start_ev, stop_ev, timer_name);
			} else {
				ctx.logger().debug("Defined timer (scalar/list-styled): {}", timer_name);
				opt_each(val, [&](PC_tree_t sub_elem) {
					auto start_ev = to_string(sub_elem) + "_start_timer";
					auto stop_ev = to_string(sub_elem) + "_stop_timer";
					register_timer(start_ev, stop_ev, timer_name);
				});
			}
		}
		print_timer_property();
	}

	void register_timer(std::string& start_event, std::string& stop_event, std::string& timer_name)
	{
		start_events[start_event].push_back(timer_name);
		stop_events[stop_event].push_back(timer_name);
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
