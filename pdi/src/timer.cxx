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

#include <pdi/logger.h>
#include <pdi/timer.h>

#include "global_context.h"

namespace PDI {

// Start a timer by name
void Timer::startTimer(const std::string& name)
{
	if (!timer_enabled) return;
	if (start_times.find(name) != start_times.end()) {
		Global_context::context().logger().error("Timer for {} is already running. Ignoring the start", name);
		return;
	}
	start_times[name] = std::chrono::high_resolution_clock::now();
}

// Stop a timer and accumulate the duration
void Timer::stopTimer(const std::string& name)
{
	if (!timer_enabled) return;
	auto it = start_times.find(name);
	if (it == start_times.end()) {
		Global_context::context().logger().error("Error: Cannot end timer for {}  because it was never started.", name);
		return;
	}

	auto end_time = std::chrono::high_resolution_clock::now();
	std::chrono::duration<double> elapsed = end_time - start_times[name];

	accumulated_times[name] += elapsed.count();
	start_times.erase(it);
}

// Output the results of all timers
void Timer::printReport() const
{
	if (!timer_enabled) return;
	for (const auto& [name, duration]: accumulated_times) {
		Global_context::context().logger().info("Totale time spent for {} : {} seconds", name, duration);
	}
}

// Output the results of timer with name
void Timer::printReport(const std::string& name) const
{
	if (!timer_enabled) return;
	auto it = accumulated_times.find(name);
	if (it != accumulated_times.end()) {
		Global_context::context().logger().info("Totale time spent for {} : {} seconds", name, it->second);
	} else {
		Global_context::context().logger().error("Cannot find timer for {}.", name);
	}
}

} // namespace PDI
