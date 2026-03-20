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

#include <pdi/timer.h>

namespace PDI {



// Stop a timer and accumulate the duration
void Timer::stopTimer(const std::string& name)
{
	#ifdef WITH_TIMER_REPORT
	auto end_time = std::chrono::high_resolution_clock::now();

	if (start_times.find(name) != start_times.end()) {
		std::chrono::duration<double> elapsed = end_time - start_times[name];
		accumulated_times[name] += elapsed.count();
	}
	#endif
}
// Start a timer by name
void Timer::startTimer(const std::string& name)
{
	#ifdef WITH_TIMER_REPORT
	start_times[name] = std::chrono::high_resolution_clock::now();
	#endif
}

// Export the results
void Timer::printReport() const
{
	#ifdef WITH_TIMER_REPORT
	std::cout << "\n--- Full Timer Report ---" << std::endl;
	for (const auto& [name, duration]: accumulated_times) {
		std::cout << name << ": " << duration << " seconds" << std::endl;
	}
	#endif
}

void Timer::printReport(const std::string& name) const
{
	#ifdef WITH_TIMER_REPORT
	std::cout << "\n--- Timer Report : " << std::endl;
	auto it = accumulated_times.find(name);

	if (it != accumulated_times.end()) {
		std::cout << name << " : " << it->second << " seconds" << std::endl;
	} else {
		std::cout << name << " timer not found." << std::endl;
	}
	#endif
}

// Get the map directly (useful for MPI export)
const std::map<std::string, double>& Timer::getResults()
{
	#ifdef WITH_TIMER_REPORT
	return accumulated_times;
	#endif
}


TimerManager& TimerManager::getInstance()
{
	static TimerManager instance;
	return instance;
}

// Start a timer by name
void TimerManager::startTimer(const std::string& name)
{
	start_times[name] = std::chrono::high_resolution_clock::now();
}

// Stop a timer and accumulate the duration
void TimerManager::stopTimer(const std::string& name)
{
	auto end_time = std::chrono::high_resolution_clock::now();

	if (start_times.find(name) != start_times.end()) {
		std::chrono::duration<double> elapsed = end_time - start_times[name];
		accumulated_times[name] += elapsed.count();
	}
}

// Export the results
void TimerManager::printReport() const
{
	std::cout << "\n--- Full Timer Report ---" << std::endl;
	for (const auto& [name, duration]: accumulated_times) {
		std::cout << name << ": " << duration << " seconds" << std::endl;
	}
}

void TimerManager::printReport(const std::string& name) const
{
	std::cout << "\n--- Timer Report : " << std::endl;
	auto it = accumulated_times.find(name);

	if (it != accumulated_times.end()) {
		std::cout << name << " : " << it->second << " seconds" << std::endl;
	} else {
		std::cout << name << " timer not found." << std::endl;
	}
}

// Get the map directly (useful for MPI export)
const std::map<std::string, double>& TimerManager::getResults()
{
	return accumulated_times;
}
} // namespace PDI
