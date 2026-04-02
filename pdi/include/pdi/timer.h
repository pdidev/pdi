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

#ifndef PDI_TIMER_H_
#define PDI_TIMER_H_

#include <chrono>
#include <iostream>
#include <map>
#include <string>
#include <utility>

#include <pdi/pdi_fwd.h>

namespace PDI {

class PDI_EXPORT Timer
{
public:
	Timer(const Timer&) = delete;

	Timer() {}

	void operator= (const Timer&) = delete;

	void startTimer(const std::string& name);
	void stopTimer(const std::string& name);
	void printReport() const;
	void printReport(const std::string& name) const;

	const std::map<std::string, double>& getResults();

	void enable_timer(const bool choice) { timer_enabled = choice; }

private:
	std::map<std::string, std::chrono::high_resolution_clock::time_point> start_times;
	std::map<std::string, double> accumulated_times;
	bool timer_enabled = false;
};
} // namespace PDI
#endif // PDI_TIMER_H_
