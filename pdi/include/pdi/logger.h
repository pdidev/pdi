/*******************************************************************************
 * Copyright (C) 2018-2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_LOGGER_H_
#define PDI_LOGGER_H_

#include <utility>
#include <string>
#include <vector>

#include <spdlog/logger.h>
#include <spdlog/spdlog.h>

#include <pdi/pdi_fwd.h>
#include <pdi/paraconf_wrapper.h>

namespace PDI {

/// Wrapper for spdlog::logger with additional pattern getter method
class PDI_EXPORT Logger
{
	/// spdlog logger
	std::shared_ptr<spdlog::logger> m_logger;
	
	/// observers of this logger to update when default pattern changes
	std::vector<std::reference_wrapper<Logger>> m_default_pattern_observers;
	
	/// vector that contains pattern block from plugins added to the logger
	std::vector<std::string> m_pattern_blocks;
	
	/// pattern of spdlog logger
	std::string m_pattern = "[%T][%n] *** %^%l%$: %v";
	
	/// true if default_pattern method shouldn't change the pattern
	bool m_pattern_from_config = false;
	
	/// builds new version of the pattern
	void build_pattern();
	
	/// parent logger
	Logger* m_parent_logger = nullptr; //change in c++17 to std::optional
	
public:
	/// Creates new empty logger
	Logger() = default;
	
	/** Creates new logger
	 * \param[in] logger_name logger name that will be displayed
	 * \param[in] config configuration tree from config file
	 * \param[in] level default level of the logger
	 */
	Logger(const std::string& logger_name, PC_tree_t config, spdlog::level::level_enum level = spdlog::level::info);
	
	/** Creates new logger with parent logger
	 * \param[in] parent_logger the logger to observe if default pattern has changed
	 * \param[in] logger_name logger name that will be displayed
	 * \param[in] config configuration tree from config file
	 *
	 * Loggers can inherit block structure from parent_logger. When inherited,
	 * pattern and level of the parent will be set as default pattern and level of the child.
	 * The pattern and level are inherited if they are not set in child config file.
	 * For now it works with plugin loggers. PDI core logger is the parent and plugin logger is the child.
	 *
	 */
	Logger(Logger& parent_logger, const std::string& logger_name, PC_tree_t config);
	
	/** Sets up the logger
	 * \param[in] logger_name logger name that will be displayed
	 * \param[in] config configuration tree from config file
	 * \param[in] level default level of the logger
	 */
	void setup(const std::string& logger_name, PC_tree_t config, spdlog::level::level_enum level = spdlog::level::info);
	
	/** Sets up the logger with parent logger
	 * \param[in] parent_logger the logger to observe if default pattern has changed
	 * \param[in] logger_name logger name that will be displayed
	 * \param[in] config configuration tree from config file
	 */
	void setup(Logger& parent_logger, const std::string& logger_name, PC_tree_t config);
	
	/** Changes pattern of the logger
	 *
	 * \param[in] pattern pattern to set
	 */
	void pattern(const std::string& pattern);
	
	/** Changes pattern of the global logger
	 *
	 * \param[in] pattern pattern to set
	 *
	 * The global logger is the logger without a parent.
	 *
	 */
	void global_pattern(const std::string& pattern);
	
	/** Changes default pattern of the logger
	 *  (won't be updated if current pattern  is from config)
	 *
	 * \param[in] pattern pattern to set
	 */
	void default_pattern(const std::string& pattern);
	
	/** Add new element to default pattern
	 *
	 * \param[in] block new string block to add
	 */
	void add_pattern_block(const std::string& block);
	
	/** Add new element to default pattern of the global logger
	*
	* \param[in] block new string block to add
	*
	* Adds block to the pattern of the global logger.
	*/
	void add_pattern_global_block(const std::string& block);
	
	/** Returns pattern of the logger
	 * \return pattern of the logger
	 */
	const std::string& pattern() const;
	
	/** Sets logger level
	 * \param[in] log_level level to set
	 */
	void level(spdlog::level::level_enum log_level);
	
	/** Returns level of the logger
	 * \return level of the logger
	 */
	spdlog::level::level_enum level() const;
	
	/** Evaluate pattern
	 *
	 * \param[in] ctx the context in which to evaluate the pattern
	 *
	 * Evaluation of the pattern.
	 */
	void evaluate_pattern(Context& ctx) const;
	
	/** Evaluate global pattern
	 *
	 * \param[in] ctx the context in which to evaluate the pattern
	 *
	 * Evaluation of the pattern. Used to evaluate parent and child patterns.
	 *
	 */
	void evaluate_global_pattern(Context& ctx) const;
	
	/** Writes trace level message
	 * \param[in] fmt fmt formatted string
	 * \param[in] args arguments for fmt string
	 */
	template<typename... Args>
	void trace(const char* fmt, Args&& ... args)
	{
		m_logger->trace(fmt, std::forward<Args>(args)...);
	}
	
	/** Writes debug level message
	 * \param[in] fmt fmt formatted string
	 * \param[in] args arguments for fmt string
	 */
	template<typename... Args>
	void debug(const char* fmt, Args&& ... args)
	{
		m_logger->debug(fmt, std::forward<Args>(args)...);
	}
	
	/** Writes info level message
	 * \param[in] fmt fmt formatted string
	 * \param[in] args arguments for fmt string
	 */
	template<typename... Args>
	void info(const char* fmt, Args&& ... args)
	{
		m_logger->info(fmt, std::forward<Args>(args)...);
	}
	
	/** Writes warning level message
	 * \param[in] fmt fmt formatted string
	 * \param[in] args arguments for fmt string
	 */
	template<typename... Args>
	void warn(const char* fmt, Args&& ... args)
	{
		m_logger->warn(fmt, std::forward<Args>(args)...);
	}
	
	/** Writes error level message
	 * \param[in] fmt fmt formatted string
	 * \param[in] args arguments for fmt string
	 */
	template<typename... Args>
	void error(const char* fmt, Args&& ... args)
	{
		m_logger->error(fmt, std::forward<Args>(args)...);
	}
	
	/** Returns real spdlog logger
	 * \return spdlog logger
	 */
	std::shared_ptr<spdlog::logger> real_logger();
	
};

} // namespace PDI

#endif // PDI_LOGGER_H_
