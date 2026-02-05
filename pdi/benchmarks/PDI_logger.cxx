// SPDX-FileCopyrightText: 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <iostream>
#include <string>
#include <benchmark/benchmark.h>

#include <fcntl.h>
#include <unistd.h>

#include <paraconf.h>
#include <pdi/callbacks.h>
#include "global_context.h"

struct Turn_off_stdout {
	PDI::Context& m_ctx;
	int m_stdout_d;
	int m_stderr_d;
	int m_dev_null_d;

	Turn_off_stdout(PDI::Context& ctx)
		: m_ctx{ctx}
	{
		m_stdout_d = dup(1);
		m_stderr_d = dup(2);
		m_dev_null_d = open("/dev/null", O_WRONLY);
		dup2(m_dev_null_d, 1);
		dup2(m_dev_null_d, 2);
	}

	~Turn_off_stdout()
	{
		dup2(m_stdout_d, 1);
		dup2(m_stderr_d, 2);
		close(m_stdout_d);
		close(m_stderr_d);
		close(m_dev_null_d);
		m_ctx.logger().level(spdlog::level::off);
	}
};

class PDI_Logger: public benchmark::Fixture
{
	PDI::Paraconf_wrapper pw;
	std::unique_ptr<PDI::Global_context> m_ctx;

public:
	PDI::Context& context() { return *m_ctx; }

	void SetUp(const ::benchmark::State& state) { m_ctx.reset(new PDI::Global_context{PC_parse_string("{logging: off}")}); }

	void TearDown(const ::benchmark::State& state) {}
};

BENCHMARK_F(PDI_Logger, OffLevel)(benchmark::State& state)
{
	Turn_off_stdout off_stdout{context()};
	for (auto _: state) {
		context().logger().trace("trace");
		context().logger().debug("debug");
		context().logger().info("info");
		context().logger().warn("warn");
		context().logger().error("error");
	}
}

BENCHMARK_F(PDI_Logger, ErrorLevel)(benchmark::State& state)
{
	Turn_off_stdout off_stdout{context()};
	context().logger().level(spdlog::level::err);
	for (auto _: state) {
		context().logger().trace("trace");
		context().logger().debug("debug");
		context().logger().info("info");
		context().logger().warn("warn");
		context().logger().error("error");
	}
}

BENCHMARK_F(PDI_Logger, WarnLevel)(benchmark::State& state)
{
	Turn_off_stdout off_stdout{context()};
	context().logger().level(spdlog::level::warn);
	for (auto _: state) {
		context().logger().trace("trace");
		context().logger().debug("debug");
		context().logger().info("info");
		context().logger().warn("warn");
		context().logger().error("error");
	}
}

BENCHMARK_F(PDI_Logger, InfoLevel)(benchmark::State& state)
{
	Turn_off_stdout off_stdout{context()};
	context().logger().level(spdlog::level::info);
	for (auto _: state) {
		context().logger().trace("trace");
		context().logger().debug("debug");
		context().logger().info("info");
		context().logger().warn("warn");
		context().logger().error("error");
	}
}

BENCHMARK_F(PDI_Logger, DebugLevel)(benchmark::State& state)
{
	Turn_off_stdout off_stdout{context()};
	context().logger().level(spdlog::level::debug);
	for (auto _: state) {
		context().logger().trace("trace");
		context().logger().debug("debug");
		context().logger().info("info");
		context().logger().warn("warn");
		context().logger().error("error");
	}
}

BENCHMARK_F(PDI_Logger, TraceLevel)(benchmark::State& state)
{
	Turn_off_stdout off_stdout{context()};
	context().logger().level(spdlog::level::trace);
	for (auto _: state) {
		context().logger().trace("trace");
		context().logger().debug("debug");
		context().logger().info("info");
		context().logger().warn("warn");
		context().logger().error("error");
	}
}
