/*******************************************************************************
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <gtest/gtest.h>

#include "logger.cxx"
#include "context_proxy.cxx"
#include "global_context.cxx"

#include "mocks/plugin_mock.h"
#include "mocks/context_mock.h"

namespace {

using std::map;
using std::pair;
using std::string;
using std::to_string;
using std::unique_ptr;
using std::unordered_set;

using testing::InSequence;
using testing::Return;
using testing::Sequence;

using PDI::Context;
using PDI::Error;
using PDI::initialize_plugins;
using PDI::Plugin;
using PDI::Plugin_load_info;

using Plugin_dependencies = pair<unordered_set<string>, unordered_set<string>>;
using Plugin_info_map = map<string, Plugin_load_info>;

/*
 * Special Mock class to check wheter functions were called.
 */
struct CheckerBase {
	virtual void ctor_call(const string&) = 0;
	virtual void deps_call(const string&) = 0;
	virtual ~CheckerBase() = default;
};

struct CallChecker : public CheckerBase {
	MOCK_METHOD1(ctor_call, void(const string&));
	MOCK_METHOD1(deps_call, void(const string&));
};

/*
 * Checker mock object is global so the mocked functions can access it (without capturing it)
 */
unique_ptr<CallChecker> call_checker;
/*
 * Struct prepared for InitializePluginsTest.
 */
struct InitializePluginsTest : public ::testing::Test {
	MockContext ctx_mock;
	PC_tree_t empty_tree;
	unordered_map<string, unique_ptr<Plugin>> plugins_map;
	
	//default behaviour of plugins, deps and reqs are empty.
	Plugin_info_map plugins_load_info {
		{
			"plugin0",
			{
				[](Context& ctx, PC_tree_t)
				{
					call_checker->ctor_call("plugin0");
					return unique_ptr<Plugin> {new MockPlugin{ctx}};
				},
				ctx_mock,
				empty_tree,
				[]()
				{
					call_checker->deps_call("plugin0");
					return Plugin_dependencies{};
				}
			}
		},
		{
			"plugin1",
			{
				[](Context& ctx, PC_tree_t)
				{
					call_checker->ctor_call("plugin1");
					return unique_ptr<Plugin> {new MockPlugin{ctx}};
				},
				ctx_mock,
				empty_tree,
				[]()
				{
					call_checker->deps_call("plugin1");
					return Plugin_dependencies{};
				}
			}
		},
		{
			"plugin2",
			{
				[](Context& ctx, PC_tree_t)
				{
					call_checker->ctor_call("plugin2");
					return unique_ptr<Plugin> {new MockPlugin{ctx}};
				},
				ctx_mock,
				empty_tree,
				[]()
				{
					call_checker->deps_call("plugin2");
					return Plugin_dependencies{};
				}
			}
		},
		{
			"plugin3",
			{
				[](Context& ctx, PC_tree_t)
				{
					call_checker->ctor_call("plugin3");
					return unique_ptr<Plugin> {new MockPlugin{ctx}};
				},
				ctx_mock,
				empty_tree,
				[]()
				{
					call_checker->deps_call("plugin3");
					return Plugin_dependencies{};
				}
			}
		},
		{
			"plugin4",
			{
				[](Context& ctx, PC_tree_t)
				{
					call_checker->ctor_call("plugin4");
					return unique_ptr<Plugin> {new MockPlugin{ctx}};
				},
				ctx_mock,
				empty_tree,
				[]()
				{
					call_checker->deps_call("plugin4");
					return Plugin_dependencies{};
				}
			}
		},
		{
			"plugin5",
			{
				[](Context& ctx, PC_tree_t)
				{
					call_checker->ctor_call("plugin5");
					return unique_ptr<Plugin> {new MockPlugin{ctx}};
				},
				ctx_mock,
				empty_tree,
				[]()
				{
					call_checker->deps_call("plugin5");
					return Plugin_dependencies{};
				}
			}
		}
	};
	
	void SetUp() override
	{
		//global mock needs to be created in every test
		call_checker.reset(new CallChecker{});
	}
	
	void TearDown() override
	{
		//global mock needs to be destroyed in every test
		call_checker.reset(nullptr);
	}
};

/*
 * Name:                InitializePluginsTest.empty
 *
 * Tested functions:    initialize_plugins()
 *
 * Description:         Test checks if plugins without any reqirements
 *                      and dependencies are correctly initialized.
 *
 */
TEST_F(InitializePluginsTest, empty)
{
	for (int i = 0; i < plugins_load_info.size(); i++) {
		string plugin_name = "plugin";
		EXPECT_CALL(*call_checker, ctor_call(plugin_name + to_string(i)));
		EXPECT_CALL(*call_checker, deps_call(plugin_name + to_string(i)));
	}
	
	initialize_plugins(plugins_load_info, plugins_map);
	ASSERT_EQ(plugins_map.size(), 6);
}

/*
 * Name:                InitializePluginsTest.req_plugins_fulfilled
 *
 * Tested functions:    initialize_plugins()
 *
 * Description:         Test checks if plugins with fulfilled reqirements
 *                      are correctly initialized.
 *
 */
TEST_F(InitializePluginsTest, req_plugins_fulfilled)
{
	plugins_load_info.at("plugin0").m_dependencies = []() {
		call_checker->deps_call("plugin0");
		return Plugin_dependencies{{"plugin1"}, {}};
	};
	plugins_load_info.at("plugin1").m_dependencies = []() {
		call_checker->deps_call("plugin1");
		return Plugin_dependencies{{"plugin2", "plugin3"}, {}};
	};
	plugins_load_info.at("plugin2").m_dependencies = []() {
		call_checker->deps_call("plugin2");
		return Plugin_dependencies{{"plugin3", "plugin4", "plugin5"}, {}};
	};
	plugins_load_info.at("plugin3").m_dependencies = []() {
		call_checker->deps_call("plugin3");
		return Plugin_dependencies{{"plugin4"}, {}};
	};
	plugins_load_info.at("plugin4").m_dependencies = []() {
		call_checker->deps_call("plugin4");
		return Plugin_dependencies{{"plugin5"}, {}};
	};
	
	for (int i = 0; i <plugins_load_info.size(); i++) {
		string plugin_name = "plugin";
		EXPECT_CALL(*call_checker, ctor_call(plugin_name + to_string(i)));
		EXPECT_CALL(*call_checker, deps_call(plugin_name + to_string(i)));
	}
	
	initialize_plugins(plugins_load_info, plugins_map);
	ASSERT_EQ(plugins_map.size(), 6);
}

/*
 * Name:                InitializePluginsTest.req_plugins_not_fulfilled
 *
 * Tested functions:    initialize_plugins()
 *
 * Description:         Test checks if unfulfilled requirement is being detected.
 *
 */
TEST_F(InitializePluginsTest, req_plugins_not_fulfilled)
{
	plugins_load_info.at("plugin0").m_dependencies = []() {
		call_checker->deps_call("plugin0");
		return Plugin_dependencies{{"plugin1", "non-existing-plugin"}, {}};
	};
	EXPECT_CALL(*call_checker, deps_call("plugin0"));
	
	for (int i = 1; i <plugins_load_info.size(); i++) {
		string plugin_name = "plugin";
		ON_CALL(*call_checker, ctor_call(plugin_name + to_string(i)));
		ON_CALL(*call_checker, deps_call(plugin_name + to_string(i)));
	}
	ASSERT_THROW(initialize_plugins(plugins_load_info, plugins_map), Error);
}

/*
 * Name:                InitializePluginsTest.in_sequence
 *
 * Tested functions:    initialize_plugins()
 *
 * Description:         Test checks if plugins are initialized in a desired order.
 *                      The dependency order in the test is:
 *                          p0: {p1}
 *                          p1: {p2}
 *                          p2: {p3}
 *                          p3: {p4}
 *                          p4: {p5}
 *                          p5: {}
 *                      Correct order of initialization:
 *                          p5 -> p4 -> p3 -> p2 -> p1 -> p0
 */
TEST_F(InitializePluginsTest, in_sequence)
{
	plugins_load_info.at("plugin0").m_dependencies = []() {
		call_checker->deps_call("plugin0");
		return Plugin_dependencies{{}, {"plugin1"}};
	};
	plugins_load_info.at("plugin1").m_dependencies = []() {
		call_checker->deps_call("plugin1");
		return Plugin_dependencies{{}, {"plugin2"}};
	};
	plugins_load_info.at("plugin2").m_dependencies = []() {
		call_checker->deps_call("plugin2");
		return Plugin_dependencies{{}, {"plugin3"}};
	};
	plugins_load_info.at("plugin3").m_dependencies = []() {
		call_checker->deps_call("plugin3");
		return Plugin_dependencies{{}, {"plugin4"}};
	};
	plugins_load_info.at("plugin4").m_dependencies = []() {
		call_checker->deps_call("plugin4");
		return Plugin_dependencies{{}, {"plugin5"}};
	};
	
	for (int i = 0; i < plugins_load_info.size(); i++) {
		string plugin_name = "plugin";
		EXPECT_CALL(*call_checker, deps_call(plugin_name + to_string(i)));
	}
	
	{
		InSequence _;
		
		EXPECT_CALL(*call_checker, ctor_call("plugin5")).Times(1);
		EXPECT_CALL(*call_checker, ctor_call("plugin4")).Times(1);
		EXPECT_CALL(*call_checker, ctor_call("plugin3")).Times(1);
		EXPECT_CALL(*call_checker, ctor_call("plugin2")).Times(1);
		EXPECT_CALL(*call_checker, ctor_call("plugin1")).Times(1);
		EXPECT_CALL(*call_checker, ctor_call("plugin0")).Times(1);
	}
	
	initialize_plugins(plugins_load_info, plugins_map);
	ASSERT_EQ(plugins_map.size(), 6);
}

/*
 * Name:                InitializePluginsTest.multi_dependency
 *
 * Tested functions:    initialize_plugins()
 *
 * Description:         Test checks if plugins are initialized in a desired order.
 *                      The dependency order in the test is:
 *                          p0: {p1}
 *                          p1: {p2, p3}
 *                          p2: {p4, p5}
 *                          p3: {p4, p5}
 *                          p4: {}
 *                          p5: {}
 *                      Correct order of initialization:
 *                          p5 --> p3 --> p1 -> p0
 *                            \/      /
 *                            /\     /
 *                          p4 --> p2
 */
TEST_F(InitializePluginsTest, multi_dependency)
{
	plugins_load_info.at("plugin0").m_dependencies = []() {
		call_checker->deps_call("plugin0");
		return Plugin_dependencies{{}, {"plugin1"}};
	};
	plugins_load_info.at("plugin1").m_dependencies = []() {
		call_checker->deps_call("plugin1");
		return Plugin_dependencies{{}, {"plugin2", "plugin3"}};
	};
	plugins_load_info.at("plugin2").m_dependencies = []() {
		call_checker->deps_call("plugin2");
		return Plugin_dependencies{{}, {"plugin4", "plugin5"}};
	};
	plugins_load_info.at("plugin3").m_dependencies = []() {
		call_checker->deps_call("plugin3");
		return Plugin_dependencies{{}, {"plugin4", "plugin5"}};
	};
	
	for (int i = 0; i < plugins_load_info.size(); i++) {
		string plugin_name = "plugin";
		EXPECT_CALL(*call_checker, deps_call(plugin_name + to_string(i)));
	}
	
	Sequence s1, s2, s3, s4;
	
	EXPECT_CALL(*call_checker, ctor_call("plugin5")).Times(1).InSequence(s1, s2);
	EXPECT_CALL(*call_checker, ctor_call("plugin4")).Times(1).InSequence(s3, s4);
	EXPECT_CALL(*call_checker, ctor_call("plugin3")).Times(1).InSequence(s1, s3);
	EXPECT_CALL(*call_checker, ctor_call("plugin2")).Times(1).InSequence(s2, s4);
	EXPECT_CALL(*call_checker, ctor_call("plugin1")).Times(1).InSequence(s1, s2, s3, s4);
	EXPECT_CALL(*call_checker, ctor_call("plugin0")).Times(1).InSequence(s1, s2, s3, s4);
	
	initialize_plugins(plugins_load_info, plugins_map);
	ASSERT_EQ(plugins_map.size(), 6);
}

/*
 * Name:                InitializePluginsTest.one_to_all
 *
 * Tested functions:    initialize_plugins()
 *
 * Description:         Test checks if plugins are initialized in a desired order.
 *                      The dependency order in the test is:
 *                          p0: {p1, p2, p3, p4, p5}
 *                          p1: {p3}
 *                          p2: {p5}
 *                          p3: {p4}
 *                          p4: {p5}
 *                          p5: {}
 *                      Correct order of initialization:
 *                          p5 -------> p2 -------> p0
 *                            \                 /
 *                             -> p4 -> p3 -> p1
 */
TEST_F(InitializePluginsTest, one_to_all)
{
	plugins_load_info.at("plugin0").m_dependencies = []() {
		call_checker->deps_call("plugin0");
		return Plugin_dependencies{{}, {"plugin1", "plugin2", "plugin3", "plugin4", "plugin5"}};
	};
	plugins_load_info.at("plugin1").m_dependencies = []() {
		call_checker->deps_call("plugin1");
		return Plugin_dependencies{{}, {"plugin3"}};
	};
	plugins_load_info.at("plugin2").m_dependencies = []() {
		call_checker->deps_call("plugin2");
		return Plugin_dependencies{{}, {"plugin5"}};
	};
	plugins_load_info.at("plugin3").m_dependencies = []() {
		call_checker->deps_call("plugin3");
		return Plugin_dependencies{{}, {"plugin4"}};
	};
	plugins_load_info.at("plugin4").m_dependencies = []() {
		call_checker->deps_call("plugin4");
		return Plugin_dependencies{{}, {"plugin5"}};
	};
	
	for (int i = 0; i < plugins_load_info.size(); i++) {
		string plugin_name = "plugin";
		EXPECT_CALL(*call_checker, deps_call(plugin_name + to_string(i)));
	}
	
	Sequence s1, s2;
	
	EXPECT_CALL(*call_checker, ctor_call("plugin5")).Times(1).InSequence(s1, s2);
	EXPECT_CALL(*call_checker, ctor_call("plugin4")).Times(1).InSequence(s1);
	EXPECT_CALL(*call_checker, ctor_call("plugin3")).Times(1).InSequence(s1);
	EXPECT_CALL(*call_checker, ctor_call("plugin2")).Times(1).InSequence(s2);
	EXPECT_CALL(*call_checker, ctor_call("plugin1")).Times(1).InSequence(s1);
	EXPECT_CALL(*call_checker, ctor_call("plugin0")).Times(1).InSequence(s1, s2);
	
	initialize_plugins(plugins_load_info, plugins_map);
	ASSERT_EQ(plugins_map.size(), 6);
}

/*
 * Name:                InitializePluginsTest.disconnected_graph
 *
 * Tested functions:    initialize_plugins()
 *
 * Description:         Test checks if plugins are initialized in a desired order.
 *                      The dependency order in the test is:
 *                          p0: {p1, p2}
 *                          p1: {p2}
 *                          p2: {}
 *                          p3: {p4, p5}
 *                          p4: {p5}
 *                          p5: {}
 *                      Correct order of initialization:
 *                          p5 -> p4 -> p3
 *                            \     /
 *                             -----
 *
 *                          p2 -> p1 -> p0
 *                            \     /
 *                             -----
 */
TEST_F(InitializePluginsTest, disconnected_graph)
{
	plugins_load_info.at("plugin0").m_dependencies = []() {
		call_checker->deps_call("plugin0");
		return Plugin_dependencies{{}, {"plugin1", "plugin2"}};
	};
	plugins_load_info.at("plugin1").m_dependencies = []() {
		call_checker->deps_call("plugin1");
		return Plugin_dependencies{{}, {"plugin2"}};
	};
	plugins_load_info.at("plugin3").m_dependencies = []() {
		call_checker->deps_call("plugin3");
		return Plugin_dependencies{{}, {"plugin4", "plugin5"}};
	};
	plugins_load_info.at("plugin4").m_dependencies = []() {
		call_checker->deps_call("plugin4");
		return Plugin_dependencies{{}, {"plugin5"}};
	};
	
	for (int i = 0; i < plugins_load_info.size(); i++) {
		string plugin_name = "plugin";
		EXPECT_CALL(*call_checker, deps_call(plugin_name + to_string(i)));
	}
	
	Sequence s1, s2;
	
	EXPECT_CALL(*call_checker, ctor_call("plugin5")).Times(1).InSequence(s2);
	EXPECT_CALL(*call_checker, ctor_call("plugin4")).Times(1).InSequence(s2);
	EXPECT_CALL(*call_checker, ctor_call("plugin3")).Times(1).InSequence(s2);
	EXPECT_CALL(*call_checker, ctor_call("plugin2")).Times(1).InSequence(s1);
	EXPECT_CALL(*call_checker, ctor_call("plugin1")).Times(1).InSequence(s1);
	EXPECT_CALL(*call_checker, ctor_call("plugin0")).Times(1).InSequence(s1);
	
	initialize_plugins(plugins_load_info, plugins_map);
	ASSERT_EQ(plugins_map.size(), 6);
}

/*
 * Name:                InitializePluginsTest.simple_loop
 *
 * Tested functions:    initialize_plugins()
 *
 * Description:         Test checks if circular dependency between plugins is being detected.
 *                      The dependency order in the test is:
 *                          p0: {p1}
 *                          p1: {p2}
 *                          p2: {p3}
 *                          p3: {p4}
 *                          p4: {p5}
 *                          p5: {p0}
 */
TEST_F(InitializePluginsTest, simple_loop)
{
	plugins_load_info.at("plugin0").m_dependencies = []() {
		call_checker->deps_call("plugin0");
		return Plugin_dependencies{{}, {"plugin1"}};
	};
	plugins_load_info.at("plugin1").m_dependencies = []() {
		call_checker->deps_call("plugin1");
		return Plugin_dependencies{{}, {"plugin2"}};
	};
	plugins_load_info.at("plugin2").m_dependencies = []() {
		call_checker->deps_call("plugin2");
		return Plugin_dependencies{{}, {"plugin3"}};
	};
	plugins_load_info.at("plugin3").m_dependencies = []() {
		call_checker->deps_call("plugin3");
		return Plugin_dependencies{{}, {"plugin4"}};
	};
	plugins_load_info.at("plugin4").m_dependencies = []() {
		call_checker->deps_call("plugin4");
		return Plugin_dependencies{{}, {"plugin5"}};
	};
	plugins_load_info.at("plugin5").m_dependencies = []() {
		call_checker->deps_call("plugin5");
		return Plugin_dependencies{{}, {"plugin0"}};
	};
	
	for (int i = 0; i < plugins_load_info.size(); i++) {
		string plugin_name = "plugin";
		EXPECT_CALL(*call_checker, deps_call(plugin_name + to_string(i)));
	}
	
	ASSERT_THROW(initialize_plugins(plugins_load_info, plugins_map), Error);
}
/*
 * Name:                InitializePluginsTest.complex_loop
 *
 * Tested functions:    initialize_plugins()
 *
 * Description:         Test checks if circular dependency between plugins is being detected.
 *                      The dependency order in the test is:
 *                          p0: {p1, p2}
 *                          p1: {p2}
 *                          p2: {p3}
 *                          p3: {p4, p5}
 *                          p4: {p5}
 *                          p5: {p0}
 */
TEST_F(InitializePluginsTest, complex_loop)
{
	plugins_load_info.at("plugin0").m_dependencies = []() {
		call_checker->deps_call("plugin0");
		return Plugin_dependencies{{}, {"plugin1", "plugin2"}};
	};
	plugins_load_info.at("plugin1").m_dependencies = []() {
		call_checker->deps_call("plugin1");
		return Plugin_dependencies{{}, {"plugin2"}};
	};
	plugins_load_info.at("plugin2").m_dependencies = []() {
		call_checker->deps_call("plugin2");
		return Plugin_dependencies{{}, {"plugin3"}};
	};
	plugins_load_info.at("plugin3").m_dependencies = []() {
		call_checker->deps_call("plugin3");
		return Plugin_dependencies{{}, {"plugin4", "plugin5"}};
	};
	plugins_load_info.at("plugin4").m_dependencies = []() {
		call_checker->deps_call("plugin4");
		return Plugin_dependencies{{}, {"plugin5"}};
	};
	plugins_load_info.at("plugin5").m_dependencies = []() {
		call_checker->deps_call("plugin5");
		return Plugin_dependencies{{}, {"plugin0"}};
	};
	
	for (int i = 0; i < plugins_load_info.size(); i++) {
		string plugin_name = "plugin";
		EXPECT_CALL(*call_checker, deps_call(plugin_name + to_string(i)));
	}
	
	ASSERT_THROW(initialize_plugins(plugins_load_info, plugins_map), Error);
}
}
