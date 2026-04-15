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

#ifndef PDI_TESTING_H_
#define PDI_TESTING_H_

// this is a header-only file because we don't want to depend on a given version of gtest/gmock
/** \file
 * Tools to help testing %PDI plugins.
 *
 * To include this, you must link against both gtest & gmock.
 * - Let your tests be fixtures using TEST_F and have your fixture class inherit ::PDI::PdiTest .
 * - Call ::PDI::PdiTest::InitPdi() to initialize %PDI in your test.
 * 
 * Have a look at Decl'HDF5 plugin `decl_hdf5_tests.cxx` for an example of use.
 * 
 * It offers the following features.
 * - If you need to re-init %PDI, you can re-call ::PDI::PdiTest::InitPdi() .
 * - If you want to finalize Pdi before you re-initialize it, you can call
 *   ::PDI::PdiTest::FinalizePdi()
 * - ::PDI::make_a<T>() initialize your data to "random" but repeatable values.
 * - `EXPECT_CALL(*this, PdiError(errcodeMatcher, errmsgMatcher))` if you expect an error.
 * 
 * Each test will run in a separate directory that's cleaned up after execution, feel free to create
 * any file you need there.
 * In order to be able to use make_a for your own classes & structures, you'll need to define a
 * matching random_init() function.
 * 
 * Some recommendations:
 * - Follow gtest naming requirements for the tests: valid C++ identifiers without _ (underscores)
 * - Make each test target one feature and test it well.
 * - Use standard c++ ::std::filesystem or the plugin underlying library directly to check that what
 *   you expected actually append.
 * - Use C++ types, such as ::std::array<int, N> vs. int[N].
 * - When defining your own type, make them default constructibel, give them a init_from member 
 *   function, a default comparison operator and a stream output operator.
 * - Make the data you expose for writing const as much as possible with the pattern: 
 *   `auto const myvar = make_a<MyType>()`.
 * - Use a separate variable for reading as much as possible, so that you can compare it against the
 *   unmodified expected value, do not rely on magic constants.
 * - When expecting errors, use partial or regex testers for strings to support message improvement.
 */

#include <algorithm>
#include <filesystem>
#include <random>
#include <ranges>
#include <type_traits>

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <paraconf.h>
#include <pdi.h>

namespace PDI {

/** An object is initializable_from<G> if it can be initalized using the init_from member function
 * from a reference of G
 */
template < class T, class G >
concept initializable_from = requires(T t, G& gen) {
	{
		t.init_from(gen)
	};
};

/** An object is buildable_from<G> if it can be constructed using the init_from member function
 * from a reference of G
 */
template <class T, class G>
concept buildable_from = std::constructible_from<T, G&> && requires(G& g) {
	{
		T(g)
	};
	{
		::new T(g)
	};
};

/** initialize an integral value with uniformly distributed values from the provided generator
 * \param gen the (pseudo)random generator
 * \param t the value to initialize
 */
static inline void random_init(std::uniform_random_bit_generator auto& gen, std::integral auto& t)
{
	using T = std::remove_reference_t<decltype(t)>;
	t = std::uniform_int_distribution<T>(std::numeric_limits<T>::min())(gen);
}

/** initialize a floating-point value with normally distributed values from the provided generator
 * \param gen the (pseudo)random generator
 * \param t the value to initialize
 */
static inline void random_init(std::uniform_random_bit_generator auto& gen, std::floating_point auto& t)
{
	t = std::normal_distribution<std::remove_reference_t<decltype(t)>>{}(gen);
}

/** initialize all elements in an iterable with the provided generator
 * \param gen the (pseudo)random generator
 * \param t the iterable to initialize
 */
static inline void random_init(std::uniform_random_bit_generator auto& gen, std::ranges::input_range auto& t)
{
	std::for_each(std::begin(t), std::end(t), [&](std::ranges::range_value_t<decltype(t)>& v) { random_init(gen, v); });
}

/** initialize an object that supports `init_from` from the provided generator
 * \param gen the (pseudo)random generator
 * \param t the object to initialize
 */
static inline void random_init(std::uniform_random_bit_generator auto& gen, initializable_from<decltype(gen)> auto& t)
{
	t.init_from(gen);
}

/** initialize an object that can be constructed from a generator from the provided generator
 * \param gen the (pseudo)random generator
 * \param t the object to initialize
 */
template <typename T, std::uniform_random_bit_generator G>
requires(!std::ranges::input_range<T> && !initializable_from<T, G> && buildable_from<T, G> && std::assignable_from<T&, T &&>)
static inline void random_init(G& gen, T& t)
{
	t = T(gen);
}

/** make a new randomly-initialized object that can be constructed from a generator
 * \param gen the (pseudo)random generator
 */
template <typename T, std::uniform_random_bit_generator G>
requires buildable_from<T, G>
static inline T make_random(G& gen)
{
	return T(gen);
}

/** make a new randomly-initialized object that supports `random_init`
 * \param gen the (pseudo)random generator
 */
template <typename T, std::uniform_random_bit_generator G>
requires(!buildable_from<T, G>, std::default_initializable<T>)
static inline T make_random(G& gen)
{
	T result;
	random_init(gen, result);
	return result;
}

/** The fixture class for PDI plugins testing.
 * 
 * - Each test run in its own directory.
 * - offer make_a to create data
 * - Takes ownership of PC_tree and handles initialization & finalization of PDI
 */
// this is a header-only class because we don't want to depend on a given version of gtest/gmock
class PdiTest: public ::testing::Test
{
	/// The workdir when the test was launched, we'll go back there after cleaning
	std::filesystem::path m_workdir;

	/// The temp directory we created and moved to, we'll remove it when cleaning
	std::filesystem::path m_tmpdir;

	/// The configuration provided to PDI, we'll have to destroy it
	PC_tree_t m_conf = {PC_NODE_NOT_FOUND, nullptr, nullptr};

	/// The generator used to initialize data for the test
	std::mt19937_64 m_random_generator;

	/// The PDI compatible errhandler we use as an adapter for PdiError
	static void s_pdi_errhandler(PDI_status_t status, const char* message, void* context)
	{
		static_cast<PdiTest*>(context)->PdiError(status, message);
	}

protected:
	PdiTest();

	~PdiTest();

	/** make a new repeatably randomly initialized object
	 */
	template <typename T>
	inline T make_a()
	{
		return ::PDI::make_random<T>(m_random_generator);
	}

	/** Initialize PDI with the provided PC_tree
	 * 
	 * Takes ownership of the tree.
	 * If %PDI was already initialized, finalize it first.
	 * 
	 * \param tree the tree to initialize from.
	 */
	inline void InitPdi(PC_tree_t tree);

	/** Finalize PDI
	 * 
	 * Leaves any file created available until the end of the test
	 */
	inline void FinalizePdi();

	MOCK_METHOD(void, PdiError, (PDI_status_t, char const *), (const));
};

PdiTest::PdiTest()
	: m_workdir(std::filesystem::canonical(std::filesystem::current_path()))
{
	static thread_local auto random_generator([]() {
		if (int gseed = ::testing::UnitTest::GetInstance()->random_seed()) {
			return std::mt19937_64(gseed);
		} else {
			std::random_device source;
			std::array<unsigned, std::mt19937_64::state_size> random_seed_data;
			std::generate(std::begin(random_seed_data), std::end(random_seed_data), std::ref(source));
			std::seed_seq seed(std::begin(random_seed_data), std::end(random_seed_data));
			return std::mt19937_64(seed);
		}
	}());
	auto const filename = [&]() {
		static constexpr char const VALID_FILE_CHARS[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-";
		std::uniform_int_distribution<size_t> random(0, std::ranges::size(VALID_FILE_CHARS) - 1);
		std::string filename(16, '\0');
		std::generate(filename.begin(), filename.end(), [&]() { return VALID_FILE_CHARS[random(random_generator)]; });
		auto&& test_info = *::testing::UnitTest::GetInstance()->current_test_info();
		return std::string("pdi_tst_dir.") + test_info.test_suite_name() + "." + test_info.name() + "." + filename;
	}();
	m_tmpdir = m_workdir / filename;
	std::filesystem::create_directory(m_tmpdir);
	std::filesystem::current_path(m_tmpdir);
	EXPECT_CALL(*this, PdiError(testing::_, testing::_)).Times(0);
}

inline void PdiTest::InitPdi(PC_tree_t tree)
{
	FinalizePdi();
	m_conf = tree;
	ASSERT_EQ(PC_OK, PC_status(m_conf));
	ASSERT_EQ(PDI_OK, PDI_init(m_conf));
	PDI_errhandler({s_pdi_errhandler, this});
}

inline void PdiTest::FinalizePdi()
{
	if (!PC_status(m_conf)) {
		ASSERT_EQ(PDI_OK, PDI_finalize());
		EXPECT_EQ(PC_OK, PC_tree_destroy(&m_conf));
		m_conf = {PC_NODE_NOT_FOUND, nullptr, nullptr};
	}
}

inline PdiTest::~PdiTest()
{
	FinalizePdi();
	std::filesystem::current_path(m_workdir);
	std::filesystem::remove_all(m_tmpdir);
}


} // namespace PDI

#endif
