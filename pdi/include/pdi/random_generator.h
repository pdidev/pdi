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

#ifndef PDI_RANDOM_GENERATOR_H_
#define PDI_RANDOM_GENERATOR_H_

// this is a header-only file because this file is include in testing.h
// And we don't want to depend on a given version of gtest/gmock for the testing framework

// Have a look at Decl'NetCDF plugin `decl_netcdf_mpi_tests.cxx` for an example of use without the testing framework.

//#include <algorithm>
#include <random>
#include <ranges>
#include <type_traits>

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

} // namespace PDI

#endif
