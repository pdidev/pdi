/*
 * SPDX-FileCopyrightText: 2015-2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <limits>

#include <gtest/gtest.h>

struct PlusOperator {
	static inline constexpr auto repr = "+";
	static inline constexpr auto sign = '+';

	template <class O1, class O2>
	static inline constexpr auto eval(O1&& o1, O2&& o2)
	{
		return o1 + o2;
	}
};

struct MinusOperator {
	static inline constexpr auto repr = "-";
	static inline constexpr auto sign = '-';

	template <class O1, class O2>
	static inline constexpr auto eval(O1&& o1, O2&& o2)
	{
		return o1 - o2;
	}
};

struct MultOperator {
	static inline constexpr auto repr = "*";
	static inline constexpr auto sign = '*';

	template <class O1, class O2>
	static inline constexpr auto eval(O1&& o1, O2&& o2)
	{
		return o1 * o2;
	}
};

struct DivOperator {
	static inline constexpr auto repr = "/";
	static inline constexpr auto sign = '/';

	template <class O1, class O2>
	static inline constexpr auto eval(O1&& o1, O2&& o2)
	{
		return o1 / o2;
	}
};

struct EqualOperator {
	static inline constexpr auto repr = "==";
	static inline constexpr auto sign = '=';

	template <class O1, class O2>
	static inline constexpr auto eval(O1&& o1, O2&& o2)
	{
		return o1 == o2;
	}
};

struct AndOperator {
	static inline constexpr auto repr = "&&";
	static inline constexpr auto sign = '&';

	template <class O1, class O2>
	static inline constexpr auto eval(O1&& o1, O2&& o2)
	{
		return o1 && o2;
	}
};

struct OrOperator {
	static inline constexpr auto repr = "||";
	static inline constexpr auto sign = '|';

	template <class O1, class O2>
	static inline constexpr auto eval(O1&& o1, O2&& o2)
	{
		return o1 || o2;
	}
};

struct GtOperator {
	static inline constexpr auto repr = ">";
	static inline constexpr auto sign = '>';

	template <class O1, class O2>
	static inline constexpr auto eval(O1&& o1, O2&& o2)
	{
		return o1 > o2;
	}
};

struct LtOperator {
	static inline constexpr auto repr = "<";
	static inline constexpr auto sign = '<';

	template <class O1, class O2>
	static inline constexpr auto eval(O1&& o1, O2&& o2)
	{
		return o1 < o2;
	}
};

struct GetOperator {
	static inline constexpr auto repr = ">=";
	static inline constexpr auto sign = ']';

	template <class O1, class O2>
	static inline constexpr auto eval(O1&& o1, O2&& o2)
	{
		return o1 >= o2;
	}
};

struct LetOperator {
	static inline constexpr auto repr = "<=";
	static inline constexpr auto sign = '[';

	template <class O1, class O2>
	static inline constexpr auto eval(O1&& o1, O2&& o2)
	{
		return o1 <= o2;
	}
};

using OperatorTypes
	= ::testing::Types< PlusOperator, MinusOperator, MultOperator, DivOperator, EqualOperator, AndOperator, OrOperator, GtOperator, LtOperator>;


template <class T>
static inline constexpr auto PARSEABLE_VALS = "Error";

// no min, because min/-1 == max+1 (out of range)
template <>
inline constexpr auto PARSEABLE_VALS<long> = {std::numeric_limits<long>::min() + 1, -1l, 1l, std::numeric_limits<long>::max()};

template <>
inline constexpr auto PARSEABLE_VALS<double> = {-1.5, -1.0000001, -.5, 0.1, .5, 1.0000001, 1.5, 500.1};


template <class T>
static inline constexpr auto ALL_VALS = "Error";

template <>
inline constexpr auto ALL_VALS<uint8_t>
	= {static_cast<uint8_t>(std::numeric_limits<uint8_t>::min() + 1),
       static_cast<uint8_t>(-1),
       static_cast<uint8_t>(1),
       std::numeric_limits<uint8_t>::max()};

template <>
inline constexpr auto ALL_VALS<int8_t>
	= {static_cast<int8_t>(std::numeric_limits<int8_t>::min() + 1),
       static_cast<int8_t>(-1),
       static_cast<int8_t>(1),
       std::numeric_limits<int8_t>::max()};

template <>
inline constexpr auto ALL_VALS<uint64_t>
	= {static_cast<uint64_t>(std::numeric_limits<uint64_t>::min() + 1),
       static_cast<uint64_t>(-1),
       static_cast<uint64_t>(1),
       std::numeric_limits<uint64_t>::max()};

template <>
inline constexpr auto ALL_VALS<int64_t>
	= {static_cast<int64_t>(std::numeric_limits<int64_t>::min() + 1),
       static_cast<int64_t>(-1),
       static_cast<int64_t>(1),
       std::numeric_limits<int64_t>::max()};

template <>
inline constexpr auto ALL_VALS<float> = {std::numeric_limits<float>::min(), -1.5f, -1.f, -.5f, .5f, 1.f, 1.5f, std::numeric_limits<float>::max()};

template <>
inline constexpr auto ALL_VALS<double> = {std::numeric_limits<double>::min(), -1.5, -1., -.5, .5, 1., 1.5, std::numeric_limits<double>::max()};
