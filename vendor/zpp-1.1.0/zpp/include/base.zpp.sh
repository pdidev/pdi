##############################################################################
# SPDX-FileCopyrightText: 2014-2024 Centre national de la recherche scientifique (CNRS)
# SPDX-FileCopyrightText: 2014-2024 Commissariat a l'énergie atomique et aux énergies alternatives (CEA)
# SPDX-FileCopyrightText: 2014-2024 Julien Bigot <julien.bigot@cea.fr>
# SPDX-FileCopyrightText: 2014-2024 Université Paris-Saclay
# SPDX-FileCopyrightText: 2014-2024 Université de Versailles Saint-Quentin-en-Yvelines
#
# SPDX-License-Identifier: MIT
##############################################################################

# Outputs a string multiple times.
#
# Parameters
# 1. the string to Repeat
# 2. the lower bound of the iterations (inclusive)
# 3. the upper bound of the iterations (inclusive)
# 4. the string separator
# 5. the string starter
# 6. the string ender
# 
# Repeats string `$1` (`$3`-`$2`+1) times, separated by string `$4` inside `$5`
# `$6`.
# * If the number of repetitions is negative, the result is empty.
# * If `$1` contains the '@N' substring, it will be replaced by the iteration
#   number (from `$2` to `$3`).
# 
# example:
# ```
# #!SH source base.zpp.sh
# zpp_str_repeat v@N 5 7 '...' '<<' '>>'
# zpp_str_repeat w@N 1 1 '...' '<<' '>>'
# zpp_str_repeat x@N 1 0 '...' '<<' '>>'
# ```
# output:
# ```
# <<v5...v6...v7>>
# <<w1>>
# ```
function zpp_str_repeat() {
  STR="$1"
  FROM="$2"
  TO="$3"
  SEP="$4"
  START="$5"
  END="$6"
  if [ "${TO}" -lt "${FROM}" ]; then return; fi
  RES="${START}${STR//@N/${FROM}}"
  (( ++FROM ))
  for N in $(seq $FROM $TO); do
    RES="${RES}${SEP}${STR//@N/${N}}"
  done
  echo "${RES}${END}"
}


# Outputs a string multiple times in reverse order.
#
# Parameters
# 1. the string to Repeat
# 2. the upper bound of the iterations (inclusive)
# 3. the lower bound of the iterations (inclusive)
# 4. the string separator
# 5. the string starter
# 6. the string ender
# 
# Repeats string `$1` (`$2`-`$3`+1) times, separated by string `$4` inside `$5`
# `$6`.
# * If the number of repetitions is negative, the result is empty.
# * If `$1` contains the '@N' substring, it will be replaced by the iteration
#   number (from `$2` to `$3`, i.e. upper to lower).
# 
# example:
# ```
# #!SH source base.zpp.sh
# zpp_str_repeat_reverse v@N 5 7 '...' '<<' '>>'
# zpp_str_repeat_reverse w@N 1 1 '...' '<<' '>>'
# zpp_str_repeat_reverse x@N 1 0 '...' '<<' '>>'
# ```
# output:
# ```
# <<v7...v6...v5>>
# <<w1>>
# ```
function zpp_str_repeat_reverse() {
  STR="$1"
  FROM="$2"
  TO="$3"
  SEP="$4"
  RES="${STR//@N/${TO}}"
  TO=$(($TO-1))
  for N in $(seq $TO -1 $FROM); do
    RES="${RES}${SEP}${STR//@N/${N}}"
  done
  echo "$RES"
}

if [ "x${ZPP_NO_COMPATIBILITY}" = "x" ]
then
function str_repeat() {
	zpp_str_repeat "$@"
}
function str_repeat_reverse() {
	zpp_str_repeat_reverse "$@"
}
fi
