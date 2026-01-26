##############################################################################
# SPDX-FileCopyrightText: 2014-2024 Centre national de la recherche scientifique (CNRS)
# SPDX-FileCopyrightText: 2014-2024 Commissariat a l'énergie atomique et aux énergies alternatives (CEA)
# SPDX-FileCopyrightText: 2014-2024 Julien Bigot <julien.bigot@cea.fr>
# SPDX-FileCopyrightText: 2014-2024 Université Paris-Saclay
# SPDX-FileCopyrightText: 2014-2024 Université de Versailles Saint-Quentin-en-Yvelines
#
# SPDX-License-Identifier: MIT
##############################################################################

source fortran.zpp.sh

ZPP_HDF5F_TYPES='INTEGER REAL REAL8 CHARACTER'

# Returns the HDF5 type constant associated to the one letter type descriptor $1
zpp_hdf5f_constant()
{
	case "$1" in
	'REAL8')
		echo -n 'H5T_NATIVE_DOUBLE'
		;;
	'REAL'|'CHARACTER'|'INTEGER')
		echo -n "H5T_NATIVE_${1}"
		;;
	esac
}

if [ "x${ZPP_NO_COMPATIBILITY}" = "x" ]
then
HDF5TYPES="${ZPP_HDF5F_TYPES}"
function hdf5_constant() {
	zpp_hdf5f_constant "$@"
}
fi
