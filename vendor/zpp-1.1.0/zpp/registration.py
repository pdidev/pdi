##############################################################################
# SPDX-FileCopyrightText: 2014-2024 Centre national de la recherche scientifique (CNRS)
# SPDX-FileCopyrightText: 2014-2024 Commissariat a l'énergie atomique et aux énergies alternatives (CEA)
# SPDX-FileCopyrightText: 2014-2024 Julien Bigot <julien.bigot@cea.fr>
# SPDX-FileCopyrightText: 2014-2024 Université Paris-Saclay
# SPDX-FileCopyrightText: 2014-2024 Université de Versailles Saint-Quentin-en-Yvelines
#
# SPDX-License-Identifier: MIT
##############################################################################

import os

from hashlib import sha1
from optparse import OptionParser
from os.path import  relpath
from pathlib import Path
from shutil import copyfile
from sys import stderr
if os.name == 'nt':
    import winreg

from .version import __version__

zpp_dir = Path(__file__).parent.resolve(True).absolute()


def cmake_user_register(base_dir=None):
    "Register a cmake package in cmake user registry"

    try:
        key_name = str(__version__)+'.'+sha1(bytes(zpp_dir), False).hexdigest()
    except TypeError:
        key_name = str(__version__)+'.'+sha1(bytes(zpp_dir)).hexdigest()

    if os.name == 'nt':
        cmake_registry = winreg.CreateKey(
            winreg.HKEY_CURRENT_USER,
            'Software\\Kitware\\CMake\\Packages\\Zpp')
        winreg.SetValue(cmake_registry, key_name,
                        winreg.REG_SZ, zpp_dir/'cmake')
    elif os.name == 'posix':
        if base_dir is None:
            base_dir = Path.home()
        cmake_registry = base_dir/'.cmake'/'packages'/'Zpp'
        cmake_registry.mkdir(parents=True, exist_ok=True)
        cmake_registry = cmake_registry.resolve(True).absolute()
        with open(cmake_registry/key_name, 'w') as zppfile:
            zppfile.write(str(zpp_dir/'cmake') + "\n")
    else:
        raise "Unsupported system type ("+os.name+")"


def cmake_system_register(base_dir=None):
    "Register a cmake package system-wide"

    if os.name == 'nt':
        key_name = str(__version__)+md5(zpp_dir, False).hexdigest()
        cmake_registry = winreg.CreateKey(
            winreg.HKEY_LOCAL_MACHINE,
            'Software\\Kitware\\CMake\\Packages\\Zpp')
        winreg.SetValue(cmake_registry, key_name,
                        winreg.REG_SZ, str(zpp_dir/'cmake'))
    elif os.name == 'posix':
        if base_dir is None:
            base_dir = Path('/usr')
        share_dir = base_dir/'share'/'Zpp'
        share_dir.mkdir(parents=True, exist_ok=True)
        share_dir = share_dir.resolve(True).absolute()
        copyfile(zpp_dir/'zpp.mk', share_dir/'zpp.mk')

        cmake_dir = share_dir/'cmake'
        cmake_dir.mkdir(parents=True, exist_ok=True)
        cmake_dir = cmake_dir.resolve(True).absolute()
        rel_zpp_dir = Path(relpath(str(zpp_dir), start=str(cmake_dir)))
        for res in (Path.cwd()/'zpp'/'cmake').iterdir():
            dst = cmake_dir/res.name
            with open(res, 'r') as data_in, open(dst, 'w') as data_out:
                for line in data_in:
                    if ('@PYTHON_INSERT_ZPP_EXECUTABLE@' in line):
                        data_out.write(
                            'get_filename_component(ZPP_EXECUTABLE "${_CURRENT_LIST_DIR}/'
                            + str(rel_zpp_dir) + '/zpp" ABSOLUTE)\n')
                    else:
                        data_out.write(line)
    else:
        raise "Unsupported system type ("+os.name+")"


def parse_cmdline():
    parser = OptionParser(
        description="Installs cmake ",
        version=__version__,
        usage="%prog [Options...]\n  use `%prog -h' for more info")
    parser.add_option('-s', '--system',
                      action='store_true',
                      dest='system',
                      help='Install system-wide.',
                      default=True
                      )
    parser.add_option('-u', '--user',
                      action='store_false',
                      dest='system',
                      help='Install for the specific user.'
                      )
    parser.add_option('-b', '--base',
                      action='store',
                      type='string',
                      dest='base',
                      help='Install for the specific user.'
                      )
    (opts, args) = parser.parse_args()
    if len(args) > 0:
        parser.error("no positional argument expected: "+', '.join(args))
        exit(1)

    return (opts.system, None if opts.base is None else Path(opts.base))


def main():
    (system, base) = parse_cmdline()
    if system:
        cmake_system_register(base)
    else:
        cmake_user_register(base)
