#!/usr/bin/env python

################################################################################
# Copyright (c) Julien Bigot - CEA (julien.bigot@cea.fr)
# All rights reserved.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
################################################################################

from os import listdir, makedirs
from os.path import abspath, dirname, expanduser, isdir, join, relpath, samefile
from platform import system
from setuptools import setup
from setuptools.command.develop import develop
from setuptools.command.install import install
from shutil import copyfileobj
from sys import stderr
try:
    from builtins import bytes
except ImportError:
    from __builtin__ import bytes

def install_helpers_method(self):
    print("running install_wrappers")
    
    share_dir = join('share', self.config_vars["dist_name"])
    cmake_dirs = [ join(share_dir, 'cmake'), join('share', 'bpp', 'cmake') ]
    
    prefix = self.install_data
    
    if self.user:
        print("Registering in cmake user registry")
        if system() == 'Linux':
            cmake_registry = abspath(expanduser("~/.cmake/packages/"))
            if (self.root is not None):
              cmake_registry = abspath(abspath(self.root)+cmake_registry)
            if not isdir(cmake_registry):
                makedirs(cmake_registry)
            cmake_registry_file = join(cmake_registry, self.config_vars["dist_fullname"])
            print("Writing cmake registry file: "+cmake_registry_file)
            with open(cmake_registry_file, 'w') as zppfile:
                zppfile.write(join(self.install_userbase, cmake_dirs[0])+"\n")
        else:
            print("Not registering in cmake user registry: unsupported system type ("+system()+")")
    else:
        if isdir(prefix) and samefile(prefix, '/'):
            prefix = join(prefix, 'usr')
    
    share_dir = join(prefix, share_dir)
    cmake_dirs = map( lambda d: join(prefix, d), cmake_dirs)
    
    if not isdir(share_dir):
        makedirs(share_dir)
    for res_name in listdir('zpp'):
        if ( res_name[-3:] == '.mk' ):
            dst = join(share_dir, res_name)
            print('Installing '+res_name+' wrapper to '+share_dir)
            copyfileobj(open(join('zpp', res_name), 'rb'), open(dst, 'wb'))
    
    for cmake_dir in cmake_dirs:
        if not isdir(cmake_dir):
            makedirs(cmake_dir)
        rel_cmake_path = relpath(abspath(self.install_scripts), cmake_dir)
        try:
            rel_cmake_path = bytes(rel_cmake_path, encoding='utf8')
        except:
            rel_cmake_path = bytes(rel_cmake_path)
        for res_name in listdir(join('zpp', 'cmake')):
            dst = join(cmake_dir, res_name)
            print('Installing '+res_name+' wrapper to '+cmake_dir)
            data_in = open(join('zpp', 'cmake', res_name), 'rb')
            with open(dst, 'wb') as data_out:
                for line in data_in:
                    if bytes(b'@PYTHON_INSERT_ZPP_EXECUTABLE@') in line:
                        data_out.write(bytes(b'get_filename_component(ZPP_EXECUTABLE "${_CURRENT_LIST_DIR}/'+rel_cmake_path+b'/zpp" ABSOLUTE)\n'))
                    else:
                        data_out.write(line)
    

class PostDevelopCommand(develop):
    install_helpers = install_helpers_method
    def run(self):
        develop.run(self)
        self.install_helpers()

class PostInstallCommand(install):
    install_helpers = install_helpers_method
    def run(self):
        install.run(self)
        self.install_helpers()

version = {}
with open("zpp/version.py") as fp:
    exec(fp.read(), version)

setup(
    packages = [ 'zpp' ],
    zip_safe = True,
    entry_points = { "console_scripts": [ "zpp = zpp:main", "bpp = zpp:main" ] },
    package_data = { "zpp": ["include/*.zpp.sh"] },
    install_requires = [ 'setuptools' ],
    cmdclass = { 'develop': PostDevelopCommand, 'install': PostInstallCommand },
    
    name = "zpp",
    version = version['__version__'],
    author = "Julien Bigot",
    author_email = "julien.bigot@cea.fr",
    description = "a Bash Pre-Processor for Fortran. ZPP is useful in order to build clean Fortran90 interfaces. It allows to generate Fortran code for all types, kinds, and array ranks supported by the compiler.",
    long_description = open(join(dirname(__file__), 'README.md'), 'r').read(),
    long_description_content_type = "text/markdown",
    license = "MIT",
    keywords = "bash Fortran pre-processor",
    project_urls = {
        "Source Code": "https://github.com/pdidev/zpp/",
    },
    classifiers = [
        "License :: OSI Approved :: MIT License",
        "Development Status :: 5 - Production/Stable",
        "Environment :: Console",
    ],
)
