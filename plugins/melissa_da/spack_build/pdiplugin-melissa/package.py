# Copyright 2013-2021 Lawrence Livermore National Security, LLC and other
# Spack Project Developers. See the top-level COPYRIGHT file for details.
#
# SPDX-License-Identifier: (Apache-2.0 OR MIT)

# ----------------------------------------------------------------------------
# If you submit this package back to Spack as a pull request,
# please first remove this boilerplate and all FIXME comments.
#
# This is a template package file for Spack.  We've put "FIXME"
# next to all the things you'll want to change. Once you've handled
# them, you can save this file and test your package like this:
#
#     spack install pdiplugin-melissa
#
# You can edit this file again by typing:
#
#     spack edit pdiplugin-melissa
#
# See the Spack documentation for more information on packaging.
# ----------------------------------------------------------------------------

from spack import *


class PdipluginMelissa(CMakePackage):
    """Melissa PDI Plug-in"""

    homepage = "https://pdi.julien-bigot.fr/"
    url = "https://gitlab.maisondelasimulation.fr/pdidev/pdi/-/archive/1.3.1/pdi-1.3.1.tar.bz2"
    git = "https://gitlab.maisondelasimulation.fr/pdidev/pdi.git"

    maintainers = ['jbigot']

    version('develop', branch='master', no_cache=True)
    version('1.4.3',   sha256='b7f049cae9e6fb0ddba3a458e15d6f1578876663c04b18f62b052f9698750974')

    variant('tests', default=False, description='Build tests')

    depends_on('cmake@3.5:',  type=('build'))
    depends_on('melissa')
    depends_on('mpi',         type=('build', 'link', 'run'))
    depends_on('pdi@develop', type=('link', 'run'), when='@develop')
    depends_on('pdi@1.4.3',   type=('link', 'run'), when='@1.4.3')
    depends_on('pkgconfig',   type=('build'))

    root_cmakelists_dir = 'plugins/melissa'

    def cmake_args(self):
        args = [
            '-DINSTALL_PDIPLUGINDIR:PATH={:s}'.format(self.prefix.lib)]
        return args

    def setup_dependent_environment(self, spack_env, run_env, dependent_spec):
        run_env.append_path('PDI_PLUGIN_PATH', self.prefix.lib)
