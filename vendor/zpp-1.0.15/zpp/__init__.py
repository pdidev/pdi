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

""" The Bash PreProcessor
"""

from __future__ import print_function
try:
    from builtins import bytes
except ImportError:
    from __builtin__ import bytes
from fileinput import FileInput
from optparse import OptionParser
from os import listdir, makedirs, symlink, unlink
from os.path import abspath, basename, dirname, exists, expanduser, isdir, join, relpath, samefile
from os.path import splitext
from platform import system
from re import compile
from shutil import copyfileobj, rmtree
from site import getuserbase, getusersitepackages
from subprocess import call
from sys import argv, exit, stderr
from tempfile import NamedTemporaryFile, mkdtemp
from uuid import uuid4

from .version import __version__


def abytes(var):
    try:
        return bytes(var, 'ASCII')
    except TypeError:
        return bytes(var)


EOCAT = b'EOCAT_' + abytes(str(uuid4())).replace(b'-', b'_')
TRIGGER_REGEX = compile(br'^\s*!\$SH\s+')


def parse_cmdline():
    def callback_def(option, opt_str, value, parser):
        (optname, _, valval) = value.partition('=')
        parser.values.defines[optname.strip()] = valval.strip()

    parser = OptionParser(
        description="Preprocesses BASH in-line commands in a source file",
        version=__version__,
        usage="%prog [Options...] <source> [<destination>]\n  use `%prog -h' for more info")
    parser.add_option('-I',
                      action='append',
                      dest='includes',
                      nargs=1,
                      default=list(),
                      metavar="DIR",
                      help='Add DIR to search list for source directives'
                      )
    parser.add_option('-o',
                      dest='output',
                      metavar="FILE",
                      help='Place the preprocessed code in file FILE.'
                      )
    parser.add_option('-D',
                      action='callback',
                      callback=callback_def,
                      dest='defines',
                      type="string",
                      default=dict(),
                      metavar='OPTION=VALUE',
                      help='Set the value of OPTION to VALUE'
                      )
    (opts, args) = parser.parse_args()
    if len(args) < 1 or len(args) > 2:
        parser.print_help()
        exit(1)
    if len(args) > 1 and opts.output is not None:
        parser.error(
            "The -o option and <destination> argument are mutually exclusive")
        exit(1)
    input = abspath(args[0])
    output = None
    if opts.output is not None:
        output = abspath(opts.output)
    elif len(args) > 1:
        output = abspath(args[1])
    else:
        (root, ext) = splitext(input)
        if ext.lower() == '.zpp':
            output = root
        else:
            output = input + '.unzpp'
    return (input, output, opts.includes, opts.defines)


def setup_dir(includes):
    tmpdir = mkdtemp(suffix='', prefix='zpp.tmp.')
    run_in_dir = False
    try:
        from pkg_resources import Requirement, resource_listdir, resource_stream
        from pkg_resources import DistributionNotFound, VersionConflict
        try:
            for res_name in resource_listdir(
                    Requirement.parse('zpp==' + __version__),
                    'zpp/include'):
                copyfileobj(resource_stream(Requirement.parse('zpp==' + __version__),
                                            join('zpp/include', res_name)),
                            open(join(tmpdir, res_name), 'wb'))
                symlink(join(tmpdir, res_name), join(tmpdir, res_name)[:-7] + '.bpp.sh')
        except (DistributionNotFound, VersionConflict):
            run_in_dir = True
    except ImportError:
        run_in_dir = True
    if run_in_dir:
        for res_name in listdir(join(abspath(dirname(__file__)), 'include')):
            copyfileobj(open(join(abspath(dirname(__file__)), 'include',
                                  res_name), 'rb'), open(join(tmpdir, res_name), 'wb'))
            symlink(join(tmpdir, res_name), join(tmpdir, res_name)[:-7] + '.bpp.sh')

    for incdir in includes:
        if isdir(incdir):
            for incfile in listdir(incdir):
                if incfile[-7:] == '.zpp.sh' or incfile[-7:] == '.bpp.sh':
                    src = abspath(join(incdir, incfile))
                    dst = join(tmpdir, basename(incfile))
                    if not exists(dst):
                        symlink(src, abytes(dst[:-7]) + b'.zpp.sh')
                        symlink(src, abytes(dst[:-7]) + b'.bpp.sh')
        else:
            print('Warning: include directory not found: "' +
                  incdir + '"', file=stderr)
    return tmpdir


def handle_file(tmpdir, input, defines, output):
    tmpfile_name = join(tmpdir, basename(input) + '.bash')
    with open(tmpfile_name, 'wb') as tmpfile, open(input, 'rb') as infile:
        inbash = True
        tmpfile.write(b'set -e\n')
        for var in defines:
            tmpfile.write(abytes(var) + b'=' + abytes(defines[var]) + b"\n")
        for line in infile:
            if TRIGGER_REGEX.match(line):
                if not inbash:
                    tmpfile.write(EOCAT + b"\n")
                    inbash = True
                tmpfile.write(TRIGGER_REGEX.sub(b'', line, 1))
            else:
                if inbash:
                    tmpfile.write(b"cat<<" + EOCAT + b"\n")
                    inbash = False
                tmpfile.write(line)
        if not inbash:
            tmpfile.write(EOCAT + b"\n")
        tmpfile.close()

        if output == '-':
            result = call(['bash', '-r', basename(tmpfile.name)], cwd=tmpdir)
        else:
            with open(output, 'w') as outfile:
                result = call(['bash', '-r', basename(tmpfile.name)],
                              stdout=outfile, cwd=tmpdir)


def main():
    result = 0
    (input, output, includes, defines) = parse_cmdline()
    tmpdir = setup_dir(includes)
    try:
        handle_file(tmpdir, input, defines, output)
    except Exception as e:
        print(e, file=stderr)
        result = -1
    rmtree(tmpdir)
    exit(result)
