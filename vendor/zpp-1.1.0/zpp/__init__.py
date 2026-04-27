##############################################################################
# SPDX-FileCopyrightText: 2014-2024 Centre national de la recherche scientifique (CNRS)
# SPDX-FileCopyrightText: 2014-2024 Commissariat a l'énergie atomique et aux énergies alternatives (CEA)
# SPDX-FileCopyrightText: 2014-2024 Julien Bigot <julien.bigot@cea.fr>
# SPDX-FileCopyrightText: 2014-2024 Université Paris-Saclay
# SPDX-FileCopyrightText: 2014-2024 Université de Versailles Saint-Quentin-en-Yvelines
#
# SPDX-License-Identifier: MIT
##############################################################################

""" The Bash PreProcessor
"""

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
from traceback import format_exc
from uuid import uuid4
from .version import __version__


EOCAT = b'EOCAT_' + bytes(str(uuid4()), 'ASCII').replace(b'-', b'_')
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
    parser.add_option('-v', '--verbose',
                      action='store_true',
                      dest='verbose',
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
    return (input, output, opts.includes, opts.defines, opts.verbose)


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
        except (DistributionNotFound, VersionConflict):
            run_in_dir = True
    except ImportError:
        run_in_dir = True
    if run_in_dir:
        for res_name in listdir(join(abspath(dirname(__file__)), 'include')):
            copyfileobj(open(join(abspath(dirname(__file__)), 'include',
                                  res_name), 'rb'), open(join(tmpdir, res_name), 'wb'))

    for incdir in includes:
        if isdir(incdir):
            for incfile in listdir(incdir):
                if incfile[-7:] == '.zpp.sh':
                    src = abspath(join(incdir, incfile))
                    dst = join(tmpdir, basename(incfile))
                    if not exists(dst):
                        symlink(src, dst)
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
            tmpfile.write(var + '=' + defines[var] + "\n")
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
    (input, output, includes, defines, verbose) = parse_cmdline()
    tmpdir = setup_dir(includes)
    try:
        print(str(tmpdir), str(input), str(defines), str(output))
        handle_file(tmpdir, input, defines, output)
    except Exception as e:
        print(e, file=stderr)
        if verbose:
            print(str(e),format_exc(e), file=stderr)
        result = -1
    finally:
        rmtree(tmpdir)
    exit(result)
