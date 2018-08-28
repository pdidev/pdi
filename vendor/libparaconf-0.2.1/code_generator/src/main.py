import argparse, yamale
from c_code_generator.c_data_loader import C_DataLoader
from c_code_generator.c_free_memory import c_free_root
from c_code_generator.c_types_generator import C_TypesGenerator
from c_code_generator.c_functions import MAIN_FUNCTION


def _run(schema_path, output_path):
    """Code generator"""

    init_name = 'pcgen_loader'
    type_name = 'types'

    schema = yamale.make_schema(schema_path)

    c_types_header = C_TypesGenerator(schema)
    c_types_header.define_types()
    c_types_header.dump_types_definition(type_name+'.h')

    c_loader = C_DataLoader(schema, init_name, type_name)
    c_loader.gen_init_code()

    c_free_code, c_free_header = c_free_root(schema)
    c_loader.init_code.extend(c_free_code)
    c_loader.init_header.extend(c_free_header)
    c_loader.dump_code()
    c_loader.dump_header()

    if not output_path==None:
        f = open(output_path+'.c', 'w')
        f.write(MAIN_FUNCTION)
        f.close()


def main():
    """Parse the arguments given to PCgen and run the code generator"""

    parser = argparse.ArgumentParser(description='PCgen --- C code generator for ParaConf', prog='pcgen')
    parser.add_argument('schema', metavar='SCHEMA', nargs=1,
                        help='path to the Yamale schema')
    parser.add_argument('-o', '--output', default=None,
                        help='output name for the main function file, by default no file is generated')
    args = parser.parse_args()
    _run(args.schema[0], args.output)


if __name__ == '__main__':
    main()
