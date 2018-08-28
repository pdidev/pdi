from c_code_generator.type_handler import *
from c_code_generator.tools import make_flat_tree

INDENT_SPACE = 8
HEADER_STRING = 'PARACONF_C_TYPES_DEFINITION__'


class C_TypesGenerator():

    def __init__(self, schema):
        """Initialize the type generator"""
        
        # Code lines will be represented as tuples:
        #     1st element -> indent level
        #     2nd element -> string corresponding to the line of code
        #     3rd element -> string identifying the variable defined by the line of code
        # These tuples are gathered in sub-lists (blocks) corresponding to their depth level
        self.code = []
        self.schema = schema


    def define_types(self):
        """Generate the code representation beginning from the last line"""

        self.code.append([(0, '#endif', None)])

        # We generate the code lines corresponding to the main nodes of the YAML schema
        #  * typedef_list stores all the struct types to declare later
        typedef_list = self._make_root_struct()

        # We define the included types while updating typedef_list
        self._make_includes(typedef_list)

        # Declaration of the kind: typedef struct var_s var_t;
        self._make_typedef(typedef_list)

        # We geenrate the header
        self._make_header()


    def _make_root_struct(self):
        """Generate the code lines corresponding to the nodes of the main YAML document"""

        indent_level = 0
        typedef_list = ['root']
        self.code.append([])
        self.code[-1].append((indent_level, 'struct root_s {', 'root'))

        # We make a flat tree (dict) where:
        #     * the keys correspond to the main nodes (root nodes) of the schema
        #     * the value associated to a key is a list of paths to the leaves:
        #          / root_node_1: [paths to leaves_1]
        #  .{root} - root_node_2: [paths to leaves_2]
        #          \ root_node_n: [paths to leaves_n]
        root_nodes = make_flat_tree(self.schema._schema.keys())

        # We extract and sort the keys of root_nodes
        root_key_list = [k for k in root_nodes.keys()]
        root_key_list.sort()

        # We generate the code line(s) for each root node
        for key in root_key_list:

            if key=='generic':
                raise ValueError('found an node with name "generic" at root')

            # If the node is a primitive type
            if root_nodes[key] is None:
                
                # We handle the primitive type and generate the code line corresponding to it
                type = Type_Handler(self.schema._schema[key])
                self.code[-1].extend(type.c_declare(key, indent_level+1, 'root', path='root.'+key))

            # If the node is not a primitive type, it is defined using a nested struct
            else:
                self._make_nested_struct(key, root_nodes[key], 1)

        # The Yamale parser validates keys that are not in the schema, so we create a generic type for them
        generic_node = Type_Handler(None)
        self.code[-1].extend(generic_node.c_declare_generic(indent_level+1, 'root'))

        self.code[-1].append((indent_level, '};', 'root'))
        self._insert_space('root', n=2)

        return typedef_list


    def _make_nested_struct(self, path_to_current_node, path_list, indent_level):
        """Create code lines for a nested struct"""

        # * path_to_current_node stores the path to the current node
        # * path_list stores the paths of all current node's dependencies

        self.code[-1].append((indent_level, 'struct {', 'root'))

        # We make a flat tree (dict) where:
        #     * the keys correspond to the current node's children dependencies
        #     * the value associated to a child dependency is a list of relative paths to its own leaves:
        #                  / child_dependency_1: [paths to leaves_1]
        #  .{current_node} - child_dependency_2: [paths to leaves_2]
        #                  \ child_dependency_n: [paths to leaves_n]
        struct_nodes = make_flat_tree(path_list)

        # We extract and sort the keys of struct_nodes
        struct_key_list = [k for k in struct_nodes.keys()]
        struct_key_list.sort()

        # We generate the code lines for each direct dependency of the struct definition
        for key in struct_key_list:

            if key=='generic':
                raise ValueError('found an node with name "generic" at root.%s' % (path_to_current_node))

            # If the node is a primitive type
            if struct_nodes[key] is None:

                # We handle the primitive type and generate the code line corresponding to it
                type = Type_Handler(self.schema._schema[path_to_current_node+'.'+key])
                self.code[-1].extend(type.c_declare(key, indent_level+1, key, path='root.'+path_to_current_node+'.'+key))

            # If the node is not a primitive type, it is defined using a nested struct
            else:
                self._make_nested_struct(path_to_current_node+'.'+key, struct_nodes[key], indent_level+1)

        # The Yamale parser validates keys that are not in the schema, so we create a generic type for them
        generic_node = Type_Handler(None)
        self.code[-1].extend(generic_node.c_declare_generic(indent_level+1, None))
        
        self.code[-1].append((indent_level, '} ' + replace_chars(path_to_current_node.split('.')[-1]) + ';', 'root'))


    def _make_includes(self, typedef_list, included_types=[], indent_level=0):
        """Create all expressions corresponding to included types from a same depth level (block)"""

        # We append a new sub-list to the code corresponding to the new block
        self.code.append([])

        # The first time _make_includes() is called, we must define all the nodes that are in the sub-schemas
        if len(included_types) == 0:
            sorted_includes_nodes = [k for k in self.schema.includes.keys()]
        # If _make_includes() is recursively called, the included_types list is not empty and we redefine its nodes in a new block
        else:
            sorted_includes_nodes = [k for k in included_types[:]]
        sorted_includes_nodes.sort()

        # The included_types list can now be freed to store new dependencies
        included_types = []

        # We define each included node
        for node in sorted_includes_nodes:

            if node == 'root':
                raise ValueError('found an included node with name "root"')

            # We search if the included node has already been defined and we store the depth level of the block(s) containing the definition
            block_depth_level = []
            for i in range(2, len(self.code)-1):
                # We gather all included nodes defined at the ith depth level
                already_defined_nodes = []
                for code_line in self.code[i]:
                    if not code_line[2] in already_defined_nodes:
                        already_defined_nodes.append(code_line[2])
                # We check if the type we want to define already is
                if node in already_defined_nodes:
                    block_depth_level.append(i)

            # If an included type has already been defined, we remove its definition from the previous block(s)
            for i in block_depth_level:
                # We calculate the line number from which the type_name is defined
                line_number = 0
                while node != self.code[i][line_number][2]:
                    line_number += 1
                # We suppress all the lines that correspond to the definition of node
                while node == self.code[i][line_number][2]:
                    self.code[i].pop(line_number)
                    if (len(self.code[i]) == line_number):
                        break

            # We extract, sort and define the sub-nodes of the included node
            type_list = [k for k in self.schema.includes[node]._schema.keys()]
            type_list.sort()
            self.code[-1].append((indent_level, 'struct ' + replace_chars(node) + '_s {', node))
            for key in type_list:
                if key=='generic':
                    raise ValueError('found an included node with name "root" at %s.%s' % (node, key))
                type = Type_Handler(self.schema.includes[node]._schema[key])
                self.code[-1].extend(type.c_declare(key, indent_level+1, node, path=node+'.'+key))

                # We test if we have to later define an included type
                if isinstance(self.schema.includes[node]._schema[key], Include) and not self.schema.includes[node]._schema[key].args[0] in included_types and not type.pointer_order+int(type.is_optional)>0:
                    included_types.append(self.schema.includes[node]._schema[key].args[0])

            # The Yamale parser validates keys that are not in the schema, so we create a generic type for them
            generic_node = Type_Handler(None)
            self.code[-1].extend(generic_node.c_declare_generic(indent_level+1, node))

            self.code[-1].append((indent_level, '};', node))
            self._insert_space(node)

            if not node in typedef_list:
                typedef_list.append(node)

        # If there are some new dependencies, we move them to a new block (depth level +1)
        if len(included_types) > 0:
            self._make_includes(typedef_list, included_types)


    def _make_typedef(self, typedef_list=[]):
        """Create the typedef expressions for struct types"""

        self.code.append([])
        for element in typedef_list:
            self.code[-1].append((0, 'typedef struct ' + replace_chars(element) + '_s ' + replace_chars(element) + '_t;', None))
        self._insert_space(None, n=2)


    def _make_header(self):
        """Generate lines corresponding to the header"""

        header = []
        header.append((0, '#ifndef {}'.format(HEADER_STRING), None))
        header.append((0, '#define {}'.format(HEADER_STRING), None))
        self.code.append(header)
        self._insert_space(None)
        self.code[-1].append((0, "#include <paraconf.h>", None))    
        self._insert_space(None, n=2)

        
    def _insert_space(self, defined_key, n=1):
        """Insert n space(s) at the last block"""

        while n > 0:
            self.code[-1].append((0, '', defined_key))
            n += -1


    def dump_types_definition(self, output='types.h'):
        """Write the code lines in the output file"""

        f = open(output, "w")
        for expression in self.code[::-1]:
            for line in expression:
                indent = ''
                for i in range(line[0] * INDENT_SPACE):
                    indent += ' '
                f.write(indent + line[1] + '\n')
