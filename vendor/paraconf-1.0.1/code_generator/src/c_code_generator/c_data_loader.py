from yamale.validators import *
from c_code_generator.type_handler import *
from c_code_generator.tools import convert_enum_to_any, make_flat_tree, replace_chars
from c_code_generator.c_free_memory import c_free_memory, has_allocated_member

INDENT_SPACE = 8
INIT_HEADER = 'PARACONF_DATA_LOADER_H__'


class C_DataLoader():

    def __init__(self, schema, init_name='pcgen_loader', type_name='types'):
        """Initialize the data loader"""

        self.schema = schema
        self.init_name = init_name  # Name of the init functions file
        self.type_name = type_name  # Name of the types' definition file

        # Lines of code will be represented as tuples:
        #     1st element -> indent level
        #     2nd element -> string corresponding to the line of code

        self.init_code = []         # Initialization functions
        self.init_header = []       # Initialization functions' header
        self.free_code = []         # Deallocation functions
        self.free_header = []       # Deallocation functions' header


    def gen_init_code(self):
        """Generate the initialization code/header"""

        # Beginning of the C init header
        self.gen_init_header()

        # Beginning of the C init code
        self.init_code.append((0, '#include <paraconf.h>'))
        self.init_code.append((0, '#include "%s.h"' % (self.init_name)))
        self._insert_space_init(n=2)
        self.init_code.append((0, LOAD_BOOL_DEFINITION))
        self._insert_space_init(n=2)
        self.init_code.append((0, LOAD_DOUBLE_DEFINITION))
        self._insert_space_init(n=2)
        self.init_code.append((0, LOAD_INT_DEFINITION))
        self._insert_space_init(n=2)
        self.init_code.append((0, LOAD_STRING_DEFINITION))
        self._insert_space_init(n=2)

        # load_root() function
        self.gen_init_root_code()

        # load_<included_type>() functions
        self.gen_init_includes()

        # load_root_<path_to_node>() functions
        root_keys = make_flat_tree(self.schema._schema.keys())
        self.iter_dependencies(root_keys, previous_path='', previous_var='root->', path_to_enum=['root'])


    def gen_init_header(self):
        """Generate the beginning of the initialization header"""

        self.init_header.append((0, '#ifndef %s' % (INIT_HEADER)))
        self.init_header.append((0, '#define %s' % (INIT_HEADER)))
        self.init_header.append((0, ''))
        self.init_header.append((0, '#include "%s.h"' % (self.type_name)))
        self.init_header.append((0, ''))
        self.init_header.append((0, ''))
        self.init_header.append((0, LOAD_BOOL_DECLARATION))
        self.init_header.append((0, LOAD_DOUBLE_DECLARATION))
        self.init_header.append((0, LOAD_INT_DECLARATION))
        self.init_header.append((0, LOAD_STRING_DECLARATION))
        self.init_header.append((0, ''))
        self.init_header.append((0, ''))
        self.init_header.append((0, 'PC_status_t load_root(PC_tree_t tree, root_t* root);'))
        self.init_header.append((0, ''))


    def gen_init_root_code(self):
        """Generate the load_root() function"""

        indent_level = 0

        # We get all the main schema's keys
        root_keys = []
        for key in self.schema._schema.keys():
            key = key.split('.')[0]
            if not key in root_keys:
                root_keys.append(key)

        # We generate all the lines of code
        self.init_code.append((indent_level, 'PC_status_t load_root(PC_tree_t tree, root_t* root) {'))

        self._insert_space_init()

        self.init_code.append((indent_level+1, 'int root_len;'))
        self.init_code.append((indent_level+1, 'PC_status_t status = PC_len(tree, &root_len);'))
        self.init_code.append((indent_level+1, 'if (status) {'))
        self.init_code.append((indent_level+2, 'goto err;'))
        self.init_code.append((indent_level+1, '}'))

        self._insert_space_init()

        self.init_code.append((indent_level+1, 'for(int i = 0 ; i < root_len ; ++i) {'))

        self.init_code.append((indent_level+2, 'char* node_name = NULL;'))
        self.init_code.append((indent_level+2, 'status = PC_string(PC_get(tree, "{%d}", i), &node_name);'))
        self.init_code.append((indent_level+2, 'if (status) {'))
        self.init_code.append((indent_level+3, 'goto err;'))
        self.init_code.append((indent_level+2, '}'))

        key = root_keys[0]
        self.init_code.append((indent_level+2, 'if (!strcmp(node_name, "%s")) {' % (key)))
        self.init_code.append((indent_level+3, 'status = load_root_%s(tree, root);' % (replace_chars(key))))
        self.init_code.append((indent_level+2, '}'))
        for key in root_keys[1:]:
            self.init_code.append((indent_level+2, 'else if (!strcmp(node_name, "%s")) {' % (key)))
            self.init_code.append((indent_level+3, 'status = load_root_%s(tree, root);' % (replace_chars(key))))
            self.init_code.append((indent_level+2, '}'))

        self.init_code.append((indent_level+2, 'else {'))
        self.init_code.append((indent_level+3, 'root->generic.node = realloc(root->generic.node, ++root->generic.len * sizeof(PC_tree_t));'))
        self.init_code.append((indent_level+3, 'root->generic.node[root->generic.len-1] = PC_get(tree, ".%s", node_name);'))
        self.init_code.append((indent_level+3, 'status = PC_status(root->generic.node[root->generic.len-1]);'))
        self.init_code.append((indent_level+2, '}'))

        self.init_code.append((indent_level+2, 'free(node_name);'))
        self.init_code.append((indent_level+2, 'if (status) {'))
        self.init_code.append((indent_level+3, 'goto err;'))
        self.init_code.append((indent_level+2, '}'))
        self.init_code.append((indent_level+1, '}'))

        self._insert_space_init()
        self.init_code.append((indent_level+1, 'return PC_OK;'))

        self._insert_space_init()
        self.init_code.append((indent_level, 'err:'))
        self.init_code.append((indent_level+1, 'free_root(root);'))
        self.init_code.append((indent_level+1, 'return status;'))
        self.init_code.append((indent_level, '}'))


    def gen_init_includes(self):
        """Generate the load_<included_node>() and load_<included_node>_<sub_node_n>() functions"""

        indent_level = 0

        # We get all the included keys
        included_keys = [k for k in self.schema.includes.keys()]
        included_keys.sort()

        # Each included key has its init function
        for included_key in included_keys:

            self.init_header.append((indent_level, 'PC_status_t load_%s(PC_tree_t tree, %s_t* %s);' % (replace_chars(included_key), replace_chars(included_key), replace_chars(included_key))))

            # We generate all the lines of code
            self._insert_space_init(n=2)
            self.init_code.append((indent_level, 'PC_status_t load_%s(PC_tree_t tree, %s_t* %s) {' % (replace_chars(included_key), replace_chars(included_key), replace_chars(included_key))))

            self._insert_space_init()

            self.init_code.append((indent_level+1, 'int %s_len;' % (replace_chars(included_key))))
            self.init_code.append((indent_level+1, 'PC_status_t status = PC_len(tree, &%s_len);' % (replace_chars(included_key))))
            self.init_code.append((indent_level+1, 'if (status) {'))
            self.init_code.append((indent_level+2, 'goto err;'))
            self.init_code.append((indent_level+1, '}'))

            self._insert_space_init()

            self.init_code.append((indent_level+1, 'for(int i = 0 ; i < %s_len ; ++i) {' % (replace_chars(included_key))))

            self.init_code.append((indent_level+2, 'char* node_name = NULL;'))
            self.init_code.append((indent_level+2, 'status = PC_string(PC_get(tree, "{%d}", i), &node_name);'))
            self.init_code.append((indent_level+2, 'if (status) {'))
            self.init_code.append((indent_level+3, 'goto err;'))
            self.init_code.append((indent_level+2, '}'))

            key = [k for k in self.schema.includes[included_key]._schema.keys()][0]
            self.init_code.append((indent_level+2, 'if (!strcmp(node_name, "%s")) {' % (key)))
            self.init_code.append((indent_level+3, 'status = load_%s_%s(tree, %s);' % (replace_chars(included_key), replace_chars(key), replace_chars(included_key))))
            self.init_code.append((indent_level+2, '}'))
            for key in [k for k in self.schema.includes[included_key]._schema.keys()][1:]:
                self.init_code.append((indent_level+2, 'else if (!strcmp(node_name, "%s")) {' % (key)))
                self.init_code.append((indent_level+3, 'status = load_%s_%s(tree, %s);' % (replace_chars(included_key), replace_chars(key), replace_chars(included_key))))
                self.init_code.append((indent_level+2, '}'))

            self.init_code.append((indent_level+2, 'else {'))
            self.init_code.append((indent_level+3, '%s->generic.node = realloc(%s->generic.node, ++%s->generic.len * sizeof(PC_tree_t));' % (replace_chars(included_key), replace_chars(included_key), replace_chars(included_key))))
            self.init_code.append((indent_level+3, '%s->generic.node[%s->generic.len-1] = PC_get(tree, ".%%s", node_name);' % (replace_chars(included_key), replace_chars(included_key))))
            self.init_code.append((indent_level+3, 'status = PC_status(%s->generic.node[%s->generic.len-1]);' % (replace_chars(included_key), replace_chars(included_key))))
            self.init_code.append((indent_level+2, '}'))

            self.init_code.append((indent_level+2, 'free(node_name);'))
            self.init_code.append((indent_level+2, 'if (status) {'))
            self.init_code.append((indent_level+3, 'goto err;'))
            self.init_code.append((indent_level+2, '}'))
            self.init_code.append((indent_level+1, '}'))

            self._insert_space_init()
            self.init_code.append((indent_level+1, 'return PC_OK;'))

            self._insert_space_init()
            self.init_code.append((indent_level, 'err:'))
            self.init_code.append((indent_level+1, 'free_%s(%s);' % (replace_chars(included_key), replace_chars(included_key))))
            self.init_code.append((indent_level+1, 'return status;'))
            self.init_code.append((indent_level, '}'))

            # We iterate over the included node's dependencies to generate the sub-init functions
            keys = make_flat_tree(self.schema.includes[included_key]._schema.keys())
            self.iter_dependencies(keys, '', replace_chars(included_key)+'->', [replace_chars(included_key)], included_key)

            self.init_header.append((0, ''))


    def iter_dependencies(self, dependency_tree, previous_path, previous_var, path_to_enum, included_key=None):
        """Go through all the dependencies of a node and create the corresponding init functions"""

        # -> dependency_tree is a flat tree where:
        #     * the keys correspond to the current node's children dependencies
        #     * the value associated to a child dependency is a list of relative paths
        #       to its own leaves:
        #                  / child_dependency_1: [paths to leaves_1]
        #  .{current_node} - child_dependency_2: [paths to leaves_2]
        #                  \ child_dependency_n: [paths to leaves_n]
        # -> previous_path is the path to the current node
        # -> previous_var is the C string corresponding to the current variable
        # -> path_to_enum is a list allowing to recreate an enum name
        #    (for ex.: ['ROOT', 'NODE1', 'SUB_NODE1', ...])
        # If included_key is None, we iterate over the root's dependencies, else, over
        # the "included_key's" dependencies

        # We declare/define the load function corresponding to each current node's child
        for key, path_to_dependencies in dependency_tree.items():

            # We refresh the information relative to the children dependencies
            current_tree = make_flat_tree(path_to_dependencies) # Returns None if there are no more dependencies
            current_path = previous_path+key
            current_var = previous_var+replace_chars(key)
            path_to_enum.append(replace_chars(key))

            if included_key==None:
                # Declaration of load_root_<path_to_dependency>()
                self.init_header.append((0, 'PC_status_t load_root_%s(PC_tree_t tree, root_t* root);' % (replace_chars(previous_path+key))))
            else:
                # Declaration of load_<included_key>_<path_to_dependency>()
                self.init_header.append((0, 'PC_status_t load_%s_%s(PC_tree_t tree, %s_t* %s);' % (replace_chars(included_key), replace_chars(previous_path+key), replace_chars(included_key), replace_chars(included_key))))

            # We create the lines of code
            self.gen_init_node_code(current_tree, current_path, current_var, path_to_enum, included_key)

            # If there is at least one dependency we iterate recursively
            if not current_tree==None:
                self.iter_dependencies(current_tree, current_path+'.', current_var+'.', path_to_enum, included_key)

            if previous_path=='' and included_key==None:
                self.init_header.append((0, ''))

            path_to_enum.pop()


    def gen_init_node_code(self, dependency_tree, position, c_variable, path_to_enum, included_key=None):
        """Generate the load_root_<path_to_node>() and load_<included_key>_<path_to_node>() functions"""

        # Generate the initialization function for a given node
        # * position gives the path to the current node in the schema
        # * c_variable gives the current variable's name (root->PATH_TO_VAR or
        #   <included_key>->PATH_TO_VAR)
        # * path_to_enum will allow to recreate the enum type names

        indent_level = 0
        self._insert_space_init(n=2)

        if included_key==None:
            # load_root_<PATH_TO_VAR>() function
            self.init_code.append((indent_level, 'PC_status_t load_root_%s(PC_tree_t tree, root_t* root) {' % (replace_chars(position))))
        else:
            # load_<included_key>_<PATH_TO_VAR>() function
            self.init_code.append((indent_level, 'PC_status_t load_%s_%s(PC_tree_t tree, %s_t* %s) {' % (replace_chars(included_key), replace_chars(position), replace_chars(included_key), replace_chars(included_key))))
            self._insert_space_init()

        if dependency_tree==None:
            # No dependency, the node corresponds to a primitive type (always the case for the included types' sub-nodes)

            if included_key==None:
                validator = self.schema._schema[position]
            else:
                validator = self.schema.includes[included_key]._schema[position]
            self.load_primitive_data(validator, position, c_variable, path_to_enum, indent_level+1)

        else:
            # Else there is at least one dependency

            current_keys = [k for k in dependency_tree.keys()]

            # We generate the lines of code that will load all the sub-nodes
            self.init_code.append((indent_level+1, 'int root_%s_len;' % (replace_chars(position))))
            self.init_code.append((indent_level+1, 'PC_status_t status = PC_len(PC_get(tree, ".%s"), &root_%s_len);' % (position, replace_chars(position))))
            self.init_code.append((indent_level+1, 'if (status) {'))
            self.init_code.append((indent_level+2, 'goto err;'))
            self.init_code.append((indent_level+1, '}'))

            self._insert_space_init()

            self.init_code.append((indent_level+1, 'for(int i = 0 ; i < root_%s_len ; ++i) {' % (replace_chars(position))))

            self.init_code.append((indent_level+2, 'char* node_name = NULL;'))
            self.init_code.append((indent_level+2, 'status = PC_string(PC_get(tree, ".%s{%%d}", i), &node_name);' % (position)))
            self.init_code.append((indent_level+2, 'if (status) {'))
            self.init_code.append((indent_level+3, 'goto err;'))
            self.init_code.append((indent_level+2, '}'))

            key = current_keys[0]
            self.init_code.append((indent_level+2, 'if (!strcmp(node_name, "%s")) {' % (key)))
            self.init_code.append((indent_level+3, 'status = load_root_%s_%s(tree, root);' % (replace_chars(position), replace_chars(key))))
            self.init_code.append((indent_level+2, '}'))
            for key in current_keys[1:]:
                self.init_code.append((indent_level+2, 'else if (!strcmp(node_name, "%s")) {' % (key)))
                self.init_code.append((indent_level+3, 'status = load_root_%s_%s(tree, root);' % (replace_chars(position), replace_chars(key))))
                self.init_code.append((indent_level+2, '}'))

            self.init_code.append((indent_level+2, 'else {'))
            self.init_code.append((indent_level+3, '%s.generic.node = realloc(%s.generic.node, ++%s.generic.len * sizeof(PC_tree_t));' % (c_variable, c_variable, c_variable)))
            self.init_code.append((indent_level+3, '%s.generic.node[%s.generic.len-1] = PC_get(tree, ".%s.%%s", node_name);' % (c_variable, c_variable, position)))
            self.init_code.append((indent_level+3, 'status = PC_status(%s.generic.node[%s.generic.len-1]);' % (c_variable, c_variable)))
            self.init_code.append((indent_level+2, '}'))

            self.init_code.append((indent_level+2, 'free(node_name);'))
            self.init_code.append((indent_level+2, 'if (status) {'))
            self.init_code.append((indent_level+3, 'goto err;'))
            self.init_code.append((indent_level+2, '}'))
            self.init_code.append((indent_level+1, '}'))

            self.init_code.append((indent_level+1, 'return PC_OK;'))

            self.init_code.append((indent_level, 'err:'))
            for key in dependency_tree.keys():
                self.init_code.append((indent_level+1, 'if (NULL != %s.generic.node) {' % (c_variable)))
                self.init_code.append((indent_level+2, 'free(%s.generic.node);' % (c_variable)))
                self.init_code.append((indent_level+1, '}'))
                if dependency_tree[key]==None:
                    if has_allocated_member(self.schema._schema[position+'.'+key]):
                        self.init_code.append((indent_level+1, 'free_root_%s(root);' % (replace_chars(position+'_'+replace_chars(key)))))
                else:
                    for path_to_leaf in dependency_tree[key]:
                        if has_allocated_member(self.schema._schema[position+'.'+key+'.'+path_to_leaf]):
                            self.init_code.append((indent_level+1, 'free_root_%s(root);' % (replace_chars(position+'_'+key+'_'+path_to_leaf))))
            self.init_code.append((indent_level+1, 'return status;'))

        self.init_code.append((indent_level, '}'))


    def load_primitive_data(self, validator, position, c_variable, path_to_enum, indent_level, indices=[], recursion_depth=0):
        """Load data of primitive type (any, bool, include, int, enum, list, map, num, str)"""

        # * indices is a list of current indices allowing to create a format string completing
        #   the position (for ex.: position="{%s}", indices=["i0"] and format string=', "i0"')
        # * recursion_depth will allow us to know:
        #     - if we have to declare the status variable
        #     - if we have to return a result
        #     - how to name new indices

        # If the type is not a pointer we don't have to allocate the variable
        if not validator.is_optional:

            if isinstance(validator, Any):
                validator.validators = find_nested_any(validator.validators)
                enum_names, _ = make_union_names(validator.validators)
                self.load_union_item(validator, position, c_variable, path_to_enum, enum_names, indent_level, indices, recursion_depth=recursion_depth)
                if recursion_depth==0:
                    self.init_code.append((indent_level, 'return status;'))
            elif isinstance(validator, Enum):
                new_validator = convert_enum_to_any(validator)
                enum_names, _ = make_union_names(new_validator.validators)
                self.load_union_item(new_validator, position, c_variable, path_to_enum, enum_names, indent_level, indices, recursion_depth=recursion_depth)
                if recursion_depth==0:
                    self.init_code.append((indent_level, 'return status;'))
            elif isinstance(validator, (List, Map)):
                self.load_map_list(validator, position, c_variable, path_to_enum, indent_level, indices, recursion_depth=recursion_depth)
                if recursion_depth==0:
                    self.init_code.append((indent_level, 'return status;'))
            elif isinstance(validator, Include):
                if recursion_depth==0:
                    self.init_code.append((indent_level, 'return load_%s(PC_get(tree, ".%s"%s), &(%s));' % (replace_chars(validator.args[0]), position, format_string(indices), c_variable)))
                else:
                    self.init_code.append((indent_level, 'status = load_%s(PC_get(tree, ".%s"%s), &(%s));' % (replace_chars(validator.args[0]), position, format_string(indices), c_variable)))
            else:
                type = Type_Handler(validator)
                if recursion_depth==0:
                    self.init_code.append((indent_level, 'return %s;' % (type.c_load(position, c_variable, indices))))
                else:
                    self.init_code.append((indent_level, 'status = %s;' % (type.c_load(position, c_variable, indices))))

        # Else we must allocate the variable
        else:
            if isinstance(validator, Any):
                validator.validators = find_nested_any(validator.validators)
                enum_names, _ = make_union_names(validator.validators)
                self.init_code.append((indent_level, '%s = calloc(1, sizeof(*(%s)));' % (c_variable, c_variable)))
                self.load_union_item(validator, position, c_variable, path_to_enum, enum_names, indent_level, indices, recursion_depth=recursion_depth)
                if recursion_depth==0:
                    self.init_code.append((indent_level, 'return status;'))
            elif isinstance(validator, Enum):
                new_validator = convert_enum_to_any(validator)
                new_validator.is_required = False
                enum_names, _ = make_union_names(new_validator.validators)
                self.init_code.append((indent_level, '%s = calloc(1, sizeof(*(%s)));' % (c_variable, c_variable)))
                self.load_union_item(new_validator, position, c_variable, path_to_enum, enum_names, indent_level, indices, recursion_depth=recursion_depth)
                if recursion_depth==0:
                    self.init_code.append((indent_level, 'return status;'))
            elif isinstance(validator, List):
                self.init_code.append((indent_level, '%s = calloc(1, sizeof(*(%s)));' % (c_variable, c_variable)))
                self.load_map_list(validator, position, c_variable, path_to_enum, indent_level, indices, recursion_depth=recursion_depth)
                if recursion_depth==0:
                    self.init_code.append((indent_level, 'return status;'))
            elif isinstance(validator, Map):
                self.init_code.append((indent_level, '%s = calloc(1, sizeof(*(%s)));' % (c_variable, c_variable)))
                self.load_map_list(validator, position, c_variable, path_to_enum, indent_level, indices, recursion_depth=recursion_depth)
                if recursion_depth==0:
                    self.init_code.append((indent_level, 'return status;'))
            elif isinstance(validator, Include):
                self.init_code.append((indent_level, '%s = calloc(1, sizeof(%s_t));' % (c_variable, replace_chars(validator.args[0]))))
                if recursion_depth==0:
                    self.init_code.append((indent_level, 'return load_%s(PC_get(tree, ".%s"%s), %s);' % (replace_chars(validator.args[0]), position, format_string(indices), c_variable)))
                else:
                    self.init_code.append((indent_level, 'status = load_%s(PC_get(tree, ".%s"%s), %s);' % (replace_chars(validator.args[0]), position, format_string(indices), c_variable)))
            elif isinstance(validator, Boolean):
                self.init_code.append((indent_level, '%s = calloc(1, sizeof(int));' % (c_variable)))
                type = Type_Handler(validator)
                if recursion_depth==0:
                    self.init_code.append((indent_level, 'return %s;' % (type.c_load(position, c_variable, indices))))
                else:
                    self.init_code.append((indent_level, 'status = %s;' % (type.c_load(position, c_variable, indices))))
            elif isinstance(validator, Integer):
                self.init_code.append((indent_level, '%s = calloc(1, sizeof(long));' % (c_variable)))
                type = Type_Handler(validator)
                if recursion_depth==0:
                    self.init_code.append((indent_level, 'return %s;' % (type.c_load(position, c_variable, indices))))
                else:
                    self.init_code.append((indent_level, 'status = %s;' % (type.c_load(position, c_variable, indices))))
            elif isinstance(validator, Number):
                self.init_code.append((indent_level, '%s = calloc(1, sizeof(double));' % (c_variable)))
                type = Type_Handler(validator)
                if recursion_depth==0:
                    self.init_code.append((indent_level, 'return %s;' % (type.c_load(position, c_variable, indices))))
                else:
                    self.init_code.append((indent_level, 'status = %s;' % (type.c_load(position, c_variable, indices))))
            elif isinstance(validator, String):
                type = Type_Handler(validator)
                if recursion_depth==0:
                    self.init_code.append((indent_level, 'return %s;' % (type.c_load(position, c_variable, indices))))
                else:
                    self.init_code.append((indent_level, 'status = %s;' % (type.c_load(position, c_variable, indices))))


    def load_map_list(self, validator, position, c_variable, path_to_enum, indent_level, indices=[], recursion_depth=0):
        """Load a list or a map"""

        if isinstance(validator, List):
            item_name = 'tab'
        else:
            item_name = 'map'

        if validator.is_optional:
            struct_ref = '->'
        else:
            struct_ref = '.'

        if recursion_depth==0: # We have to declare the status
            self.init_code.append((indent_level, 'PC_status_t status = PC_len(PC_get(tree, ".%s"), &(%s%slen));' % (position, c_variable, struct_ref)))
            if len(validator.validators)>0: # We initialize errh only if the list/map has at least one validator
                self.init_code.append((indent_level, 'PC_errhandler_t errh;'))
        else: # We don't have to declare the status
            self.init_code.append((indent_level, 'status = PC_len(PC_get(tree, ".%s"%s), &(%s%slen));' % (position, format_string(indices), c_variable, struct_ref)))

        # We allocate the list/map array and write the "for" loop while introducing a new indice
        self.init_code.append((indent_level, '%s%s%s = calloc(%s%slen, sizeof(*(%s%s%s)));' % (c_variable, struct_ref, item_name, c_variable, struct_ref, c_variable, struct_ref, item_name)))
        self.init_code.append((indent_level, 'for (int i%d = 0 ; i%d < %s%slen ; ++i%d) {' % (recursion_depth, recursion_depth, c_variable, struct_ref, recursion_depth)))
        indices.append('i%d' %(recursion_depth))

        if len(validator.validators)==0:
            if isinstance(validator, List):
                self.init_code.append((indent_level+1, '%s%stab[i%d].item.node = PC_get(tree, ".%s[%%d]"%s);' % (c_variable, struct_ref, recursion_depth, position, format_string(indices))))
                self.init_code.append((indent_level+1, 'status = PC_status(%s%stab[i%d].item.node);' % (c_variable, struct_ref, recursion_depth)))
                self.init_code.append((indent_level+1, 'if (status) {'))
                self.init_code.extend(c_free_memory(self.schema, validator, c_variable, path_to_enum, indent_level+1, indices, recursion_depth))
                self.init_code.append((indent_level+2, 'return status;'))
                self.init_code.append((indent_level+1, '}'))
            else:
                self.init_code.append((indent_level+1, 'status = PC_string(PC_get(tree, ".%s{%%d}"%s), &(%s%smap[i%d].key));' % (position, format_string(indices), c_variable, struct_ref, recursion_depth)))
                self.init_code.append((indent_level+1, '%s%smap[i%d].item.node = PC_get(tree, ".%s<%%d>"%s);' % (c_variable, struct_ref, recursion_depth, position, format_string(indices))))
        else:
            validator.validators = find_nested_any(validator.validators) # There are no more nested any
            enum_names, _ = make_union_names(validator.validators)

            if isinstance(validator, Map):
                # We load the ith map's key
                self.init_code.append((indent_level+1, 'status = PC_string(PC_get(tree, ".%s{%%d}"%s), &(%s%smap[i%d].key));' % (position, format_string(indices), c_variable, struct_ref, recursion_depth)))
                # We load the ith map's value
                self.load_map_list_item(validator, position+'<%d>', '%s%s%s[i%d]' % (c_variable, struct_ref, item_name, recursion_depth), path_to_enum, enum_names, indent_level+1, indices, recursion_depth=recursion_depth)

            else:
                # We load the ith list's value
                self.load_map_list_item(validator, position+'[%d]', '%s%s%s[i%d]' % (c_variable, struct_ref, item_name, recursion_depth), path_to_enum, enum_names, indent_level+1, indices, recursion_depth=recursion_depth)

        self.init_code.append((indent_level, '}'))

        indices.pop()


    def load_map_list_item(self, validator, position, c_variable, path_to_enum, enum_names, indent_level, indices=[], recursion_depth=0):
        """Generate the lines of code to detect a list/map item's type and load the corresponding data"""

        if isinstance(validator, List):
            item_name = 'tab'
        else:
            item_name = 'map'

        if validator.is_optional:
            struct_ref = '->'
        else:
            struct_ref = '.'

        # We flatten the nested any
        validators = find_nested_any(validator.validators)

        # We try to load the data as if it was validated by "validators[0]"
        val = validators[0]
        if isinstance(val, Include):
            val.is_required = False # An included type is always a pointer inside lists/maps in order to allow recursive definitions
        name = enum_names[0] + '_value'

        path_to_enum.append(enum_names[0])

        if isinstance(val, String) or isinstance(val, Include):
            self.init_code.append((indent_level, '%s = NULL;' % (c_variable+'.item.'+name)))
        self.init_code.append((indent_level, 'errh = PC_errhandler(PC_NULL_HANDLER);'))
        self.load_primitive_data(val, position, c_variable+'.item.'+name, path_to_enum, indent_level, indices, recursion_depth=recursion_depth+1)
        self.init_code.append((indent_level, 'PC_errhandler(errh);'))
        # We check if it succeeded
        self.init_code.append((indent_level, 'if (!status) {'))
        self.init_code.append((indent_level+1, '%s = %s;' % (c_variable+'.type', '_'.join(path_to_enum))))
        self.init_code.append((indent_level, '}'))
        self.init_code.append((indent_level, 'else {')) # Maybe the data was not validated by "vaidators[0]"...

        path_to_enum.pop()

        # If the first try did not work, we reiterate with the others validators
        for i, val in enumerate(validators[1:-1]):
            # We have to free the memory allocated in the previous try
            if not isinstance(validators[i], String):
                self.init_code.extend(c_free_memory(self.schema, validators[i], c_variable+'.item.'+name, path_to_enum, indent_level, indices, recursion_depth+2))

            if isinstance(val, Include):
                val.is_required = False # An included type is always a pointer inside lists/maps to allow recursive definitions

            # We extract the new name
            name = enum_names[i+1] + '_value'

            path_to_enum.append(enum_names[i+1])

            if isinstance(val, String) or isinstance(val, Include):
                self.init_code.append((indent_level+i+1, '%s = NULL;' % (c_variable+'.item.'+name)))
            # We load the primitive data at "position" in "c_variable+'item.'+name"
            self.init_code.append((indent_level+i+1, 'errh = PC_errhandler(PC_NULL_HANDLER);'))
            self.load_primitive_data(val, position, c_variable+'.item.'+name, path_to_enum, indent_level+i+1, indices, recursion_depth=recursion_depth+1)
            self.init_code.append((indent_level+i+1, 'PC_errhandler(errh);'))
            # We check if it succeeded
            self.init_code.append((indent_level+i+1, 'if (!status) {'))
            self.init_code.append((indent_level+i+2, '%s = %s;' % (c_variable+'.type', '_'.join(path_to_enum))))
            self.init_code.append((indent_level+i+1, '}'))
            self.init_code.append((indent_level+i+1, 'else {'))

            path_to_enum.pop()

        # We treat the last validator separately
        # If it fails we either want to break the loop (recursion depth > 0) or return an error (recursion depth = 0)
        i = len(validators)
        if i > 1:
            # We have to free the memory allocated in the previous try
            if not isinstance(validators[-2], String):
                path_to_enum.append(enum_names[-2])
                self.init_code.extend(c_free_memory(self.schema, validators[-2], c_variable+'.item.'+name, path_to_enum, indent_level+i-1, indices, recursion_depth+2))
                path_to_enum.pop()

            if isinstance(validators[-1], Include):
                validators[-1].is_required = False # An included type is always a pointer inside lists/maps to allow recursive definitions

            # We extract the new name
            name = enum_names[-1] + '_value'

            path_to_enum.append(enum_names[-1])

            # We load the primitive data at "position" in "c_variable+'item.'+name"
            self.init_code.append((indent_level+i-1, 'errh = PC_errhandler(PC_NULL_HANDLER);'))
            self.load_primitive_data(validators[-1], position, c_variable+'.item.'+name, path_to_enum, indent_level+i-1, indices, recursion_depth=recursion_depth+1)
            self.init_code.append((indent_level+i-1, 'PC_errhandler(errh);'))
            # We check if it succeeded
            self.init_code.append((indent_level+i-1, 'if (!status) {'))
            self.init_code.append((indent_level+i, '%s = %s;' % (c_variable+'.type', '_'.join(path_to_enum))))
            self.init_code.append((indent_level+i-1, '}'))
            self.init_code.append((indent_level+i-1, 'else {'))

            path_to_enum.pop()

        # We have to free the memory allocated in the previous try
        if recursion_depth==0:
            self.init_code.append((indent_level+i, 'return PC_INVALID_NODE_TYPE;'))
        else:
            self.init_code.append((indent_level+i, 'break;'))

        while i>0:
            i -= 1
            self.init_code.append((indent_level+i, '}'))

        self._insert_space_init()


    def load_union_item(self, validator, position, c_variable, path_to_enum, enum_names, indent_level, indices=[], recursion_depth=0):
        """Generate the lines of code to detect a union item's type and load the corresponding data"""

        if validator.is_optional:
            struct_ref = '->'
        else:
            struct_ref = '.'

        validators = find_nested_any(validator.validators) # There are no more nested any
        for _validator in validators:
            if not isinstance(_validator, Include):
                _validator.is_required = True
            else:
                _validator.is_required = False

        val = validators[0]
        name = enum_names[0] + '_value'
        path_to_enum.append(enum_names[0])

        if recursion_depth==0: # We have to declare the status
            self.init_code.append((indent_level, 'PC_status_t status;'))
            self.init_code.append((indent_level, 'PC_errhandler_t errh;'))

        if isinstance(val, String) or isinstance(val, Include) and val.is_optional:
            self.init_code.append((indent_level, '%s = NULL;' % (c_variable+struct_ref+'item.'+name)))
        # We load the primitive data at "position" in "c_variable+struct_ref+'item.'+name"
        self.init_code.append((indent_level, 'errh = PC_errhandler(PC_NULL_HANDLER);'))
        self.load_primitive_data(val, position, c_variable+struct_ref+'item.'+name, path_to_enum, indent_level, indices, recursion_depth=recursion_depth+1)
        self.init_code.append((indent_level, 'PC_errhandler(errh);'))
        # We check if it succeeded
        self.init_code.append((indent_level, 'if (!status) {'))
        self.init_code.append((indent_level+1, '%s = %s;' % (c_variable+struct_ref+'type', '_'.join(path_to_enum))))
        self.init_code.append((indent_level, '}'))
        self.init_code.append((indent_level, 'else {'))

        path_to_enum.pop()

        # If the first try failed, we reiterate with the other validators
        for i, val in enumerate(validators[1:-1]):

            # We have to free the memory allocated in the previous try
            self.init_code.extend(c_free_memory(self.schema, validators[i], c_variable+struct_ref+'item.'+name, path_to_enum, indent_level, indices, recursion_depth+2))

            name = enum_names[i+1] + '_value'
            path_to_enum.append(enum_names[i+1])

            if isinstance(val, String) or isinstance(val, Include) and val.is_optional:
                self.init_code.append((indent_level, '%s = NULL;' % (c_variable+struct_ref+'item.'+name)))
            # We load the primitive data at "position" in "c_variable+struct_ref+'item.'+name"
            self.init_code.append((indent_level+i+1, 'errh = PC_errhandler(PC_NULL_HANDLER);'))
            self.load_primitive_data(val, position, c_variable+struct_ref+'item.'+name, path_to_enum, indent_level+i+1, indices, recursion_depth=recursion_depth+1)
            self.init_code.append((indent_level+i+1, 'PC_errhandler(errh);'))
            # We check if it succeeded
            self.init_code.append((indent_level+i+1, 'if (!status) {'))
            self.init_code.append((indent_level+i+2, '%s = %s;' % (c_variable+struct_ref+'type', '_'.join(path_to_enum))))
            self.init_code.append((indent_level+i+1, '}'))
            self.init_code.append((indent_level+i+1, 'else {'))

            path_to_enum.pop()

        i = len(validators)
        if i > 1:

            # We have to free the memory allocated in the previous try
            if not isinstance(validators[-2], String):
                path_to_enum.append(enum_names[-2])
                self.init_code.extend(c_free_memory(self.schema, validators[-2], c_variable+struct_ref+'item.'+name, path_to_enum, indent_level, indices, recursion_depth+2))
                path_to_enum.pop()

            name = enum_names[-1] + '_value'
            path_to_enum.append(enum_names[-1])

            # We load the primitive data at "position" in "c_variable+struct_ref+'item.'+name"
            self.init_code.append((indent_level+i-1, 'errh = PC_errhandler(PC_NULL_HANDLER);'))
            self.load_primitive_data(validators[-1], position, c_variable+struct_ref+'item.'+name, path_to_enum, indent_level+i-1, indices, recursion_depth=recursion_depth+1)
            self.init_code.append((indent_level+i-1, 'PC_errhandler(errh);'))
            # We check if it succeeded
            self.init_code.append((indent_level+i-1, 'if (!status) {'))
            self.init_code.append((indent_level+i, '%s = %s;' % (c_variable+struct_ref+'type', '_'.join(path_to_enum))))
            self.init_code.append((indent_level+i, 'return status;'))
            self.init_code.append((indent_level+i-1, '}'))
            self.init_code.append((indent_level+i-1, 'else {'))
            self.init_code.append((indent_level+i, 'return PC_INVALID_NODE_TYPE;')) # We don't have to separate the cases where recursion depth > 0 and recursion depth = 0 (inside List/Map, "any" validators are replaced with their sub-validators)

            path_to_enum.pop()

        else:
            self.init_code.append((indent_level+1, 'return PC_INVALID_NODE_TYPE;'))

        while i>0:
            i -= 1
            self.init_code.append((indent_level+i, '}'))

        self._insert_space_init()


    def _insert_space_init(self, n=1):
        """Insert n space(s) at the end of the code list"""

        while n > 0:
            self.init_code.append((0, ''))
            n += -1


    def dump_code(self):
        """Dump the initialization functions code in <self.init_name>.c"""

        f = open(self.init_name+'.c', "w")

        for line in self.init_code:
            indent = ''
            for i in range(INDENT_SPACE * line[0]):
                indent += ' '
            f.write(indent + line[1] + '\n')

        f.close()


    def dump_header(self):
        """Dump the initialization functions header in <self.init_name>.h"""

        f = open(self.init_name+'.h', "w")

        for line in self.init_header:
            indent = ''
            for i in range(INDENT_SPACE * line[0]):
                indent += ' '
            f.write(indent + line[1] + '\n')

        f.close()
