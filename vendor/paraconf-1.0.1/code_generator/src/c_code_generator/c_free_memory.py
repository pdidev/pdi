from yamale.validators import *
from c_code_generator.tools import convert_enum_to_any, find_nested_any, make_flat_tree, make_union_names, replace_chars

INDENT_SPACE = 8
FREE_HEADER = 'PARACONF_FREE_MEMORY_H__'


def c_free_memory(schema, validator, c_variable, path_to_enum, indent_level, indices=[], recursion_depth=0):
    """Free the memory of a c variable's allocated elements, without deallocating the c variable itself"""

    # Lines of code are represented by tuples of two elements: indentation level and string of characters
    code_lines = []

    if validator.is_optional:
        struct_ref = '->'
    else:
        struct_ref = '.'

    # Freeing the memory of a variable corresponding to an any() validator
    if isinstance(validator, Any):
        validator.validators = find_nested_any(validator.validators)
        sub_validators = [_val for _val in validator.validators if isinstance(_val, (Include, List, Map, String))] # Inside Lists/Maps only Includes/Lists/Maps/Strings are allocated
        for _val in sub_validators:
            if isinstance(_val, Include):
                _val.is_required=False

        if len(sub_validators)>0:
            enum_names, _ = make_union_names(sub_validators) # We only consider the enum names of allocatable fields

            enum_name = enum_names[0]
            code_lines.append((indent_level+1, 'if (%s == %s) {' % ('_'.join(path_to_enum)+'_'+enum_name, c_variable+struct_ref+'type')))
            if isinstance(sub_validators[0], String):
                code_lines.append((indent_level+2, 'if (NULL != %s) {' % (c_variable+struct_ref+'item.'+enum_name+'_value')))
                code_lines.append((indent_level+3, 'free(%s);' % (c_variable+struct_ref+'item.'+enum_name+'_value')))
                code_lines.append((indent_level+2, '}'))

            else:
                if sub_validators[0].is_optional:
                    indent_level += 1
                    code_lines.append((indent_level+1, 'if (NULL != %s) {' % (c_variable+struct_ref+'item.'+enum_name+'_value')))

                code_lines.extend(c_free_memory(schema, sub_validators[0], c_variable+struct_ref+'item.'+enum_name+'_value', path_to_enum+[enum_name], indent_level+1, indices, recursion_depth))

                if sub_validators[0].is_optional:
                    code_lines.append((indent_level+2, 'free(%s);' % (c_variable+struct_ref+'item.'+enum_name+'_value')))
                    code_lines.append((indent_level+1, '}'))
                    indent_level -= 1

            code_lines.append((indent_level+1, '}'))

            for i, enum_name in enumerate(enum_names[1:], 1):

                code_lines.append((indent_level+1, 'else if (%s == %s) {' % ('_'.join(path_to_enum)+'_'+enum_name, c_variable+struct_ref+'type')))

                if isinstance(sub_validators[i], String):
                    code_lines.append((indent_level+2, 'if (NULL != %s) {' % (c_variable+struct_ref+'item.'+enum_name+'_value')))
                    code_lines.append((indent_level+3, 'free(%s);' % (c_variable+struct_ref+'item.'+enum_name+'_value')))
                    code_lines.append((indent_level+2, '}'))

                else:
                    if sub_validators[i].is_optional:
                        indent_level += 1
                        code_lines.append((indent_level+1, 'if (NULL != %s) {' % (c_variable+struct_ref+'item.'+enum_name+'_value')))

                    code_lines.extend(c_free_memory(schema, sub_validators[i], c_variable+struct_ref+'item.'+enum_name+'_value', path_to_enum+[enum_name], indent_level+1, indices, recursion_depth))

                    if sub_validators[i].is_optional:
                        code_lines.append((indent_level+2, 'free(%s);' % (c_variable+struct_ref+'item.'+enum_name+'_value')))
                        code_lines.append((indent_level+1, '}'))
                        indent_level -= 1

                code_lines.append((indent_level+1, '}'))

    # Freeing the memory of a variable corresponding to an enum() validator
    elif isinstance(validator, Enum):
        new_validator = convert_enum_to_any(validator)
        code_lines.extend(c_free_memory(schema, new_validator, c_variable, path_to_enum, indent_level, indices, recursion_depth))

    # Freeing the memory of a variable corresponding to an include() validator
    elif isinstance(validator, Include):
        if validator.is_optional:
            code_lines.append((indent_level+1, 'free_%s(%s);' % (replace_chars(validator.args[0]), c_variable)))
        else:
            code_lines.append((indent_level+1, 'free_%s(&(%s));' % (replace_chars(validator.args[0]), c_variable)))

    # Freeing the memory of a variable corresponding to a list() validator
    elif isinstance(validator, List):
        validator.validators = find_nested_any(validator.validators)
        sub_validators = [_val for _val in validator.validators if isinstance(_val, (Include, List, Map, String))] # Inside Lists/Maps only Includes/Lists/Maps are allocated
        for sub_val in sub_validators:
            # Only Includes validators will be pointers
            if not isinstance(sub_val, Include):
                sub_val.is_required = True
            else:
                sub_val.is_required = False

        # If the list has been allocated we have to deallocate it
        code_lines.append((indent_level+1, 'if (NULL != %s%stab) {' % (c_variable, struct_ref)))
        if len(sub_validators)>0:
            indices.append('i%d' % (recursion_depth))
            code_lines.append((indent_level+2, 'for (int %s = 0 ; %s < %s%slen ; ++%s) {' % (indices[-1], indices[-1], c_variable, struct_ref, indices[-1])))

            _val = Any()
            _val.validators = sub_validators
            code_lines.extend(c_free_memory(schema, _val, '%s%stab[%s]' % (c_variable, struct_ref, indices[-1]), path_to_enum, indent_level+2, indices, recursion_depth+1))
            code_lines.append((indent_level+2, '}'))
            indices.pop()

        code_lines.append((indent_level+2, 'free(%s%stab);' % (c_variable, struct_ref)))
        code_lines.append((indent_level+1, '}'))

    # Freeing the memory of a variable corresponding to a map() validator
    elif isinstance(validator, Map):
        validator.validators = find_nested_any(validator.validators)
        sub_validators = [_val for _val in validator.validators if isinstance(_val, (Include, List, Map, String))] # Inside Lists/Maps only Includes/Lists/Maps are allocated
        for sub_val in sub_validators:
            # Only Includes validators will be pointers
            if not isinstance(sub_val, Include):
                sub_val.is_required = True
            else:
                sub_val.is_required = False

        # If the map has been allocated we have to deallocate it
        code_lines.append((indent_level+1, 'if (NULL != %s%smap) {' % (c_variable, struct_ref)))
        indices.append('i%d' % (recursion_depth))
        code_lines.append((indent_level+2, 'for (int %s = 0 ; %s < %s%slen ; ++%s) {' % (indices[-1], indices[-1], c_variable, struct_ref, indices[-1])))
        code_lines.append((indent_level+3, 'if (NULL != %s%smap[%s].key) {' % (c_variable, struct_ref, indices[-1])))
        code_lines.append((indent_level+4, 'free(%s%smap[%s].key);' % (c_variable, struct_ref, indices[-1])))
        code_lines.append((indent_level+3, '}'))

        if len(sub_validators)>0:
            _val = Any()
            _val.validators = sub_validators
            code_lines.extend(c_free_memory(schema, _val, '%s%smap[%s]' % (c_variable, struct_ref, indices[-1]), path_to_enum, indent_level+2, indices, recursion_depth+1))

        code_lines.append((indent_level+2, '}'))
        code_lines.append((indent_level+2, 'free(%s%smap);' % (c_variable, struct_ref)))
        code_lines.append((indent_level+1, '}'))

        indices.pop()

    return code_lines;


def c_free_root(schema):
    """Generate the functions' declaration and definitions to free all the root's fields"""

    code_lines = []
    header_lines = []
    indent_level = 0

    c_free_includes(schema, code_lines, header_lines)

    code_lines.append((indent_level, ''))
    code_lines.append((indent_level, 'void free_root(root_t* root) {'))
    code_lines.append((indent_level, ''))

    header_lines.append((indent_level, ''))
    header_lines.append((indent_level, 'void free_root(root_t* root);'))
    header_lines.append((indent_level, ''))

    # We free all generic types
    dependency_tree = make_flat_tree(schema._schema.keys())
    code_lines.extend(c_free_generic(dependency_tree, '', 'root->', indent_level+1))

    # We gather all potentially allocated variables
    allocated_variables = []
    for key in schema._schema.keys():
        if schema._schema[key].is_optional or isinstance(schema._schema[key], (Include, List, Map, String)):
            allocated_variables.append(key)
        elif isinstance(schema._schema[key], Any):
            any_validators = find_nested_any(schema._schema[key].validators)
            for val in any_validators:
                if isinstance(val, (Include, List, Map, String)):
                    allocated_variables.append(key)
                    break

    allocated_variables.sort()

    # We call the sub-free functions (free_root_<path_to_depenedncy>()) inside free_root()
    for allocated_var in allocated_variables:
        c_path = [replace_chars(s) for s in allocated_var.split('.')]
        code_lines.append((indent_level+1, 'free_root_%s(root);' % ('_'.join(c_path))))
    code_lines.append((indent_level, '}'))

    # We define the sub-free functions (free_root_<path_to_depenedncy>())
    for allocated_var in allocated_variables:
        validator = schema._schema[allocated_var]
        c_path = [replace_chars(s) for s in allocated_var.split('.')]
        c_variable = 'root->' + '.'.join(c_path)
        path_to_enum = ['root'] + c_path
        code_lines.extend(c_free_node(schema, validator, c_variable, path_to_enum))
        header_lines.append((indent_level, 'void free_root_%s(root_t* root);' % ('_'.join(c_path))))

    # We generate the end of the header file
    header_lines.append((indent_level, ''))
    header_lines.append((indent_level, ''))
    header_lines.append((indent_level, '#endif'))

    return code_lines, header_lines


def c_free_includes(schema, code_lines, header_lines):
    """Generate the "free" functions corresponding to the included nodes"""

    indent_level = 0

    header_lines.append((indent_level, ''))

    # We gather all potentially allocated variables
    allocated_variables = []
    sorted_key_list = [k for k in schema.includes.keys()]
    sorted_key_list.sort()

    for included_key in sorted_key_list:
        allocated_variables.append([])
        for key in schema.includes[included_key]._schema.keys():
            if schema.includes[included_key]._schema[key].is_optional or isinstance(schema.includes[included_key]._schema[key], (Include, List, Map, String)):
                allocated_variables[-1].append(key)
            elif isinstance(schema.includes[included_key]._schema[key], Any):
                any_validators = find_nested_any(schema.includes[included_key]._schema[key].validators)
                for val in any_validators:
                    if isinstance(val, (Include, List, Map, String)):
                        allocated_variables[-1].append(key)
                        break
            elif isinstance(schema.includes[included_key]._schema[key], Enum):
                any_val = convert_enum_to_any(schema.includes[included_key]._schema[key])
                for val in any_val.validators:
                    if isinstance(val, String):
                        allocated_variables[-1].append(key)
                        break

    # We add the declaration/definition of "free" functions in the header/code
    for i, included_key in enumerate(sorted_key_list):

        header_lines.append((indent_level, 'void free_%s(%s_t* %s);' % (replace_chars(included_key), replace_chars(included_key), replace_chars(included_key))))

        code_lines.append((indent_level, ''))
        code_lines.append((indent_level, ''))
        code_lines.append((indent_level, 'void free_%s(%s_t* %s) {' % (replace_chars(included_key), replace_chars(included_key), replace_chars(included_key))))

        code_lines.append((indent_level+1, 'if (NULL != %s->generic.node) {' % (replace_chars(included_key))))
        code_lines.append((indent_level+2, 'free(%s->generic.node);' % (replace_chars(included_key))))
        code_lines.append((indent_level+1, '}'))

        for key in allocated_variables[i]:
            val = schema.includes[included_key]._schema[key]
            path_to_enum = [replace_chars(included_key), replace_chars(key)]
            header_lines.append((indent_level, 'void free_%s(%s_t* %s);' % ('_'.join(path_to_enum), replace_chars(included_key), replace_chars(included_key))))
            code_lines.append((indent_level+1, 'free_%s(%s);' % ('_'.join(path_to_enum), replace_chars(included_key))))

        code_lines.append((indent_level, '}'))

    # We add the declaration/edfinition of free sub-functions in the header/code
    for i, included_key in enumerate(sorted_key_list):

        for key in allocated_variables[i]:
            val = schema.includes[included_key]._schema[key]
            c_variable = '%s->%s' % (replace_chars(included_key), replace_chars(key))
            path_to_enum = [replace_chars(included_key), replace_chars(key)]
            code_lines.extend(c_free_node(schema, val, c_variable, path_to_enum))


def c_free_generic(dependency_tree, previous_path, previous_var, indent_level):
    """Free the generic fields of the root/included nodes"""

    code_lines = []

    code_lines.append((indent_level, 'if (NULL != %s) {' % (previous_var+'generic.node')))
    code_lines.append((indent_level+1, 'free(%s);' % (previous_var+'generic.node')))
    code_lines.append((indent_level, '}'))

    # We iterate recursively towards the leaves
    for key, path_to_dependencies in dependency_tree.items():
        new_tree = make_flat_tree(path_to_dependencies)
        new_path = previous_path+key
        new_var = previous_var+replace_chars(key)
        if not new_tree==None:
            code_lines.extend(c_free_generic(new_tree, new_path+'.', new_var+'.', indent_level))

    return code_lines


def c_free_node(schema, validator, c_variable, path_to_enum):
    """Generate the function to deallocate a root/included node leaf (= primitive type)"""

    code_lines = []
    indent_level = 0

    code_lines.append((indent_level, ''))
    code_lines.append((indent_level, ''))
    code_lines.append((indent_level, 'void free_%s(%s_t* %s) {' % ('_'.join(path_to_enum), path_to_enum[0], path_to_enum[0])))

    # If the variable points to a scalar type, we deallocate with the free() function
    if isinstance(validator, (Boolean, Integer, Number)) and validator.is_optional or isinstance(validator, String):
        code_lines.append((indent_level+1, 'if (NULL != %s) {' % (c_variable)))
        code_lines.append((indent_level+2, 'free(%s);' % (c_variable)))
        code_lines.append((indent_level+1, '}'))
    # Else the variable's type correspond to an any/enum/list/map validator
    else:
        if validator.is_optional:
            indent_level += 1
            code_lines.append((indent_level, 'if (NULL != %s) {' % (c_variable)))

        code_lines.extend(c_free_memory(schema, validator, c_variable, path_to_enum, indent_level))

        if validator.is_optional:
            code_lines.append((indent_level+1, 'free(%s);' % (c_variable)))
            code_lines.append((indent_level, '}'))
            indent_level -= 1

    code_lines.append((indent_level, '}'))

    return code_lines


def has_allocated_member(validator):
    """Test if a validator has an allocatable field"""

    if validator.is_optional or isinstance(validator, (Include, List, Map, String)):
        return True
    if isinstance(validator, Any):
        any_validators = find_nested_any(validator)
        for val in any_validators:
            if isinstance(val, (Include, List, Map, String)):
                return True

    return False
