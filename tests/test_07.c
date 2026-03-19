/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * * Neither the name of CEA nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/

#include <assert.h>
#include <pdi.h>

#include "test.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE_LENGTH 1024
#define PLACEHOLDER "/<full>/<path>/<to>/<test_07_data.yml>"
#define TEMP_FILE_PATH "/tmp_dir_test/temp_test_07.yml"  // Use /tmp_dir_test as the writable path

struct Record_data {
	int a;
	int* b;
} typedef Record_data;

// //
static void print_indent(int indent)
{
    for(int i = 0; i < indent; i++)
        printf("  ");
}

static const char* yaml_node_type_str(yaml_node_type_t t)
{
    switch(t)
    {
        case YAML_NO_NODE: return "YAML_NO_NODE";
        case YAML_SCALAR_NODE: return "YAML_SCALAR_NODE";
        case YAML_SEQUENCE_NODE: return "YAML_SEQUENCE_NODE";
        case YAML_MAPPING_NODE: return "YAML_MAPPING_NODE";
        default: return "INVALID_NODE_TYPE";
    }
}

static void print_node_ptr(yaml_document_t *doc, yaml_node_t *node, int indent)
{
    if(!node)
        return;

    switch(node->type)
    {
        case YAML_SCALAR_NODE:
            printf("%s", (char*)node->data.scalar.value);
            break;

        case YAML_SEQUENCE_NODE:
        {
            yaml_node_item_t *item = node->data.sequence.items.start;

            while(item < node->data.sequence.items.top)
            {
                yaml_node_t *child = yaml_document_get_node(doc, *item);

                print_indent(indent);
                // printf("- ");

                // if(child->type == YAML_SCALAR_NODE)
                // {
                //     printf("%s\n", (char*)child->data.scalar.value);
                // }
                // else
                // {
                //     printf("\n");
                //     print_node_ptr(doc, child, indent + 1);
                // }
                printf("- ");

                if(child->type == YAML_SCALAR_NODE)
                {
                    printf("%s\n", (char*)child->data.scalar.value);
                }
                else if(child->type == YAML_MAPPING_NODE)
                {
                    printf("\n");
                    print_node_ptr(doc, child, indent + 1);
                }
                else
                {
                    printf("\n");
                    print_node_ptr(doc, child, indent + 1);
                }

                item++;
            }

            break;
        }

        case YAML_MAPPING_NODE:
        {
            yaml_node_pair_t *pair = node->data.mapping.pairs.start;

            while(pair < node->data.mapping.pairs.top)
            {
                yaml_node_t *key = yaml_document_get_node(doc, pair->key);
                yaml_node_t *val = yaml_document_get_node(doc, pair->value);

                print_indent(indent);
                printf("%s:", (char*)key->data.scalar.value);

                if(val->type == YAML_SCALAR_NODE)
                {
                    printf(" %s\n", (char*)val->data.scalar.value);
                }
                else
                {
                    printf("\n");
                    print_node_ptr(doc, val, indent + 1);
                }

                pair++;
            }

            break;
        }

        // default:
        //     printf("UNKNOWN\n");
        
        default:
        {
            const char *tag = node->tag ? (char*)node->tag : "(none)";

            fprintf(stderr,
                "\nYAML ERROR: unknown node type\n"
                "  type: %s (%d)\n"
                "  tag : %s\n"
                "  indent level: %d\n",
                yaml_node_type_str(node->type),
                node->type,
                tag,
                indent
            );

            abort();
        }
    }
}
// //
void PC_debug_print(PC_tree_t tree)
{
    if(!tree.document)
    {
        printf("No YAML document\n");
        return;
    }

    if(!tree.node)
    {
        printf("No YAML root node\n");
        return;
    }

    print_node_ptr(tree.document, tree.node, 0);

    printf("\n");
}
// //

// int replace_placeholder_in_file(const char *file_path, const char *replace_str) {
//     FILE *file = fopen(file_path, "r");
//     if (!file) {
//         perror("Failed to open file");
//         return 1;
//     }
//     FILE *temp_file = fopen("temp_test_07.yml", "w");
//     if (!temp_file) {
//         perror("Failed to open temp file");
//         fclose(file);
//         return 1;
//     }
//     char line[MAX_LINE_LENGTH];
//     while (fgets(line, sizeof(line), file)) {
//         char *pos = strstr(line, PLACEHOLDER);
//         if (pos) {
//             fwrite(line, 1, pos - line, temp_file);
//             fwrite(replace_str, 1, strlen(replace_str), temp_file);
//             fwrite(pos + strlen(PLACEHOLDER), 1, strlen(pos) - strlen(PLACEHOLDER), temp_file);
//         }
//     }
//     fclose(file);
//     fclose(temp_file);
//     if (remove(file_path) != 0) {
//         perror("Failed to remove the original file");
//         return 1;
//     }
//     if (rename("temp_test_07.yml", file_path) != 0) {
//         perror("Failed to rename temp file to original file");
//         return 1;
//     }
//     return 0;
// }

int replace_placeholder_in_file(const char *file_path, const char *replace_str) {
    FILE *file = fopen(file_path, "r");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Create a temporary file to store modified content in a writable directory
    FILE *temp_file = fopen(TEMP_FILE_PATH, "w");
    if (!temp_file) {
        perror("Failed to open temp file");
        fclose(file);
        return 1;
    }

    char line[MAX_LINE_LENGTH];
    while (fgets(line, sizeof(line), file)) {
        // Find the placeholder and replace it
        char *pos = strstr(line, PLACEHOLDER);
        if (pos) {
            // Write the part before the placeholder
            fwrite(line, 1, pos - line, temp_file);
            // Write the replacement string
            fwrite(replace_str, 1, strlen(replace_str), temp_file);
            // Write the rest of the line after the placeholder
            fwrite(pos + strlen(PLACEHOLDER), 1, strlen(pos) - strlen(PLACEHOLDER), temp_file);
        } else {
            // If no placeholder, just copy the line as is
            fputs(line, temp_file);
        }
    }

    fclose(file);
    fclose(temp_file);

    // Inform that the modified file is now in the writable location
    printf("Modified YAML written to: %s\n", TEMP_FILE_PATH);

    return 0;
}

int main(int argc, char* argv[])
{
    // if (argc < 3) {
    //     fprintf(stderr, "May be missing absolute path to test_07_data.yml", argv[0]);
    //     return 1;
    // }
    // const char *yaml_path = argv[1];
    // const char *data_yaml_path = argv[2];
    // if (replace_placeholder_in_file(yaml_path, data_yaml_path) != 0) {
    //     fprintf(stderr, "Failed to modify the root YAML file test_07.yml\n");
    //     return 1;
    // }

	if (argc < 2) {
		fprintf(stderr, "Missing path to test_07.yml\n");
		return 1;
	}

	const char *yaml_path = argv[1];

	/* Build test_07_data.yml path from test_07.yml */
	char data_path[1024];
	snprintf(data_path, sizeof(data_path),
			"%.*s_data.yml",
			(int)(strlen(yaml_path) - 4),  // remove ".yml"
			yaml_path);

	/* Replace placeholder in YAML -> writes to /tmp_dir_test */
	if (replace_placeholder_in_file(yaml_path, data_path) != 0) {
		fprintf(stderr, "Failed to modify the root YAML file\n");
		return 1;
	}

	/* IMPORTANT: parse the MODIFIED file, not the original */
	PC_tree_t conf = PC_parse_path("/tmp_dir_test/temp_test_07.yml");
	PDI_init(PC_get(conf, ".pdi"));

	// PC_tree_t conf = PC_parse_path(argv[1]);
	// PDI_init(PC_get(conf, ".pdi"));

	PC_debug_print(conf);

	int input = 0;
	PDI_expose("input", &input, PDI_OUT);

	int scalar_data = 42;
	PDI_expose("scalar_data", &scalar_data, PDI_OUT);
	int array_data[8];
	for (int i = 0; i < 8; i++) {
		array_data[i] = 42 + i;
	}
	PDI_expose("array_data", array_data, PDI_OUT);

	Record_data record_data;
	record_data.a = 50;
	int b = 51;
	record_data.b = &b;
	PDI_expose("record_data", &record_data, PDI_OUT);

	input = 1;
	PDI_expose("input", &input, PDI_OUT);

	int scalar_data_read;
	PDI_expose("scalar_data", &scalar_data_read, PDI_IN);
	printf("%d ?== %d\n", scalar_data, scalar_data_read);
	if (scalar_data != scalar_data_read) {
		fprintf(stderr, "Assertion failed: %d != %d\n", scalar_data, scalar_data_read);
		exit(EXIT_FAILURE);
	}

	int array_data_read[8];
	PDI_expose("array_data", array_data_read, PDI_IN);
	for (int i = 2; i < 6; i++) {
		printf("[%d] %d ?== %d\n", i, array_data[i], array_data_read[i]);
		assert(array_data[i] == array_data_read[i]);
	}

	int b_read;
	Record_data record_data_read;
	record_data_read.b = &b_read;
	PDI_expose("record_data", &record_data_read, PDI_IN);
	printf("%d ?== %d\n", record_data.a, record_data_read.a);
	assert(record_data.a == record_data_read.a);
	printf("%d ?== %d\n", b, b_read);
	assert(b == b_read);

	PDI_finalize();
	return 0;
}
