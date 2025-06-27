#include <paraconf.h>
#include <iostream>
#include <vector>
#include <cstdlib>
#include "pdi.h"

using std::size_t;
using std::vector;

constexpr char pdi_config[]=R"(
types:
    vector_content: 
        type: struct
        members:
            - data: 
                type: pointer
                subtype: {type: array, subtype: double, size: $size_glo } 

            - size: size_t

data:
    size_glo: size_t
    my_array: {type: vector_content}

plugins:
    trace: ~
    json:
        my_array: test.json
            

)";

template <typename T>
struct vector_content {
    T* data;
    size_t size;
};

int main ()
{
    PDI_init(PC_parse_string(pdi_config));

    vector_content<double> my_vec2{new double[5], 5};
    // *(my_vec2.data) = {1,2,3,4,5};
    for (int i=0; i<5; i++)
    {
        my_vec2.data[i] = i+1;
    }

    PDI_multi_expose("test2", "size_glo", &my_vec2.size,     PDI_OUT,
                              "my_array", &my_vec2, PDI_OUT,
                              NULL);
    

    PDI_finalize();
}
