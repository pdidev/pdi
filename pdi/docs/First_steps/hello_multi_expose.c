#include <pdi.h>

int main(int argc, char* argv[]) {
    PDI_init(PC_parse_path("hello_multi_expose.yml"));
    int x = 0;
    float y = 0;
    char* z = "RGB = Really Gawky Biscuit";

    PDI_multi_expose("event_between", 
                     "my_int", &x, PDI_OUT,
                     "my_float", &y, PDI_OUT,
                     "my_string", &z, PDI_OUT,
                     NULL);

    PDI_finalize();
    return 0;
}
