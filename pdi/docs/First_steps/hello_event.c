#include <pdi.h>

int main(int argc, char* argv[]) {
    PDI_init(PC_parse_path("hello_event.yml"));

    PDI_event("Hello World Event");

    PDI_finalize();
    return 0;
}
