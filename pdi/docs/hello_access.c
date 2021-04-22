#include <pdi.h>

void print_secret_msg() {
    int* value;
    PDI_access("my_value", (void**)&value, PDI_IN);
    printf("%d\n", *value);
    PDI_release("my_value");

    char* message;
    PDI_access("my_message", (void**)&message, PDI_IN);
    printf("%s\n", message);
    PDI_release("my_message");
}

int main(int argc, char* argv[]) {
    PDI_init(PC_parse_path("world_access.yml"));
    int my_value = 42;
    PDI_share("my_value", &my_value, PDI_OUT);

    char* secret_msg = "Watermelon is the tastiest fruit";
    PDI_share("my_message", secret_msg, PDI_OUT);

    print_secret_msg();

    PDI_reclaim("my_message");
    PDI_reclaim("my_value");

    PDI_finalize();
    return 0;
}
