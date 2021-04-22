#include <pdi.h>

int main(int argc, char* argv[]) {
    
    PDI_init(PC_parse_path("hello_data.yml"));
    int my_world = 42;
    
    //! [share_reclaim]
    PDI_share("world", &my_world, PDI_OUT);
    PDI_reclaim("world");
    //! [share_reclaim]
    
    //! [expose]
    PDI_expose("world", &my_world, PDI_OUT);
    //! [expose]

    PDI_finalize();
    return 0;
}
