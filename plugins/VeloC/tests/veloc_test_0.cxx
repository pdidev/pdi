#include <assert.h>
#include <mpi.h>
#include <pdi.h>
#include <iostream>
#include <stdexcept>

static void error_handler(PDI_status_t status, const char* message, void* ctx)
{
    if (status) {
        std::cerr << "[PDI error] " << message << "\n";
        *static_cast<int*>(ctx) = 1;
    }
}

 const char CONF_VALID[] =
        "metadata:\n"
        "  ii: int\n"
        "data:\n"
        "  var: int\n"
        "plugins:\n"
        "  veloc:\n"
        "    failure: 0\n"
        "    config_file: veloc_config.cfg\n"
        "    checkpoint_label: test_01\n"
        "    iteration: ii\n"
        "    protect_data: [ii, var]\n"
        "    checkpoint_on: ckp\n";

    const char CONF_CONFIG_MISSING[] =
        "metadata:\n"
        "  ii: int\n"
        "data:\n"
        "  var: int\n"
        "plugins:\n"
        "  veloc:\n"
        "    failure: 0\n"
        "    checkpoint_label: test_01\n"
        "    iteration: ii\n"
        "    protect_data: [ii, var]\n"
        "    checkpoint_on: ckp\n";

    const char CONF_EVENTS_MISSING[] =
    "metadata:\n"
    "  ii: int\n"
    "data:\n"
    "  var: int\n"
    "plugins:\n"
    "  veloc:\n"
    "    failure: 0\n"
    "    config_file: veloc_config.cfg\n"
    "    checkpoint_label: test_01\n"
    "    iteration: ii\n"
    "    protect_data: [ii, var]\n";

    const char CONF_ITERATION_MISSING[] =
        "metadata:\n"
        "  ii: int\n"
        "data:\n"
        "  var: int\n"
        "plugins:\n"
        "  veloc:\n"
        "    failure: 0\n"
        "    config_file: veloc_config.cfg\n"
        "    checkpoint_label: test_01\n"
        "    protect_data: [ii, var]\n"
        "    checkpoint_on: ckp\n";

    const char CONF_ITER_NOT_PROTECTED[] =
        "metadata:\n"
        "  ii: int\n"
        "data:\n"
        "  var: int\n"
        "plugins:\n"
        "  veloc:\n"
        "    failure: 0\n"
        "    config_file: veloc_config.cfg\n"
        "    checkpoint_label: test_01\n"
        "    iteration: ii\n"
        "    protect_data: [var]\n"
        "    checkpoint_on: ckp\n";

    const char CONF_BAD_FAILURE[] =
        "metadata:\n"
        "  ii: int\n"
        "data:\n"
        "  var: int\n"
        "plugins:\n"
        "  veloc:\n"
        "    failure: 99\n"
        "    config_file: veloc_config.cfg\n"
        "    checkpoint_label: test_01\n"
        "    iteration: ii\n"
        "    protect_data: [ii, var]\n"
        "    checkpoint_on: ckp\n";

    const char CONF_FAILURE_MISSING[] =
        "metadata:\n"
        "  ii: int\n"
        "data:\n"
        "  var: int\n"
        "plugins:\n"
        "  veloc:\n"
        "    config_file: veloc_config.cfg\n"
        "    checkpoint_label: test_01\n"
        "    iteration: ii\n"
        "    protect_data: [ii, var]\n"
        "    checkpoint_on: ckp\n";

    const char CONF_LABEL_MISSING[] =
        "metadata:\n"
        "  ii: int\n"
        "data:\n"
        "  var: int\n"
        "plugins:\n"
        "  veloc:\n"
        "    failure: 0\n"
        "    config_file: veloc_config.cfg\n"
        "    iteration: ii\n"
        "    protect_data: [ii, var]\n"
        "    checkpoint_on: ckp\n";

    const char CONF_PROTECT_DATA_MISSING[] =
        "metadata:\n"
        "  ii: int\n"
        "data:\n"
        "  var: int\n"
        "plugins:\n"
        "  veloc:\n"
        "    failure: 0\n"
        "    config_file: veloc_config.cfg\n"
        "    checkpoint_label: test_01\n"
        "    iteration: ii\n"
        "    checkpoint_on: ckp\n";

    const char CONF_CP_ROUTED_FILE_MISSING[] =
        "metadata:\n"
        "  ii: int\n"
        "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
        "data:\n"
        "  var: int\n"
        "plugins:\n"
        "  veloc:\n"
        "    failure: 0\n"
        "    config_file: veloc_config.cfg\n"
        "    checkpoint_label: test_01\n"
        "    iteration: ii\n"
        "    protect_data: [ii, var]\n"
        "    manual_checkpoint:\n"
        "      original_file: my_file.dat\n"
        "      start_on: start_ckp\n"
        "      route_file_on: route_ckp\n"
        "      end_on: end_ckp\n";

    const char CONF_CP_START_MISSING[] =
        "metadata:\n"
        "  ii: int\n"
        "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
        "data:\n"
        "  var: int\n"
        "plugins:\n"
        "  veloc:\n"
        "    failure: 0\n"
        "    config_file: veloc_config.cfg\n"
        "    checkpoint_label: test_01\n"
        "    iteration: ii\n"
        "    protect_data: [ii, var]\n"
        "    manual_checkpoint:\n"
        "      original_file: my_file.dat\n"
        "      veloc_file: veloc_file_buf\n"
        "      route_file_on: route_ckp\n"
        "      end_on: end_ckp\n";

    const char CONF_CP_ROUTE_MISSING[] =
        "metadata:\n"
        "  ii: int\n"
        "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
        "data:\n"
        "  var: int\n"
        "plugins:\n"
        "  veloc:\n"
        "    failure: 0\n"
        "    config_file: veloc_config.cfg\n"
        "    checkpoint_label: test_01\n"
        "    iteration: ii\n"
        "    protect_data: [ii, var]\n"
        "    manual_checkpoint:\n"
        "      original_file: my_file.dat\n"
        "      veloc_file: veloc_file_buf\n"
        "      start_on: start_ckp\n"
        "      end_on: end_ckp\n";

    const char CONF_CP_END_MISSING[] =
        "metadata:\n"
        "  ii: int\n"
        "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
        "data:\n"
        "  var: int\n"
        "plugins:\n"
        "  veloc:\n"
        "    failure: 0\n"
        "    config_file: veloc_config.cfg\n"
        "    checkpoint_label: test_01\n"
        "    iteration: ii\n"
        "    protect_data: [ii, var]\n"
        "    manual_checkpoint:\n"
        "      original_file: my_file.dat\n"
        "      veloc_file: veloc_file_buf\n"
        "      start_on: start_ckp\n"
        "      route_file_on: route_ckp\n";

    const char CONF_REC_ROUTED_FILE_MISSING[] =
        "metadata:\n"
        "  ii: int\n"
        "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
        "data:\n"
        "  var: int\n"
        "plugins:\n"
        "  veloc:\n"
        "    failure: 1\n"
        "    config_file: veloc_config.cfg\n"
        "    checkpoint_label: test_01\n"
        "    iteration: ii\n"
        "    protect_data: [ii, var]\n"
        "    recover_on: rec\n"
        "    manual_recovery:\n"
        "      original_file: my_file.dat\n"
        "      start_on: start_rec\n"
        "      route_file_on: route_rec\n"
        "      end_on: end_rec\n";

    const char CONF_REC_START_MISSING[] =
        "metadata:\n"
        "  ii: int\n"
        "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
        "data:\n"
        "  var: int\n"
        "plugins:\n"
        "  veloc:\n"
        "    failure: 1\n"
        "    config_file: veloc_config.cfg\n"
        "    checkpoint_label: test_01\n"
        "    iteration: ii\n"
        "    protect_data: [ii, var]\n"
        "    recover_on: rec\n"
        "    manual_recovery:\n"
        "      original_file: my_file.dat\n"
        "      veloc_file: veloc_file_buf\n"
        "      route_file_on: route_rec\n"
        "      end_on: end_rec\n";

    const char CONF_REC_ROUTE_MISSING[] =
        "metadata:\n"
        "  ii: int\n"
        "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
        "data:\n"
        "  var: int\n"
        "plugins:\n"
        "  veloc:\n"
        "    failure: 1\n"
        "    config_file: veloc_config.cfg\n"
        "    checkpoint_label: test_01\n"
        "    iteration: ii\n"
        "    protect_data: [ii, var]\n"
        "    recover_on: rec\n"
        "    manual_recovery:\n"
        "      original_file: my_file.dat\n"
        "      veloc_file: veloc_file_buf\n"
        "      start_on: start_rec\n"
        "      end_on: end_rec\n";

    const char CONF_REC_END_MISSING[] =
        "metadata:\n"
        "  ii: int\n"
        "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
        "data:\n"
        "  var: int\n"
        "plugins:\n"
        "  veloc:\n"
        "    failure: 1\n"
        "    config_file: veloc_config.cfg\n"
        "    checkpoint_label: test_01\n"
        "    iteration: ii\n"
        "    protect_data: [ii, var]\n"
        "    recover_on: rec\n"
        "    manual_recovery:\n"
        "      original_file: my_file.dat\n"
        "      veloc_file: veloc_file_buf\n"
        "      start_on: start_rec\n"
        "      route_file_on: route_rec\n";

int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);

    struct Test { const char* name; const char* yaml; int expect_error; };

    const Test tests[] = {
        { "valid config",                               CONF_VALID,                   0 },
        { "no events defined ",                         CONF_EVENTS_MISSING,          1 },
        { "missing config_file",                        CONF_CONFIG_MISSING,          1 },
        { "missing iteration",                          CONF_ITERATION_MISSING,       1 },
        { "iteration not in protect_data",              CONF_ITER_NOT_PROTECTED,      1 },
        { "invalid failure value",                      CONF_BAD_FAILURE,             1 },
        { "missing failure value",                      CONF_FAILURE_MISSING,         1 },
        { "missing checkpoint_label",                   CONF_LABEL_MISSING,           1 },
        { "checkpoint event but no protect_data",       CONF_PROTECT_DATA_MISSING,    1 },
        { "manual_checkpoint: missing veloc_file",      CONF_CP_ROUTED_FILE_MISSING,  1 },
        { "manual_checkpoint: missing start_on",        CONF_CP_START_MISSING,        1 },
        { "manual_checkpoint: missing route_file_on",   CONF_CP_ROUTE_MISSING,        1 },
        { "manual_checkpoint: missing end_on",          CONF_CP_END_MISSING,          1 },
        { "manual_recovery: missing veloc_file",        CONF_REC_ROUTED_FILE_MISSING, 1 },
        { "manual_recovery: missing start_on",          CONF_REC_START_MISSING,       1 },
        { "manual_recovery: missing route_file_on",     CONF_REC_ROUTE_MISSING,       1 },
        { "manual_recovery: missing end_on",            CONF_REC_END_MISSING,         1 },
    };


    int passed = 0, failed = 0;

    for (auto& t : tests) {
        int has_errored = 0;

        // Build a custom error handler that records whether PDI reported an error.
        PDI_errhandler_t custom_handler;
        custom_handler.func    = error_handler;
        custom_handler.context = &has_errored;

        PC_tree_t conf = PC_parse_string(t.yaml);

        // Install custom handler, saving the previous one.
        PDI_errhandler_t prev_handler = PDI_errhandler(custom_handler);

        PDI_init(conf);

        // Restore the previous handler immediately after init.
        PDI_errhandler(prev_handler);

        // If init succeeded (no error reported), finalize
        if (!has_errored) {
            PDI_finalize();
        }

        bool ok = (has_errored == t.expect_error);
        std::cout << (ok ? "[PASS] " : "[FAIL] ") << t.name << "\n";
        ok ? ++passed : ++failed;
    }

    std::cout << "\n" << passed << " passed, " << failed << " failed.\n";

    if(failed == 0){
        std::cout << "TEST 0_0 PASSED " <<std::endl;
    }

    MPI_Finalize();
    return (failed == 0) ? 0 : 1;
}