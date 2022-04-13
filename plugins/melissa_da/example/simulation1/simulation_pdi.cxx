#include <mpi.h>
#include <cassert>
#include <chrono>
#include <vector>
#include <algorithm>

#include <unistd.h>

#include <iostream>
#include <fstream>

#include <pdi.h>

#include <csignal>

typedef struct index_map_s
{
    int index;
    int varid;
} index_map_t;

#define INDEX_MAP_T index_map_t

// int GLOBAL_VECT_SIZE = 40* 10000;
int GLOBAL_VECT_SIZE = 40;
// const int GLOBAL_VECT_SIZE = 1000;
// const int GLOBAL_VECT_SIZE = 1000*100*10;
// const int GLOBAL_VECT_SIZE = 1000*1000*10;

using namespace std;

int main(int argc, char * args[])
{
    auto init_start = std::chrono::steady_clock::now();
    if (argc > 1)
    {
        GLOBAL_VECT_SIZE = atoi(args[1]);
        printf("Changed global vect size to %d\n", GLOBAL_VECT_SIZE);
    }

    int comm_size;
    int comm_rank;

    MPI_Init(NULL, NULL);
    MPI_Comm_size(MPI_COMM_WORLD, &comm_size);
    MPI_Comm_rank(MPI_COMM_WORLD, &comm_rank);

    PDI_errhandler(PDI_NULL_HANDLER);
    PDI_init(PC_parse_path("/home/kacper/Workspace/pdi/plugins/melissa_da/example/simulation1/simulation_pdi.yaml"));

    auto init_end = std::chrono::steady_clock::now();

    int local_vect_size = GLOBAL_VECT_SIZE / comm_size;
    if (GLOBAL_VECT_SIZE % comm_size != 0 && comm_rank < GLOBAL_VECT_SIZE % comm_size)
    {
        local_vect_size += 1;
    }

    PDI_expose("local_vect_size", &local_vect_size, PDI_OUT);

    // how many pixels are left of me?
    vector<int> offsets(comm_size); //(comm_size,0)
    vector<int> counts(comm_size);
    fill(offsets.begin(), offsets.end(), 0);
    fill(counts.begin(), counts.end(), 0);
    int next_offset = 0;
    for (int rank = 0; rank < comm_size; rank++)
    {
        offsets[rank] = next_offset;

        counts[rank] = GLOBAL_VECT_SIZE / comm_size;
        // add one for all who have one more.
        if (GLOBAL_VECT_SIZE % comm_size != 0 && rank <
            GLOBAL_VECT_SIZE % comm_size)
        {
            counts[rank] += 1;
        }

        next_offset += counts[rank];
    }
    vector<double> state1(local_vect_size);

// INDEX_MAP
    std::vector<INDEX_MAP_T> local_index_map(state1.size());
    int last_entry = 0;
    int entry = 1;
    int new_entry;
    // calculate entries left of this rank:
    for (int i = 0; i < offsets[comm_rank]; i++)
    {
        new_entry = last_entry + entry;
        last_entry = entry;
        entry = new_entry;
    }

    for (auto &it : local_index_map)
    {
        new_entry = last_entry + entry;
        last_entry = entry;
        entry = new_entry;
        it.index = new_entry;
        it.varid = 1;
        cout << "index_map_entry: " << it.varid << ':' << it.index << std::endl;
    }

    PDI_expose("local_index_map", local_index_map.data(), PDI_OUT);
// END INDEX_MAP
// INDEX MAP HIDDEN
    int* local_hidden_vect_size;
    PDI_status_t secret_state_status = PDI_access("local_hidden_vect_size", (void**)&local_hidden_vect_size, PDI_IN);
    if (secret_state_status == PDI_OK) {
        std::vector<INDEX_MAP_T> local_index_map_hidden(*local_hidden_vect_size);
        last_entry = 0;
        entry = 1;
        for (int i = 0; i < comm_rank * (*local_hidden_vect_size); i++)
        {
            new_entry = last_entry + entry;
            last_entry = entry;
            entry = new_entry;
        }

        for (auto &it : local_index_map_hidden)
        {
            new_entry = last_entry + entry;
            last_entry = entry;
            entry = new_entry;
            it.index = new_entry;
            it.varid = 2;
            cout << "index_map_entry (hidden): " << it.varid << ':' << it.index << std::endl;
        }
        PDI_release("local_hidden_vect_size");
        PDI_expose("local_index_map_hidden", local_index_map_hidden.data(), PDI_INOUT);
    }
// END INDEX MAP HIDDEN

    fill(state1.begin(), state1.end(), 0);
    printf("offset %d on rank %d \n", offsets[comm_rank], comm_rank);


    static bool is_first_timestep = true;
    int nsteps = 1;
    do
    {
        double* secret_state;
        if (secret_state_status == PDI_OK) {
            // advance secret state
            PDI_access("secret_state", (void**)&secret_state, PDI_INOUT);
            secret_state[0]++;
            secret_state[1]++;
        }
        for (int step = 0; step < nsteps; step++)
        {
            int i = 0;
            for (auto it = state1.begin(); it != state1.end(); it++)
            {
                *it += offsets[comm_rank] + i;
                if (secret_state_status == PDI_OK) {
                    *it += secret_state[0] + secret_state[1];
                }
                i++;
            }
        }
        if (secret_state_status == PDI_OK) {
            PDI_release("secret_state");
        }
        
        ofstream df {"/home/kacper/debugsim.log." + std::to_string(comm_rank) , ios::app};
        df << nsteps << ": ";
        for(auto x : state1) {
            df << x << " ";
        }
        df << std::endl;
        df.close();

        usleep(1000000); // 1 sec
        PDI_expose("variableX", state1.data(), PDI_INOUT);
        PDI_expose("nsteps", &nsteps, PDI_IN);
        printf("nsteps: %d\n", nsteps);
        if (nsteps > 0 && is_first_timestep)
        {
            int timestep;
            PDI_expose("current_step", &timestep, PDI_IN);
            printf("First timestep to propagate: %d\n", timestep);
            is_first_timestep = false;
        }

        // if (secret_state_status == PDI_OK) {
        //     // Recover secret/hidden state from current_step
        //     int step;
        //     PDI_expose("current_step", &step, PDI_IN);
        //     PDI_access("secret_state", (void**)&secret_state, PDI_INOUT);
        //     for (int i = 1; i < step; ++i)
        //     {
        //         // advance secret state:
        //         secret_state[0]++;
        //         secret_state[1]++;
        //     }
        //     PDI_release("secret_state");
        // }

        // TODO does not work if we remove this for reasons.... (it will schedule many many things as simulation ranks finish too independently!
        MPI_Barrier(MPI_COMM_WORLD);

        // file output of allways ensemble member 0
        // TODO: maybe move this functionality into api?
        int current_state_id;
        PDI_expose("current_state_id", &current_state_id, PDI_IN);
        printf("nsteps: %d, current_state_id: %d\n", nsteps, current_state_id);
        if (nsteps > 0 && current_state_id == 1)
        {
            // raise(SIGINT);
            if (comm_rank == 0)
            {
                ofstream myfile;
                // 1 is the smallest time step.
                int current_step;
                PDI_expose("current_step", &current_step, PDI_IN);
                if (current_step == 1)
                {
                    // remove old file...
                    myfile.open ("output.txt", ios::trunc);
                }
                else
                {
                    myfile.open ("output.txt", ios::app);
                }
                vector<double> full_state(GLOBAL_VECT_SIZE);

                MPI_Gatherv(state1.data(), state1.size(),
                            MPI_DOUBLE,
                            full_state.data(), counts.data(),
                            offsets.data(),
                            MPI_DOUBLE, 0, MPI_COMM_WORLD);
                for (auto it = full_state.begin(); it !=
                     full_state.end(); it++)
                {
                    myfile << *it << ",";
                }
                myfile << "\n";
                myfile.close();
            }
            else
            {
                MPI_Gatherv(state1.data(), state1.size(),
                            MPI_DOUBLE,
                            NULL, NULL, NULL,
                            MPI_DOUBLE, 0, MPI_COMM_WORLD);
            }
        }
    } while (nsteps > 0);

    auto exe_end = std::chrono::steady_clock::now();

    PDI_finalize();
    int ret = MPI_Finalize();

    auto fin_end = std::chrono::steady_clock::now();
    std::chrono::duration<double> init_time = init_end - init_start;
    std::chrono::duration<double> exe_time = exe_end - init_end;
    std::chrono::duration<double> fin_time = fin_end - exe_end;
    std::chrono::duration<double> total_time = init_time + exe_time + fin_time;
    std::chrono::duration<double> total_check = fin_end - init_start;

    std::ofstream myfile;
    myfile.open ("time_measures.csv", std::ios::out | std::ios::app);
    myfile << init_time.count() << "," << exe_time.count() << "," << fin_time.count() << "," << total_time.count() << "," << total_check.count() << "," << "\n";
    myfile.close();

    assert(ret == MPI_SUCCESS);
}


