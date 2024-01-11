/******************************************************************************
 * Copyright (c) 2020-2024 Centre national de la recherche scientifique (CNRS)
 * Copyright (c) 2020-2024 Commissariat a l'énergie atomique et aux énergies alternatives (CEA)
 * Copyright (c) 2020-2024 Institut national de recherche en informatique et en automatique (Inria)
 * Copyright (c) 2020-2024 Université Paris-Saclay
 * Copyright (c) 2020-2024 Université de Versailles Saint-Quentin-en-Yvelines
 *
 * SPDX-License-Identifier: MIT
 *
 *****************************************************************************/

#include <iostream>
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <pybind11/pybind11.h>
#include <pybind11/embed.h>
#include <pybind11/numpy.h>
#include <pybind11/stl.h>

#include <mpi.h>
#include <pdi.h>
#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/data_descriptor.h>
#include <pdi/datatype.h>
#include <pdi/expression.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>
#include <pdi/scalar_datatype.h>
#include <pdi/python/tools.h>

namespace {

using namespace PDI;
using namespace pybind11::literals;
using pydict = pybind11::dict;
using pymod = pybind11::module;
using pyobj = pybind11::object;


pybind11::dtype datatype_to_pydtype(const std::shared_ptr<const Scalar_datatype>& scalar_type) {
    switch (scalar_type->kind()) {
        case Scalar_kind::FLOAT: {
            switch (scalar_type->datasize()) {
                case sizeof(float):
                    return pybind11::dtype::of<float>();
                case sizeof(double):
                    return pybind11::dtype::of<double>();
                default:
                    throw Type_error{"Unable to pass {} bytes floating point value to python", scalar_type->datasize()};
            }
        }
            break;
        case Scalar_kind::SIGNED: {
            switch (scalar_type->datasize()) {
                case sizeof(int8_t):
                    return pybind11::dtype::of<int8_t>();
                case sizeof(int16_t):
                    return pybind11::dtype::of<int16_t>();
                case sizeof(int32_t):
                    return pybind11::dtype::of<int32_t>();
                case sizeof(int64_t):
                    return pybind11::dtype::of<int64_t>();
                default:
                    throw Type_error{"Unable to pass {} bytes integer value to python", scalar_type->datasize()};
            }
        }
            break;
        case Scalar_kind::UNSIGNED: {
            switch (scalar_type->datasize()) {
                case sizeof(uint8_t):
                    return pybind11::dtype::of<uint8_t>();
                case sizeof(uint16_t):
                    return pybind11::dtype::of<uint16_t>();
                case sizeof(uint32_t):
                    return pybind11::dtype::of<uint32_t>();
                case sizeof(uint64_t):
                    return pybind11::dtype::of<uint64_t>();
                default:
                    throw Type_error{"Unable to pass {} bytes unsigned integer value to python", scalar_type->datasize()};
            }
        }
            break;
        default:
            throw Type_error{"Unable to pass value of unexpected type to python"};
    }
}


/** The deisa plugin
*/
class deisa_plugin : public Plugin {
    bool interpreter_initialized_in_plugin = false; // Determine if python interpreter is initialized by the plugin
    Expression scheduler_info;
    std::unordered_map<std::string, Datatype_template_ptr> deisa_arrays;
    std::unordered_map<std::string, std::string> deisa_map_ins;
    Expression rank;
    Expression size;
    Expression time_step;

public:
    static std::pair<std::unordered_set<std::string>, std::unordered_set<std::string>> dependencies() {
        return {{"mpi"}, {"mpi"}};
    }


    deisa_plugin(Context &ctx, PC_tree_t conf) : Plugin{ctx} {
        if (!Py_IsInitialized()) {
            pybind11::initialize_interpreter();
            interpreter_initialized_in_plugin = true;
        }

        // init params
        each(conf, [&](PC_tree_t key_tree, PC_tree_t value) {
            std::string key = to_string(key_tree);
            if (key == "scheduler_info") {
                scheduler_info = to_string(value);
            } else if (key == "deisa_arrays") {
                each(value, [&](PC_tree_t key_map, PC_tree_t value_map) {
                    deisa_arrays.emplace(to_string(key_map), ctx.datatype(value_map));
                });
            } else if (key == "time_step") {
                time_step = to_string(value);
            }
            else if (key == "map_in") {
                //
            } else if (key == "logging" || key == "init_on") {
                //
            } else {
                throw Config_error{key_tree, "Unknown key in Deisa file configuration: '{}'", key};
            }
        });


        int mpi_size;
        rank = Expression{Ref_r{ctx.desc("MPI_COMM_WORLD_rank").ref()}.scalar_value<long>()};
        MPI_Comm comm = *static_cast<const MPI_Comm *>(Ref_r{ctx.desc("MPI_COMM_WORLD").ref()}.get());
        MPI_Comm_size(comm, &mpi_size);
        size = Expression{static_cast<long>(mpi_size)};


        // plugin init
        PC_tree_t init_tree = PC_get(conf, ".init_on");
        if (!PC_status(init_tree)) {
            ctx.callbacks().add_event_callback([&](const std::string &) {
                init_deisa();
            }, to_string(init_tree));
        } else {
            throw Config_error{conf, "Deisa plugin requires init_on key "};
        }

        // map_in
        PC_tree_t map_tree = PC_get(conf, ".map_in");
        if (!PC_status(map_tree)) {
            each(map_tree, [&](PC_tree_t key_map, PC_tree_t value_map) {
//                deisa_map_ins.emplace({to_string(key_map), to_string(value_map)});

                ctx.callbacks().add_data_callback(
                        [&, deisa_array_name = to_string(value_map)](const std::string &, const Ref& data_ref) {
                            try {
                                // start a python context and call bridge.publish_data(...)
                                pydict pyscope = pymod::import("__main__").attr("__dict__");
                                pyscope[deisa_array_name.c_str()] = to_python(data_ref);
                                pyscope["time_step"] = time_step.to_long(ctx);
                                pyscope["name"] = deisa_array_name.c_str();

                                pybind11::exec("bridge.publish_data(" + deisa_array_name + ", name, time_step)", pyscope);

                                pyscope[deisa_array_name.c_str()] = NULL;   // TODO: is this needed ?
                            } catch (const std::exception &e) {
                                std::cerr << " *** [PDI/Deisa] Error: while publishing data, caught exception: " << e.what() << std::endl;
                            } catch (...) {
                                std::cerr << " *** [PDI/Deisa] Error: while publishing data." << std::endl;
                            }
                        }, to_string(key_map));
            });
        }
    }

    ~deisa_plugin() noexcept override {
            try {
                if (interpreter_initialized_in_plugin) {
                    pybind11::finalize_interpreter();
                }
            } catch (const std::exception &e) {
                std::cerr << " *** [PDI/Deisa] Error in destructor, caught exception: " << e.what() << std::endl;
            } catch (...) {
                std::cerr << " *** [PDI/Deisa] Error in destructor. " << std::endl;
            }
    }

private:
    void init_deisa() {
        std::unordered_map<std::string, std::unordered_map<std::string, std::vector<size_t>>> darrs;
        std::unordered_map<std::string, pybind11::dtype> darrs_dtype;
        for (auto &&key_value: deisa_arrays) {
            std::unordered_map<std::string, std::vector<size_t>> darr;
            std::vector<size_t> sizes;
            std::vector<size_t> starts;
            std::vector<size_t> subsizes;
            std::vector<size_t> timedim;
            std::string deisa_array_name = key_value.first;
            Datatype_sptr type_sptr = key_value.second->evaluate(context());
            timedim.emplace_back(key_value.second->attribute("timedim").to_long(context()));
            // get info from datatype
            while (auto &&array_type = std::dynamic_pointer_cast<const PDI::Array_datatype>(type_sptr)) {
                sizes.emplace_back(array_type->size());
                starts.emplace_back(array_type->start());
                subsizes.emplace_back(array_type->subsize());
                type_sptr = array_type->subtype();
            }
            darr["sizes"] = sizes;
            darr["starts"] = starts;
            darr["subsizes"] = subsizes;
            darr["timedim"] = timedim;
            darrs[deisa_array_name] = darr;
            darrs_dtype[deisa_array_name] = datatype_to_pydtype(std::dynamic_pointer_cast<const Scalar_datatype>(type_sptr));
        }

        try {
            // setup python context and instantiate bridge
            pydict pyscope = pymod::import("__main__").attr("__dict__");
            pyscope["deisa"] = pymod::import("deisa");
            pymod deisa = pymod::import("deisa");
            pyscope["init"] = deisa.attr("init");
            pyscope["scheduler_info"] = to_python(scheduler_info.to_ref(context()));
            pyscope["size"] = to_python(size.to_ref(context()));
            pyscope["rank"] = to_python(rank.to_ref(context()));
            pyscope["deisa_arrays"] = darrs;
            pyscope["deisa_arrays_dtype"] = darrs_dtype;

            pybind11::exec("bridge = init(scheduler_info, rank, size, deisa_arrays, deisa_arrays_dtype);", pyscope);
        } catch (const std::exception &e) {
            std::cerr << " *** [PDI/Deisa] Error: while initializing deisa, caught exception: " << e.what() << std::endl;
        } catch (...) {
            std::cerr << " *** [PDI/Deisa] Error: while initializing deisa" << std::endl;
        }
    }

}; // class deisa_plugin

} // namespace <anonymous>


PDI_PLUGIN(deisa)
