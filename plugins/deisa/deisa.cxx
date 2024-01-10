/******************************************************************************
 * Copyright (c) 2020-2022 Centre national de la recherche scientifique (CNRS)
 * Copyright (c) 2020-2022 Commissariat a l'énergie atomique et aux énergies alternatives (CEA)
 * Copyright (c) 2020-2022 Institut national de recherche en informatique et en automatique (Inria)
 * Copyright (c) 2020-2022 Université Paris-Saclay
 * Copyright (c) 2020-2022 Université de Versailles Saint-Quentin-en-Yvelines
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

using PDI::Array_datatype;
using PDI::Context;
using PDI::Config_error;
using PDI::each;
using PDI::Datatype;
using PDI::Datatype_sptr;
using PDI::Datatype_template;
using PDI::Datatype_template_ptr;
using PDI::Error;
using PDI::Expression;
using PDI::len;
using PDI::Plugin;
using PDI::Ref;
using PDI::Ref_rw;
using PDI::Ref_r;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;
using PDI::to_string;
using PDI::Type_error;
using pydict = pybind11::dict;
using pymod = pybind11::module;
using pyobj = pybind11::object;
using namespace pybind11::literals;
using std::cerr;
using std::endl;
using std::exception;
using std::string;
using std::unordered_multimap;
using std::unordered_map;
using std::vector;

pybind11::dtype datatype_to_pydtype(std::shared_ptr<const Scalar_datatype> scalar_type)
{
    switch (scalar_type->kind()) {
    case Scalar_kind::FLOAT: {
      switch (scalar_type->datasize()) {
      case sizeof(float): return pybind11::dtype::of<float>();
      case sizeof(double): return pybind11::dtype::of<double>();
      default: throw Type_error{"Unable to pass {} bytes floating point value to python", scalar_type->datasize()};
      }
    } break;
    case Scalar_kind::SIGNED: {
      switch (scalar_type->datasize()) {
      case sizeof(int8_t): return pybind11::dtype::of<int8_t>();
      case sizeof(int16_t): return pybind11::dtype::of<int16_t>();
      case sizeof(int32_t): return pybind11::dtype::of<int32_t>();
      case sizeof(int64_t): return pybind11::dtype::of<int64_t>();
      default: throw Type_error{"Unable to pass {} bytes integer value to python", scalar_type->datasize()};
      }
    } break;
    case Scalar_kind::UNSIGNED: {
      switch (scalar_type->datasize()) {
      case sizeof(uint8_t): return pybind11::dtype::of<uint8_t>();
      case sizeof(uint16_t): return pybind11::dtype::of<uint16_t>();
      case sizeof(uint32_t): return pybind11::dtype::of<uint32_t>();
      case sizeof(uint64_t): return pybind11::dtype::of<uint64_t>();
      default: throw Type_error{"Unable to pass {} bytes unsigned integer value to python", scalar_type->datasize()};
      }
    } break;
    default: throw Type_error{"Unable to pass value of unexpected type to python"};
    }
  }


/** The deisa plugin
 */
class deisa_plugin:
  public Plugin
{
  //Determine if python interpreter is initialized by the plugin.
  bool interpreter_initialized_in_plugin = false;
  Expression scheduler_info;
  unordered_map<string, Datatype_template_ptr> deisa_arrays;
  unordered_map<string, string> deisa_map_ins;
  Expression rank ;
  Expression size ;
  Expression time_step;
  Expression feedback;

  int feedback2 = 7777;

public:
  static std::pair<std::unordered_set<std::string>, std::unordered_set<std::string>> dependencies()
  {
      return {{"mpi"},{"mpi"}};
  }
  void init_deisa()
  {
    unordered_map<string, unordered_map<string, std::vector<size_t>>> darrs ;
    unordered_map<string, pybind11::dtype> darrs_dtype ;
    for (auto&& key_value : deisa_arrays) {
      unordered_map<string, std::vector<size_t>> darr ;
      vector<size_t> sizes;
      vector<size_t> starts;
      vector<size_t> subsizes;
      vector<size_t> timedim ;
      string deisa_array_name = key_value.first;
      Datatype_sptr type_sptr = key_value.second->evaluate(context());
      timedim.emplace_back(key_value.second->attribute("timedim").to_long(context()));
      // get info from datatype
      while (auto&& array_type = std::dynamic_pointer_cast<const PDI::Array_datatype>(type_sptr)) {
          sizes.emplace_back(array_type->size());
          starts.emplace_back(array_type->start());
          subsizes.emplace_back(array_type->subsize());
          type_sptr = array_type->subtype();
      }
      darr["sizes"] = sizes ;
      darr["starts"] = starts ;
      darr["subsizes"] = subsizes ;
      darr["timedim"] = timedim ;
      darrs[deisa_array_name] = darr ;
      darrs_dtype[deisa_array_name] = datatype_to_pydtype(std::dynamic_pointer_cast<const Scalar_datatype>(type_sptr));
    }

    // a python context we fill with exposed variables
    pydict pyscope = pymod::import("__main__").attr("__dict__");
    pyscope["deisa"] = pymod::import("deisa");
    pymod deisa = pymod::import("deisa");
    pyscope["init"]  = deisa.attr("init");
    pyscope["scheduler_info"] = to_python(scheduler_info.to_ref(context()));
    pyscope["size"] = to_python(size.to_ref(context()));
    pyscope["rank"] = to_python(rank.to_ref(context()));
    pyscope["deisa_arrays"] = darrs ;
    pyscope["deisa_arrays_dtype"] = darrs_dtype ;
    try {
      pybind11::exec("bridge = init(scheduler_info, rank, size, deisa_arrays, deisa_arrays_dtype);", pyscope);
    } catch ( const std::exception& e ) {
      cerr << " *** [PDI/Deisa] Error: while initializating deisa, caught exception: "<<e.what()<<endl;
    } catch (...) {
      cerr << " *** [PDI/Deisa] Error: while initializating deisa, caught exception"<<endl;
    }
  }

  deisa_plugin(Context& ctx, PC_tree_t conf) : Plugin{ctx}
  {
    if ( ! Py_IsInitialized() ) {
      pybind11::initialize_interpreter();
      interpreter_initialized_in_plugin = true;
    }

    // init params
    each(conf, [&](PC_tree_t key_tree, PC_tree_t value) {
      string key = to_string(key_tree);
      if ( key == "scheduler_info" ) {
        scheduler_info = to_string(value);
      }else if (key=="deisa_arrays"){
        each(value, [&](PC_tree_t key_map, PC_tree_t value_map) {
          deisa_arrays.emplace(to_string(key_map), ctx.datatype(value_map));
        });
      }else if ( key == "time_step" ) {
        time_step = to_string(value);
      }else if (key=="map_in"){
                        //
      }else if (key=="logging" || key=="init_on"){
        //
      } else if (key == "feedback") {
//          feedback = to_string(value);
      } else {
        throw Config_error{key_tree, "Unknown key in Deisa file configuration: `{}'", key};
      }
    });

    int s ;

    rank = Expression{Ref_r{ctx.desc("MPI_COMM_WORLD_rank").ref()}.scalar_value<long>()};
    MPI_Comm comm = *static_cast<const MPI_Comm*>(Ref_r{ctx.desc("MPI_COMM_WORLD").ref()}.get());
    MPI_Comm_size(comm, &s);
    size = Expression{static_cast<long>(s)};
    // init step
    PC_tree_t init_tree = PC_get(conf, ".init_on");
    if (!PC_status(init_tree)) {
      ctx.callbacks().add_event_callback([this](const std::string&)mutable{this->init_deisa();},to_string(init_tree));
    }else{
      throw Config_error{conf, "Deisa plugin requires init_on key "};
    }

    //map_in passes
    PC_tree_t map_tree = PC_get(conf, ".map_in");
    if (!PC_status(map_tree)) {
      each(map_tree, [&](PC_tree_t key_map, PC_tree_t value_map) {
        //deisa_map_ins.emplace({to_string(key_map), to_string(value_map)});
        ctx.callbacks().add_data_callback([&ctx, timestep_exp = time_step ,deisa_array_name = to_string(value_map)](const string&, Ref data_ref){
          // a python context we fill with exposed variables
          size_t timestep = timestep_exp.to_long(ctx);
          pydict pyscope = pymod::import("__main__").attr("__dict__");
          pyscope[deisa_array_name.c_str()] = to_python(data_ref);
          pyscope["time_step"] = timestep;
          pyscope["name"] = deisa_array_name.c_str();
          try {
//            pybind11::exec(fmt::format("bridge.publish_data({},name,time_step)",deisa_array_name), pyscope);

//            auto res = pybind11::eval(fmt::format("bridge.publish_data({},name,time_step)",deisa_array_name), pyscope);
//            if(not pybind11::isinstance<pybind11::none>(res)) {
//                std::cout << ">>>>>>>>>>> returned value " << res << std::endl;
//                // TODO: how do I change a PDI variable ?
//                // TODO: alert user that there is something in the feedback loop
//
//                ctx.callbacks().call_event_callbacks("deisa-feedback");
//
//                ctx.callbacks().call_data_callbacks("deisa-feedback", Ref(res));
//            }

            pyscope[deisa_array_name.c_str()]=NULL;
          } catch ( const std::exception& e ) {
            cerr << " *** [PDI/Deisa] Error: while publishing data through deisa , caught exception: "<<e.what()<<endl;
          } catch (...) {
            cerr << " *** [PDI/Deisa] Error: while publishing data through deisa, caught exception"<<endl;
          }
        }, to_string(key_map));
      });
    }

    ctx.callbacks().add_event_callback([&](const string&){

            int i = 0;
            void *ptr = &i;

            // read
//            PDI_access("feedback", &ptr, PDI_IN);
//            PDI_release("feedback");
//            std::cout << ">>>>>>>>>>> feedback=" << *static_cast<int*>(ptr) << std::endl;

//            int feedback = 0;
//            PDI_expose("feedback", &feedback, PDI_IN);
//            std::cout << ">>>>>>>>>>> feedback=" << feedback << std::endl;

            // write
            PDI_expose("feedback2", &feedback2, PDI_OUT);

//            // read
//            int feedback3 = 0;
//            void *ptr2 = &feedback3;
////            PDI_expose("feedback2", &feedback3, PDI_IN);
////            std::cout << ">>>>>>>>>>> feedback3=" << feedback3 << std::endl;
//            PDI_access("feedback2", &ptr2, PDI_IN);
//            PDI_release("feedback2");
//            std::cout << ">>>>>>>>>>> feedback3=" << *static_cast<int*>(ptr2) << std::endl;


//        pydict pyscope = pymod::import("__main__").attr("__dict__");
//        auto ret = pybind11::eval("bridge.poll_for_feedback_data()", pyscope);
//
//        if(not pybind11::isinstance<pybind11::none>(ret)) {
//            std::cout << ">>>>>>>>>>> returned value " << ret << std::endl;
////            PDI_expose("feedback", &ret, PDI_INOUT);
//
//            int i = ret.cast<int>();
//            void *ptr = &i;
//
//            PDI_access("feedback", &ptr, PDI_IN);
//            PDI_release("feedback");
//        }

    }, "deisa-feedback-poll");

  }
  ~deisa_plugin()
  {
    if (interpreter_initialized_in_plugin) pybind11::finalize_interpreter();
  }

}; // class deisa_plugin

} // namespace <anonymous>


PDI_PLUGIN(deisa)
