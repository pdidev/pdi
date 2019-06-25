/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <functional>
#include <iostream>
#include <string>
#include <map>

#include <spdlog/spdlog.h>

#include <pdi.h>
#include <pdi/context.h>
#include <pdi/logger.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>
#include <pdi/datatype.h>
#include <pdi/scalar_datatype.h>
#include <pdi/array_datatype.h>
#include <pdi/expression.h>

#include <csignal>

//#include <mpi.h>
#include <melissa_api.h>

namespace {

  using PDI::Context;
  using PDI::Ref;
  using PDI::Ref_r;
  using PDI::Error;
  using PDI::Plugin;
  using PDI::Array_datatype;
  using PDI::Scalar_datatype;
  using PDI::to_string;
  using PDI::each;
  using PDI::Datatype;
  using PDI::Datatype_uptr;
  using PDI::Expression;
  using std::bind;
  using std::reference_wrapper;
  using std::string;
  using std::map;
  using std::pair;


  struct melissa_plugin: Plugin {

    /**
     * As There is a ?BUG? in PDI making it impossible to load data descriptors in the
     * standard init event (add_init_callback) we need to init melissa later, when the
     * first data event occurs. To not init twice we need this flag:
     */
    bool m_inited = false;

    map<string,size_t> m_known_variables;

    /** Set-up the plugin-specific logger
     *
     * \param logging_tree the logging specific config
     */
    void set_up_logger(PC_tree_t logging_tree)
    {
      context().logger()->set_pattern("[PDI][Melissa][%T] *** %^%l%$: %v");

      int mpi_init = 0;
      MPI_Initialized(&mpi_init);
      if (mpi_init) {
        //set up format
        int world_rank;
        MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
        char format[64];
        snprintf(format, 64, "[PDI][Melissa][%06d][%%T] *** %%^%%l%%$: %%v", world_rank);
        context().logger()->set_pattern(string(format));
      }
    }

    /**
     * Set-up melissa for the variable
     * This must be called for every dataset.
     * Hereby each data represensts one vector
     */
    size_t init_variable(PC_tree_t& tree, const string& name)
    {
      const Datatype_uptr field_datatype =
        context()[name].default_type()->evaluate(context());
      const Array_datatype* array_datatype =
        dynamic_cast<Array_datatype*>(field_datatype.get());

      const Scalar_datatype* scalar_datatype;

      size_t local_vect_size = 0;

      if (array_datatype) {
        local_vect_size = array_datatype->subsize();
        scalar_datatype = dynamic_cast<const Scalar_datatype*>(&array_datatype->subtype());
      } else {
        throw Error{PDI_ERR_TYPE,
          "Wrong Type in transmitted array. Must be 1D double Array!",
        };
      }

      if (!scalar_datatype) {
        throw Error{PDI_ERR_TYPE,
          "Wrong Type in transmitted array. Must be 1D double Array!",
        };
      }

      typecheck(*array_datatype, *scalar_datatype, local_vect_size);
      context().logger()->info("local_vect_size = {}", local_vect_size);

      Expression communicator = to_string(PC_get(tree, ".communicator"));
      Ref_r comm_ref{communicator.to_ref(context())};

      MPI_Comm comm = MPI_COMM_SELF;
      comm = *(static_cast<const MPI_Comm*>(
            comm_ref.get()));


      melissa_init_mpi(name.c_str(),
          local_vect_size,
          comm,
          MELISSA_COUPLING_ZMQ);

      return local_vect_size;
    }

    PC_tree_t m_send_tree;
    void init(void) {

      for (auto it = m_known_variables.begin(); it != m_known_variables.end(); ++it) {
        string path = "." + it->first;
        auto value = PC_get(m_send_tree, path.c_str());
        it->second = init_variable(value, it->first);
      }
    }

    melissa_plugin(Context& ctx, PC_tree_t config):
      Plugin {ctx}
    {
      set_up_logger(PC_get(config, ".logging"));

      m_send_tree = PC_get(config, ".send");

      each(m_send_tree, [&](PC_tree_t key_tree, PC_tree_t value) {
          string name = to_string(key_tree);
          m_known_variables.insert(pair<string, size_t>(name, 0));
          });

      ctx.add_data_callback([this](const string& name, Ref ref) {
          this->data(name, ref);
          });

      // we do not react to other events so far...

      context().logger()->info("Welcome!");
    }

    ~melissa_plugin()
    {
      melissa_finalize();
      context().logger()->info("Goodbye!");
    }


    /**
     * Performs some type checks on the received data
     */
    void typecheck(const Array_datatype& array_datatype, const Scalar_datatype& scalar_datatype, const size_t len)
    {
      //assert(datatype.debug_string() == "double");
      assert(array_datatype.simple());
      assert(array_datatype.subsize() == len);

      assert(scalar_datatype.simple());
      assert(scalar_datatype.datasize() == 8);  // best we can do to guarantee double
    }

    void data(const string& name, const Ref_r& ref)
    {
      auto known = m_known_variables.find(name);
      if (known != m_known_variables.end()) {
        // Init on first known variables. So we can be sure that all meta data was received already.
        if (!m_inited) {
          init();
          m_inited = true;
        }

        if (ref)
        {
          // Check if data is good...
          // if it is not an array of doubles these casts should fail:
          const Array_datatype& array_datatype = dynamic_cast<const Array_datatype&>(ref.type());
          const Scalar_datatype& scalar_datatype = dynamic_cast<const Scalar_datatype&>(array_datatype.subtype());
          context().logger()->info("Receiving Data for ({}) Data type : {}", name,
              array_datatype.debug_string());

          typecheck(array_datatype, scalar_datatype, known->second);
          const double * datas = static_cast<const double*>(ref.get());

          //raise(SIGINT);

          melissa_send(name.c_str(), datas);
        } else {
          throw Error{PDI_ERR_TYPE,
            "Wrong Type in transmitted array. Must be 1D double Array!",
            ref.type().debug_string()};
        }
      }
    }
  }; // struct melissa_plugin

} // namespace <anonymous>

PDI_PLUGIN(melissa)
