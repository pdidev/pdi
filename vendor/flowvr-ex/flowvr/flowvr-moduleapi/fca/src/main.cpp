/**
 * This file is part of the FCA (FlowVR C API).
 *
 * The FCA is  free software: you can redistribute it  and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either  version 3 of the  License, or (at your  option) any later
 * version.
 *
 * The FCA is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
 * A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy  of the GNU General Public License along with
 * the FCA.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Authors:
 *  Julien Fontanet <julien.fontanet@isonoe.net>
 */

#include <fca.h>


////////////////////////////////////////
// Standard includes and namespaces.

#include <cassert>
#include <cstdarg>
#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>

using std::vector;


////////////////////////////////////////
// FlowVR includes and namespaces.

#include <flowvr/allocator.h>
#include <flowvr/bufferpool.h>
#include <flowvr/module.h>
#include <flowvr/parallel.h>
#include <flowvr/xml.h>

using flowvr::BaseType;
using flowvr::Buffer;
using flowvr::BufferWrite;
using flowvr::Port;
using flowvr::InputPort;
using flowvr::OutputPort;
using flowvr::Trace;
using flowvr::TypedTrace;
using flowvr::Parallel;
using flowvr::StampInfo;

using flowvr::TypeArray;
using flowvr::TypeBinary;
using flowvr::TypeFloat;
using flowvr::TypeInt;
using flowvr::TypeString;

//using flowvr::xml;


////////////////////////////////////////
// C++ stuff.

namespace
{
	//--------------------------------------
	// Generic utilities.

	static
	void
	not_implemented()
	{
		std::cerr << "Not implemented.\n";
		std::abort();
	}

	static
	void
	not_allowed(std::string msg) {
		std::cerr << msg << std::endl;
	}

	//--------------------------------------
	// Decorators for FlowVR classes.

	/**
	 * This class should be inherited by all fca objects.
	 *
	 * For the moment, it allows all objects to be deleted uniformly.
	 */
	struct Base
	{
		virtual ~Base()
		{}
	};

	/**
	 * This  interface must  be implemented  by any  classes which  can allocate
	 * buffers (“Module” and “BufferPool”).
	 *
	 * /!\: This is not directly related to FlowVR's allocators.
	 */
	struct Allocator : Base
	{
		virtual
		BufferWrite
		alloc(size_t size) = 0;
	};

	/**
	 * Decorator for flowvr::ModuleAPI.
	 *
	 * This class  also owns the ports  and will consequently frees  them in its
	 * destructor.
	 */
	class Module : public Allocator
	{
	public:

        Module(flowvr::ModuleAPI *api, vector<Port *> ports, vector<Trace *> traces, std::string modulename = std::string(""))
            : _api(api), _ports(ports), _traces(traces), _modulename(modulename)
		{
			//assert(api != NULL);
		}

		~Module()
		{
			for (vector<Port *>::iterator
				     it = _ports.begin(),
				     end = _ports.end();
			     it != end;
			     ++it)
			{
				delete *it;
			}

			for (vector<Trace *>::iterator
                                     it = _traces.begin(),
                                     end = _traces.end();
                             it != end;
                             ++it)
                        {
                                delete *it;
                        }

			_api->close();
			delete _api;
		}

		bool initModule(){
			assert(_api == NULL);
            std::cout<<"Initialisation du module "<<_modulename <<" avec "<<_ports.size()<<" ports et "<<_traces.size()<<" traces."<<std::endl;

            _api = flowvr::initModule(_ports,_traces,_modulename);
			return _api != NULL;
		}

		void appendPort(Port *p){
			assert(_api == NULL);
			assert(p != NULL);
			_ports.push_back(p);
		}

        void setModulename(const char *modulename){
            _modulename = std::string(modulename);
        }

		void appendTrace(Trace *t){
			assert(_api == NULL);
			assert(t != NULL);
			_traces.push_back(t);
		}

		BufferWrite
		alloc(size_t size)
		{
			return _api->alloc(size);
		}

		flowvr::Allocator *
		getAllocator()
		{
			return _api->getAllocator();
		}

		Port *
		getPortByName(const std::string &name)
		{
			return _api->getPortByName(name);
		}

		Trace *
		getTraceByName(const std::string &name)
		{
			return _api->getTraceByName(name);
		}

		bool
		wait()
		{
			return (_api->wait() != 0);
		}

    void
    abort()
    {
      return _api->abort();
    }

	private:

		flowvr::ModuleAPI *_api;

		vector<Port *> _ports;
		vector<Trace*> _traces;
        std::string _modulename;
	};


	/**
	 * These  classes  allows  to  access  uniformly  read-only  and  read-write
	 * messages.
	 */
	struct Message : Base
	{
		/**
		 * This  method returns  a pointer  to the  buffer associated  with this
		 * message.
		 */
		virtual
		flowvr::Buffer &
		get_buffer() = 0;
	};
	struct MessageRead : Message, flowvr::Message
	{
		flowvr::Buffer &
		get_buffer()
		{
			return data;
		}
	};
	struct MessageWrite : Message, flowvr::MessageWrite
	{
		flowvr::Buffer &
		get_buffer()
		{
			return data;
		}
	};
	struct MessagePut : Message, flowvr::MessagePut
	{
		flowvr::Buffer &
		get_buffer()
		{
			return data;
		}
	};

	/**
	 * This class  is equivalent to  “flowvr::BufferPool” but contains  also the
	 * associated allocator.
	 */
	class Pool : public Allocator
	{
	public:

		Pool(flowvr::Allocator *allocator, size_t capacity)
			: _allocator(allocator), _bp(capacity)
		{
			assert(allocator != NULL);
		}

		BufferWrite
		alloc(size_t size)
		{
			// Do not exceed the capacity.
			return _bp.alloc(_allocator, size, true);
		}

		size_t
		capacity() const
		{
			return _bp.getMaxBuffer();
		}

		void
		capacity(size_t capacity)
		{
			_bp.setMaxBuffer(capacity);
		}

	private:

		flowvr::Allocator *_allocator;

		flowvr::BufferPool _bp;
	};

	//--------------------------------------
	// Utilities related to the conversion between FlowVR's types and FCA's.

	static
	BaseType *
	get_flowvr_type(fca_stamp_type type, std::va_list vl)
	{
		switch (type)
		{
		case fca_ARRAY:
			{
				size_t size = va_arg(vl, size_t);
				fca_stamp_type
					next_type = static_cast<fca_stamp_type>(va_arg(vl, int));
				BaseType *type = get_flowvr_type(next_type, vl);
                printf("Creation d'un stamp array de taille %lu et type %i\n", size, next_type);
				return TypeArray::create(size, type);
			}
		case fca_BINARY:
			{
				size_t size = va_arg(vl, size_t);
				return TypeBinary::create(size);
			}
		case fca_FLOAT:
			return TypeFloat::create();
		case fca_INT:
			return TypeInt::create();
		case fca_STRING:
			return TypeString::create();
		}

		return NULL;
	}

	static
	fca_stamp_type
	get_fca_type(const BaseType *type)
	{
		assert(type != NULL);

		if (dynamic_cast<const TypeArray *>(type) != NULL)
		{
			return fca_ARRAY;
		}
		if (dynamic_cast<const TypeBinary *>(type) != NULL)
		{
			return fca_BINARY;
		}
		if (dynamic_cast<const TypeFloat *>(type) != NULL)
		{
			return fca_FLOAT;
		}
		if (dynamic_cast<const TypeInt *>(type) != NULL)
		{
			return fca_INT;
		}

		assert(dynamic_cast<const TypeString *>(type) != NULL);
		return fca_STRING;
	}

	/**
	 * Computes the size needed to store the content of a type.
	 *
	 * We do not use “flowvr::BaseType::size()” because we store only a “char *”
	 * for while FlowVR additionally stores the length of the string.
	 */
	static
	size_t
	get_type_size(const BaseType *type)
	{
		assert(type != NULL);

		if (const TypeArray *t = dynamic_cast<const TypeArray *>(type))
		{
			return (t->n * get_type_size(t->t));
		}
		if (dynamic_cast<const TypeString *>(type) != NULL)
		{
			return sizeof(char *);
		}

		return type->size();
	}

	//--------------------------------------
	// Stamp related utilities.

	template <typename T>
	static
	void
	read_stamp(const T &stamps, const StampInfo *stamp, void* &result)
	{
		assert(stamp != NULL);
		assert(result != NULL);

		int bufferSize;
		char* stampByteBuffer;
        const flowvr::TypeArray *type_array = dynamic_cast<const flowvr::TypeArray*>(stamp->getType());
        float *result_float;
        int *result_int;

		switch (get_fca_type(stamp->getType()))
		{
            case fca_ARRAY:
                bufferSize = stamp->getSize();
                if(type_array){
                    fca_stamp_type type_fca_array = get_fca_type(type_array->t);
                    switch (type_fca_array) {
                        case fca_FLOAT:
                            result_float = static_cast<float*>(result);
                            for(int i = 0; i < type_array->n; i++){
                                float data;
                                stamps.read((*stamp)[i],data);
                                result_float[i] = data;

                            }
                            break;
                        case fca_INT:
                            result_int = static_cast<int*>(result);
                            for(int i = 0; i < type_array->n; i++){
                                int data;
                                stamps.read((*stamp)[i],data);
                                result_int[i] = data;
                            }
                            break;
                        default:
                            not_implemented();
                            assert(false);
                        }

                }
                else {
                    printf("Error : type fca is fca_ARRAY but couldn't cast in TypeArray\n");
                    assert(false);
                }
                break;
		case fca_BINARY:
			bufferSize = stamp->getSize();
			stampByteBuffer = (char*)stamps.readAccess();
			memcpy(result, stampByteBuffer, bufferSize);
			break;

		case fca_FLOAT:
			stamps.read(*stamp, *reinterpret_cast<float *>(result));
			break;
		case fca_INT:
			stamps.read(*stamp, *reinterpret_cast<int *>(result));
			break;
		case fca_STRING:
			{
				free(result);

				std::string tmp;
				stamps.read(*stamp, tmp);

				const size_t size = tmp.size();
				char *str = reinterpret_cast<char *>(std::malloc(sizeof(char)
				                                                 * (size + 1)));
				tmp.copy(str, size);
				str[size] = '\0';

				result = str;
				//*(reinterpret_cast<char **>(result)) = str;
			}
			break;
		default:
			assert(false);
		}
	}

	//--------------------------------------
	// Traits which associate C++ types to their C wrappers and vice versa.

	template <typename T1>
	struct box_traits
	{};

#	define BOX_TRAITS_1(BOX, ITEM) \
	template <> \
	struct box_traits<BOX> \
	{ \
		typedef ITEM item; \
	}
#	define BOX_TRAITS_2(BOX, ITEM) \
	template <> \
	struct box_traits<ITEM> \
	{ \
		typedef BOX box; \
	}
#	define BOX_TRAITS(BOX, ITEM) BOX_TRAITS_1(BOX, ITEM); BOX_TRAITS_2(BOX, ITEM)

	/* Here we  associate the  type of  the (C) box  to the  corresponding (C++)
	 * object and vice versa. */
	BOX_TRAITS(fca_message, Message);
	BOX_TRAITS(fca_module, Module);
	BOX_TRAITS(fca_pool, Pool);
	BOX_TRAITS(fca_port, Port);
	BOX_TRAITS(fca_stamp, StampInfo);
	BOX_TRAITS(fca_trace, Trace);

	/* For derivated classes we only associate them with the boxes of their base
	 * class. */
	BOX_TRAITS_2(fca_message, MessageRead);
	BOX_TRAITS_2(fca_message, MessageWrite);
	BOX_TRAITS_2(fca_message, MessagePut);
	BOX_TRAITS_2(fca_port, InputPort);
	BOX_TRAITS_2(fca_port, OutputPort);
	BOX_TRAITS_2(fca_trace, TypedTrace<int>);
	BOX_TRAITS_2(fca_trace, TypedTrace<float>);
	BOX_TRAITS_2(fca_trace, TypedTrace<std::string>);

#	undef BOX_TRAITS
#	undef BOX_TRAITS_2
#	undef BOX_TRAITS_1

	//--------------------------------------
	// Utilities to convert C++ objects to their C wrappers (box) and vice versa
	// (unbox).

	template <typename T>
	static
	typename box_traits<T>::box
	box(T *item)
	{
		return reinterpret_cast<typename box_traits<T>::box>(item);
	}

	template <typename T>
	static
	typename box_traits<T>::item *
	unbox(T box)
	{
		return reinterpret_cast<typename box_traits<T>::item *>(box);
	}

	template <typename Item, typename Box>
	static
	Item *
	unbox(Box box)
	{
		assert(dynamic_cast<Item *>(reinterpret_cast<Base *>(box)) == reinterpret_cast<Item *>(box));
		return reinterpret_cast<Item *>(box);
	}
} // anonymous namespace


////////////////////////////////////////
// C wrappers.

extern "C"
{
	//--------------------------------------
	// Stateful functions (not ideal for threading).

	void
	fca_init_parallel(size_t rank, size_t nprocs)
	{
		Parallel::init(rank, nprocs);
	}

	bool
	fca_init_module(fca_module mod)
	{
		return unbox(mod)->initModule();
	}

	//--------------------------------------
	// Generic functions.

  void
  fca_abort(fca_module module)
  {
    return unbox(module)->abort();
  }

	void
	fca_free(void *object)
	{
		delete reinterpret_cast<Base *>(object);
	}

	//--------------------------------------
	// Module functions.

	fca_module
	fca_new_module(const char *p, ...)
	{
		vector<Port *> ports;
		vector<Trace *> traces;
		{
			std::va_list vl;
			va_start(vl, p);
			const char *name = p;
			while (name != NULL)
			{
				int typeArg = va_arg(vl, int);
				if(typeArg == fca_IN || typeArg == fca_OUT){
					bool in = (typeArg == fca_IN);
					fca_port_flag flag = va_arg(vl, int);

					Port *port;
					if (in)
					{
						port = new InputPort(name, NULL,
					                     (flag & fca_NON_BLOCKING) != 0);
					}
					else
					{
						port = new OutputPort(name);
					}
					ports.push_back(port);
				}
				else {
					flowvr::TypedTrace<int>* pTraceInt;
					flowvr::TypedTrace<float>* pTraceFloat;
					flowvr::TypedTrace<std::string>* pTraceString;
					switch(typeArg){
						case fca_trace_INT:
							pTraceInt = new flowvr::TypedTrace<int>(name);
							traces.push_back(pTraceInt);
							break;
						case fca_trace_FLOAT:
							pTraceFloat = new flowvr::TypedTrace<float>(name);
							traces.push_back(pTraceFloat);
							break;
						case fca_trace_STRING:
                                                        pTraceString = new flowvr::TypedTrace<std::string>(name);
                                                        traces.push_back(pTraceString);
                                                        break;
					}
				}

				name = va_arg(vl, const char *);
			}
			va_end(vl);
		}

		flowvr::ModuleAPI *api = flowvr::initModule(ports);
		return (api == NULL ? NULL : box(new Module(api, ports,traces)));
	}

	fca_module
	fca_new_empty_module()
	{
		std::vector<Port*> ports;
		std::vector<Trace*> traces;
		return box(new Module(NULL,ports,traces));
	}

    void
    fca_set_modulename(fca_module module, const char *modulename)
    {
        unbox(module)->setModulename(modulename);
    }

	fca_module
	fca_new_module_from_ports(fca_port p, ...)
	{
		vector<Port *> ports;
		{
			std::va_list vl;
			va_start(vl, p);
			fca_port curr = p;
			while (curr != NULL)
			{
				ports.push_back(unbox(curr));

				curr = va_arg(vl, fca_port);
			}
			va_end(vl);
		}
		vector<Trace *> traces;
		flowvr::ModuleAPI *api = flowvr::initModule(ports);
		return (api == NULL ? NULL : box(new Module(api, ports,traces)));
	}



	fca_message
	fca_get(fca_port port)
	{
		InputPort *in = dynamic_cast<InputPort *>(unbox(port));
		if (in == NULL)
		{
			return box<Message>(NULL);
		}

		assert(in->getModule());

		MessageRead *msg = new MessageRead();
		in->getModule()->get(in, *msg);

		return box(msg);
	}

	bool
	fca_put(fca_port port, fca_message msg)
	{
		OutputPort *out = dynamic_cast<OutputPort *>(unbox(port));
		MessageWrite *msg_w;

		// The dynamic cast  is inside the if to use  the short circuit property
		// of “||”: if  the first condition is verified,  the dynamic cast won't
		// be executed.
		if ((out == NULL)
		    || ((msg_w = dynamic_cast<MessageWrite *>(unbox(msg))) == NULL))
		{
			return false;
		}

		assert(out->getModule());

		out->getModule()->put(out, *msg_w);
		return true;
	}

	bool
	fca_wait(fca_module mod)
	{
		assert(mod != NULL);

		return (unbox(mod)->wait() != 0);
	}

	//--------------------------------------
	// Port functions.

	fca_port
	fca_new_port(const char *p, ...)
	{
		assert(p != NULL);

		std::va_list vl;
		va_start(vl, p);
		const char *name = p;

		bool in = (va_arg(vl, int) == fca_IN);
		fca_port_flag flag = va_arg(vl, int);

		Port *port;
		if (in)
		{
            port = new InputPort(name);
            if((flag & fca_NON_BLOCKING) != 0){
                InputPort* input = dynamic_cast<InputPort*>(port);
                input->setNonBlockingFlag(true);
            }
		}
		else
		{
			port = new OutputPort(name);
		}

		va_end(vl);


		return box(port);
	}

	fca_port
	fca_get_port(fca_module mod, const char *name)
	{
		assert(mod != NULL);
		assert(name != NULL);

		return box(unbox(mod)->getPortByName(name));
	}

	fca_port_type
	fca_get_port_type(fca_port port)
	{
		assert(port != NULL);

		if (unbox(port)->isInput())
		{
			return fca_IN;
		}

		return fca_OUT;
	}

	//--------------------------------------
	// Message functions.

	fca_message
	fca_new_message(void *allocator, size_t size)
	{
		assert(allocator != NULL);

		BufferWrite buf = unbox<Allocator>(allocator)->alloc(size);
		if (!buf.valid())
		{
			return NULL;
		}

		MessageWrite *msg = new MessageWrite();
		msg->data = buf;

		return box(msg);
	}

	size_t
	fca_number_of_segments(const fca_message msg)
	{
		assert(msg != NULL);

		return unbox(msg)->get_buffer().getNumberOfSegments();
	}

	bool
	fca_add_segment(void *allocator, fca_message msg, size_t size)
	{
		assert(allocator != NULL);
		assert(msg != NULL);

		BufferWrite buf = unbox<Allocator>(allocator)->alloc(size);
		if (!buf.valid())
		{
			return false;
		}

		unbox(msg)->get_buffer() += buf;

		return true;
	}

	bool
	fca_remove_segment(fca_message msg, size_t segment)
	{
		assert(msg != NULL);

		Buffer &buf = unbox(msg)->get_buffer();

		assert(segment < buf.getNumberOfSegments());

		buf -= buf[segment];

		return true;
	}

	bool
	fca_resize_segment(fca_message msg, size_t segment, size_t size)
	{
		MessageWrite *msg_w = dynamic_cast<MessageWrite *>(unbox(msg));
		if (msg_w == NULL)
		{
			return false;
		}

		return msg_w->data.resize(size, false, segment);
	}

	size_t
	fca_get_segment_size(const fca_message msg, size_t segment)
	{
		assert(msg != NULL);

		return unbox(msg)->get_buffer().getSize(segment);
	}

	const void *
	fca_get_read_access(const fca_message msg, size_t segment)
	{
		assert(msg != NULL);

		return unbox(msg)->get_buffer().readAccess(segment);
	}

	void *
	fca_get_write_access(fca_message msg, size_t segment)
	{
		MessageWrite *msg_w = dynamic_cast<MessageWrite *>(unbox(msg));
		if (msg_w == NULL)
		{
			return NULL;
		}

		return msg_w->data.writeAccess(segment);
	}

	void
	fca_append_port(fca_module mod, fca_port port){
		assert(mod != NULL);
		assert(port != NULL);
		unbox(mod)->appendPort(unbox(port));
	}




	//-------------------------------------
	// Trace functions


	fca_trace
        fca_new_trace(const char *p, ...)
        {

		std::va_list vl;
                va_start(vl, p);
                const char *name = p;

                int typeArg = va_arg(vl, int);

                flowvr::TypedTrace<int>* pTraceInt;
                flowvr::TypedTrace<float>* pTraceFloat;
                flowvr::TypedTrace<std::string>* pTraceString;
                switch(typeArg){
                        case fca_trace_INT:
                                pTraceInt = new flowvr::TypedTrace<int>(name);
				return box(pTraceInt);
                                break;
                        case fca_trace_FLOAT:
                                pTraceFloat = new flowvr::TypedTrace<float>(name);
				return box(pTraceFloat);
                                break;
                        case fca_trace_STRING:
                                pTraceString = new flowvr::TypedTrace<std::string>(name);
				return box(pTraceString);
                                break;
                }


                va_end(vl);


                return NULL;
        }

	void
	fca_append_trace(fca_module mod, fca_trace trace){
		assert(mod != NULL);
		assert(trace != NULL);
		unbox(mod)->appendTrace(unbox(trace));
		return;
	}

	fca_trace
        fca_get_trace(fca_module mod, const char *name)
        {
                assert(mod != NULL);
                assert(name != NULL);

                return box(unbox(mod)->getTraceByName(name));
        }


	bool
        fca_write_trace(fca_trace trace, void *data)
	{
		assert(trace != NULL);
		assert(data != NULL);

		Trace* _trace = unbox(trace);
		TypedTrace<std::string> *_strTrace = dynamic_cast<TypedTrace<std::string> *>(_trace);
		if(_strTrace){
			std::string *str = reinterpret_cast<std::string*>(data);
			_strTrace->write(*str);
			return true;
		}

		TypedTrace<int> *_intTrace = dynamic_cast<TypedTrace<int> *>(_trace);
		if(_intTrace){
			int *i = reinterpret_cast<int*>(data);
                        _intTrace->write(*i);
			return true;
		}

		TypedTrace<float> *_floatTrace = dynamic_cast<TypedTrace<float> *>(_trace);
		if(_floatTrace){
			float *f = reinterpret_cast<float*>(data);
                        _floatTrace->write(*f);
			return true;
		}
		return false;
	}


	//--------------------------------------
	// Stamp functions.

	fca_stamp
	fca_register_stamp(fca_port port, const char *name,
	                   fca_stamp_type type, ...)
	{
		assert(port != NULL);
		assert(name != NULL);

		BaseType *flowvr_type;
		{
			std::va_list vl;
			va_start(vl, type);
			flowvr_type = get_flowvr_type(type, vl);
			va_end(vl);
		}
		assert(flowvr_type != NULL);

		StampInfo* stamp = (*(unbox(port)->stamps))[name];

		assert(stamp == NULL);

		//stamp = new StampInfo(name, flowvr_type, false);
		stamp = new StampInfo(name, flowvr_type, true);

		assert(unbox(port)->stamps != NULL);

		if (!unbox(port)->stamps->add(stamp))
		{
			delete stamp;
			stamp = NULL;
		}

		return box(stamp);
	}

	fca_stamp
	fca_get_stamp(fca_port port, const char *name)
	{
		return box((*(unbox(port)->stamps))[name]);
	}

	bool
	fca_write_stamp(fca_message message, const fca_stamp stamp,
	                void *value)
	{

		/* Multiple cases:
		 * Message
		 * MessageWrite
		 * MessagePut
		*/

		const Message *msg = unbox(message);
		const StampInfo *stamp_info = unbox(stamp);
		const BaseType *type = stamp_info->getType();
		fca_stamp_type type_fca = get_fca_type(type);
        const flowvr::TypeArray *type_array = dynamic_cast<const flowvr::TypeArray*>(type);

		assert(msg != NULL);
		assert(stamp_info != NULL);

		if (const MessageRead *msg_r = dynamic_cast<const MessageRead *>(msg)) {
			// Writing stamps is not allowed on MessageRead
			not_allowed("Error: writing stamps on read-only message");
			return false;
		} else {
			// MessageWrite or MessagePut
			flowvr::StampsWrite* stamp_write;

			if (const MessageWrite *msg_w = dynamic_cast<const MessageWrite *>(msg)) {
				stamp_write = (flowvr::StampsWrite*)&(msg_w->stamps);
			} else {
				assert(dynamic_cast<const MessagePut *>(msg) != NULL);
				const MessagePut *msg_p = reinterpret_cast<const MessagePut *>(msg);

				stamp_write = (flowvr::StampsWrite*)&(msg_p->stamps);
			}

			int bufferSize;
			char* stampByteBuffer;
			std::string stampString = "";

			switch (type_fca) {
				case fca_ARRAY:
                    if(type_array){
                        fca_stamp_type type_fca_array = get_fca_type(type_array->t);
                        switch (type_fca_array) {
                            case fca_FLOAT:
                                for(int i = 0; i < type_array->n; i++){
                                    float data = ((static_cast<float*>(value))[i]);
                                    stamp_write->write((*stamp_info)[i],data);
                                }
                                break;
                            case fca_INT:
                                for(int i = 0; i < type_array->n; i++){
                                    int data = ((static_cast<int*>(value))[i]);
                                    stamp_write->write((*stamp_info)[i],data);
                                }
                                break;
                            default:
                                not_implemented();
                                break;
                            }

                    }
                    else {
                        printf("Error : type fca is fca_ARRAY but couldn't cast in TypeArray\n");
                        return false;
                    }
					return true;
					//not_implemented();
					break;

				case fca_BINARY:
					bufferSize = stamp_write->getSize();
					stampByteBuffer = (char*)stamp_write->writeAccess();
					memcpy(stampByteBuffer, value, bufferSize);
					return true;
					break;

				case fca_FLOAT:
					stamp_write->write(*stamp_info,*(static_cast<float*>(value)));
					return true;
					break;

				case fca_INT:
					stamp_write->write(*stamp_info,*(static_cast<int*>(value)));
					return true;
					break;

				case fca_STRING:
					bufferSize = (reinterpret_cast<const flowvr::TypeString*>(type))->size();
					stampString.append((char*)value);

					stamp_write->write(*stamp_info,stampString);
					return true;
					break;

				default:
					break;
			}

		}

		return false;
	}

	void *
	fca_read_stamp(const fca_message boxed_msg, const fca_stamp boxed_stamp)
	{
		// Unboxes the various objects.
		const Message *msg = unbox(boxed_msg);
		const StampInfo *stamp = unbox(boxed_stamp);

		assert(msg != NULL);
		assert(stamp != NULL);

		// Allocates the memory needed to store the stamp's value.
		void *result = std::malloc(get_type_size(stamp->getType()));

		// We have to check which  kind of message it is because, unfortunately,
		// the  types of  “MessageWrite::stamps”  and “MessageRead::stamps”  are
		// different.
		// There is also type MessagePut::stamps
		const MessageWrite *msg_w = dynamic_cast<const MessageWrite *>(msg);
		const MessagePut *msg_p = dynamic_cast<const MessagePut *>(msg);
		const MessageRead *msg_r = dynamic_cast<const MessageRead *>(msg);

		if (msg_w)
		{
			read_stamp(msg_w->stamps, stamp, result);
		} else if (msg_p)
		{
			read_stamp(msg_p->stamps, stamp, result);
		}
		else
		{
			assert(dynamic_cast<const MessageRead *>(msg) != NULL);
			read_stamp(msg_r->stamps, stamp, result);
		}

		return result;
	}

	const char *
	fca_get_stamp_name(const fca_stamp stamp)
	{
		assert(stamp != NULL);

		return unbox(stamp)->getName().c_str();
	}

	fca_stamp_type
	fca_get_stamp_type(const fca_stamp stamp)
	{
		assert(stamp != NULL);

		return get_fca_type(unbox(stamp)->getType());

	}

	//--------------------------------------
	// Pool functions.

	fca_pool
	fca_new_pool(fca_module mod, size_t capacity)
	{
		assert(mod != NULL);

		return box<Pool>(new Pool(unbox(mod)->getAllocator(), capacity));
	}

	size_t
	fca_get_pool_capacity(const fca_pool pool)
	{
		assert(pool != NULL);

		return unbox(pool)->capacity();
	}

	void
	fca_set_pool_capacity(fca_pool pool, size_t capacity)
	{
		assert(pool != NULL);

		unbox(pool)->capacity(capacity);
	}
}
