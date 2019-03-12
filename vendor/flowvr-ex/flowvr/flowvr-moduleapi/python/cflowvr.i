// -*- c++ -*- 


%module cflowvr


%{ 

#include <flowvr/module.h>


// flatten out the namespace
using namespace flowvr;

%} 

// do not seem to be defined...
%ignore flowvr::ModuleAPI::writePID;
%ignore flowvr::Buffer::getSegments;



// all pure FlowVR functions release the GIL...
%exception  {
  Py_BEGIN_ALLOW_THREADS
  $action
  Py_END_ALLOW_THREADS
};



// map vector and string to something usable by Python
%include <std_vector.i>
%include <std_string.i>


// order matters because there is no recursive inclusion

%include <flowvr/common.h>
%include <flowvr/parallel.h>

%include <flowvr/buffer.h>
%include <flowvr/bufferpool.h>

%include <flowvr/stamp.h>
%include <flowvr/message.h>

%include <flowvr/moduleapi.h>
%include <flowvr/moduleapifactory.h>


// used in the initModule call 
%template(vectorPort) std::vector<flowvr::Port*>;

// ... extension function do not release the GIL (they contain Python API calls)
%exception {
  $action
}


// in Python, string is a crossroads between binary types (thanks to
// the array and struct modules)

%extend flowvr::Buffer {
  PyObject * asString() {
    PyObject *ret = PyString_FromStringAndSize(NULL, $self->getSize());
    memcpy(PyString_AsString(ret), $self->readAccess(), $self->getSize());
    return ret;
  };
}; 

%extend flowvr::Stamps {
  PyObject * asString() {
    PyObject *ret = PyString_FromStringAndSize(NULL, $self->getSize());
    memcpy(PyString_AsString(ret), $self->readAccess(), $self->getSize());
    return ret;
  };
}; 


// make it raise an exception if allocation failure
// exception is not very readable, though...
%contract flowvr::ModuleAPI::allocString(PyObject *o) {
require:
  PyString_Check(o);
ensure:
  allocString.valid();
}




%extend flowvr::ModuleAPI {
  
  // constructor from string. Same as the C++ version but avoids an extra copy.
  flowvr::BufferWrite allocString(PyObject *o) {
    size_t size = PyString_Size(o); 
    flowvr::BufferWrite ret = $self->alloc(size); 
    if(ret.valid())
      memcpy(ret.writeAccess(), PyString_AsString(o), size);  
    return ret;
  }

}; 

%extend flowvr::StampList {
  flowvr::StampInfo * __getitem__(int i) { 
    return (*$self)[i]; 
  }
  flowvr::StampInfo * __getitem__(const char *name) { 
    return (*$self)[name]; 
  }
}; 

// the Accessor class is not visible in SWIG, so add a few intermediate functions

%extend flowvr::StampsWrite {
  void write1(const flowvr::StampInfo &si, int v) { $self->write(si, v); } 
  void write1(const flowvr::StampInfo &si, float v) { $self->write(si, v); } 
  void write1(const flowvr::StampInfo &si, const std::string &v) { $self->write(si, v); } 

  // same, for arrays
  void writeArray(const flowvr::StampInfo &si, int i, int v) { $self->write(si[i], v); } 
  void writeArray(const flowvr::StampInfo &si, int i, float v) { $self->write(si[i], v); } 
  void writeArray(const flowvr::StampInfo &si, int i, const std::string &v) { $self->write(si[i], v); } 

}; 

// The stamps type encoding is slightly over-engineered (and leaks
// memory). We need a few methods to make it usable. No attempt to fix
// the mem leaks. 
%extend flowvr::BaseType {
  
  // need some runtime-typing to return a result. The XML description is too messy
  const char *typeName() {
    if(dynamic_cast<flowvr::TypeArray*>($self)) return "array"; 
    if(dynamic_cast<flowvr::TypeBinary*>($self)) return "binary"; 
    if(dynamic_cast<flowvr::TypeInt*>($self)) return "int"; 
    if(dynamic_cast<flowvr::TypeFloat*>($self)) return "float"; 
    if(dynamic_cast<flowvr::TypeString*>($self)) return "string"; 
  }

  flowvr::TypeArray* toTypeArray() {
    return dynamic_cast<flowvr::TypeArray*>($self); 
  }
  flowvr::TypeBinary* toTypeBinary() {
    return dynamic_cast<flowvr::TypeBinary*>($self); 
  }
  
}; 





