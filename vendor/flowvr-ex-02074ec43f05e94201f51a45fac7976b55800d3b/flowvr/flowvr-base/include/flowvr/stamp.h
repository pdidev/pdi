/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                         Base Libraries                          *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490). ALL RIGHTS RESERVED.                                *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Ronan Gaugne,                                                *
*    Valerie Gouranton,                                           *
*    Loick Lecointre,                                             *
*    Sebastien Limet,                                             *
*    Bruno Raffin,                                                *
*    Sophie Robert,                                               *
*    Emmanuel Melin.                                              *
*                                                                 *
*******************************************************************
*                                                                 *
* File: include/flowvr/stamp.h                                    *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_STAMP_H
#define FLOWVR_STAMP_H

#include "flowvr/buffer.h"
#include "flowvr/bufferpool.h"
#include "flowvr/xml.h"

#include <vector>

namespace flowvr
{

class StampList;
class Stamps;
class StampsWrite;

/** @file
 FLOWVR Stamps.

  Stamps are used to associate properties to messages (source, iteration, ...).
  Each stamp has a name and a type, which can be:
  - int
  - float
  - std::string
  - array : n values of a type t

  Two stamps are predefined by FLOWVR:
  - it (int) : iteration number
  - source (string) : source of the message
  - num  (int) : message number

  The user can define other stamps by specifying a name and a type and adding it
  to the list of stamps of a port.

*/

/// Type definition abstract base class.
class BaseType
{
 public:

  /// @name Specification
  /// @{

 protected:
  BaseType() {} // do not directly create a type
  virtual ~BaseType() {} // do not directly delete a type
 public:
  /// Generate the xml description.
  virtual xml::DOMNode* xmlDesc() const = 0;

  /// Create a type definition given a xml description.
  static BaseType* create(xml::DOMNode* spec);

  /// Clone this description
  virtual BaseType* clone() const = 0;

  /// Remove this description
  virtual void remove() = 0;

  /// Return the size in bytes of this type.
  virtual size_t size() const = 0;
  /// @}

  /// Array access.
  /// @param index index of the element in the array.
  /// @param offset of the array in the stamps data, modified to the offset of the indexed element.
  /// @return definition of the element's type, or NULL on errors (not an array or bad index).
  virtual const BaseType* array(int index, int& offset) const = 0;

  /// @name Read access
  /// @{
  virtual bool read(int& val, const Buffer& data, int offset) const = 0;
  virtual bool read(float& val, const Buffer& data, int offset) const = 0;
  virtual bool read(std::string &val, const Buffer& data, int offset) const = 0;
  /// @}

  /// @name Write access
  /// @{
  virtual bool write(int val, BufferWrite& data, int offset, StampList* list=NULL) const = 0;
  virtual bool write(float val, BufferWrite& data, int offset, StampList* list=NULL) const = 0;
  virtual bool write(const std::string& val, BufferWrite& data, int offset, StampList* list=NULL) const = 0;
  /// @}
};

/// Integer stamp type.
class TypeInt : public BaseType
{
 public:
  static TypeInt* create();
  BaseType* clone() const;
  void remove();
  static const char* xmlName; ///< XML tag name.
  xml::DOMNode* xmlDesc() const;
  size_t size() const { return sizeof(int); }
  bool read(int& val, const Buffer& data, int offset) const
  {
    val=*data.getRead<int>(offset);
    return true;
  }
  bool read(float& val, const Buffer& data, int offset) const
  {
    val=(float)*data.getRead<int>(offset);
    return true;
  }
  bool read(std::string &val, const Buffer& data, int offset) const;
  const BaseType* array(int /*index*/, int& /*offset*/) const { return NULL; }
  bool write(int val, BufferWrite& data, int offset, StampList* /*list*/=NULL) const
  {
    *data.getWrite<int>(offset) = val;
    return true;
  }
  bool write(float val, BufferWrite& data, int offset, StampList* /*list*/=NULL) const
  {
    *data.getWrite<int>(offset) = (int)val;
    return true;
  }
  bool write(const std::string& val, BufferWrite& data, int offset, StampList* list=NULL) const;
};

/// Float stamp type.
class TypeFloat : public BaseType
{
 public:
  static TypeFloat* create();
  BaseType* clone() const;
  void remove();
  static const char* xmlName; ///< XML tag name.
  xml::DOMNode* xmlDesc() const;
  size_t size() const { return sizeof(float); }
  bool read(int& val, const Buffer& data, int offset) const
  {
    val=(int)*data.getRead<float>(offset);
    return true;
  }
  bool read(float& val, const Buffer& data, int offset) const
  {
    val=*data.getRead<float>(offset);
    return true;
  }
  bool read(std::string &val, const Buffer& data, int offset) const;
  const BaseType* array(int /*index*/, int& /*offset*/) const { return NULL; }
  bool write(int val, BufferWrite& data, int offset, StampList* /*list*/=NULL) const
  {
    *data.getWrite<float>(offset) = (float)val;
    return true;
  }
  bool write(float val, BufferWrite& data, int offset, StampList* /*list*/=NULL) const
  {
    *data.getWrite<float>(offset) = val;
    return true;
  }
  bool write(const std::string& val, BufferWrite& data, int offset, StampList* list=NULL) const;
};

/// std::string stamp type.
class TypeString : public BaseType
{
 public:
  static TypeString* create();
  BaseType* clone() const;
  void remove();
  static const char* xmlName; ///< XML tag name.
  xml::DOMNode* xmlDesc() const;
  size_t size() const { return 2*sizeof(int); }
  bool read(int& val, const Buffer& data, int offset) const;
  bool read(float& val, const Buffer& data, int offset) const;
  bool read(std::string& val, const Buffer& data, int offset) const;
  const BaseType* array(int /*index*/, int& /*offset*/) const { return NULL; }
  bool write(int val, BufferWrite& data, int offset, StampList* list=NULL) const;
  bool write(float val, BufferWrite& data, int offset, StampList* list=NULL) const;
  bool write(const std::string& val, BufferWrite& data, int offset, StampList* list=NULL) const;
};

/// Array stamp type.
class TypeArray : public BaseType
{
 protected:
  TypeArray(int _n, BaseType* _t) : n(_n), t(_t) {}
  TypeArray(const TypeArray& a) : BaseType(a), n(a.n), t((a.t==NULL)?NULL:a.t->clone()) {}
  //~TypeArray() { if (t!=NULL) t->remove(); }
 public:
  static TypeArray* create(int _n, BaseType* _t);
  BaseType* clone() const;
  void remove();

  static const char* xmlName; ///< XML tag name.
  xml::DOMNode* xmlDesc() const;
  size_t size() const { return n*t->size(); }
  bool read(int& val, const Buffer& data, int offset) const
  { return t->read(val,data,offset); }
  bool read(float& val, const Buffer& data, int offset) const
  { return t->read(val,data,offset); }
  bool read(std::string& val, const Buffer& data, int offset) const
  { return t->read(val,data,offset); }
  const BaseType* array(int index, int& offset) const
  {
    if ((unsigned)index>=(unsigned)n)
      return NULL;
    offset+=index*t->size();
    return t;
  }
  bool write(int val, BufferWrite& data, int offset, StampList* list=NULL) const
  { return t->write(val, data,offset,list); }
  bool write(float val, BufferWrite& data, int offset, StampList* list=NULL) const
  { return t->write(val,data,offset,list); }
  bool write(const std::string& val, BufferWrite& data, int offset, StampList* list=NULL) const
  { return t->write(val,data,offset,list); }

  const int n; ///< Number of elements.
  BaseType* t; ///< Element type definition.

};

/// Binary stamp type.
class TypeBinary : public BaseType
{
 protected:
  TypeBinary(int _size) : bsize(_size) {}
 public:
  static TypeBinary* create(int _size);
  BaseType* clone() const;
  void remove();

  static const char* xmlName; ///< XML tag name.
  xml::DOMNode* xmlDesc() const;
  size_t size() const { return bsize; }
  bool read(int& /*val*/, const Buffer& /*data*/, int /*offset*/) const
  { return false; }
  bool read(float& /*val*/, const Buffer& /*data*/, int /*offset*/) const
  { return false; }
  bool read(std::string& /*val*/, const Buffer& /*data*/, int /*offset*/) const
  { return false; }
  const BaseType* array(int /*index*/, int& /*offset*/) const
  { return NULL; }
  bool write(int /*val*/, BufferWrite& /*data*/, int /*offset*/, StampList* /*list*/=NULL) const
  { return false; }
  bool write(float /*val*/, BufferWrite& /*data*/, int /*offset*/, StampList* /*list*/=NULL) const
  { return false; }
  bool write(const std::string& /*val*/, BufferWrite& /*data*/, int /*offset*/, StampList* /*list*/=NULL) const
  { return false; }

  const int bsize;

};


/// Stamp specification.
/// @ingroup Messagehandling
class StampInfo
{
 public:
  /**
   *  Standard constructor.
   *  @param _name unique name of this stamp (unique within the StampList)
   *  @param _type definition of this stamp's type (pointer afterward owned and destroyed by this class).
   *  @param _dynamic dynamic StampInfos are user given, all system defined
   *                  stamps are marked non-dynamic.
   *
   */
  StampInfo(const std::string& _name, BaseType* _type, bool _dynamic=false)
    : name (_name), type(_type), parent(NULL), offset(-1), dynamic(_dynamic)
  {
  }

  /// Copy constructor.
  StampInfo(const StampInfo& src)
    : name(src.name), type((src.type==NULL)?NULL:src.type->clone()), parent(NULL), offset(-1), dynamic(src.dynamic)
  {
  }

  /// Destructor.
  ~StampInfo()
  {
    if (type!=NULL)
      type->remove();
  }

  /// Name accessor.
  const std::string& getName() const { return name; }

  /// Set the name of this stamp. WARNING: only use during initialization.
  void setName(const std::string& n) { name = n; }

  /// Size of this stamp (in bytes).
  int getSize() const { return type->size(); }

  /// Offset of this stamp (in bytes).
  int getOffset() const { return offset; }

  /// Type definition accessor.
  const BaseType* getType() const { return type; }

  /**
   * @return true if this StampInfo was given by the user and
   *         not on system level
   */
  bool isDynamic() const { return dynamic; }

  /// Stamp read/write internal helper class.
  class Accessor
  {
  public:

    Accessor(const BaseType* _type, int _offset, StampList* _list)
      : type(_type), offset(_offset), list(_list)
    {
    }

    Accessor(const Accessor& array, int index)
      : type(array.type), offset(array.offset), list(array.list)
    {
      if (type!=NULL)
        type=type->array(index,offset);
    }

    Accessor operator[](int index)
    {
      return Accessor(*this,index);
    }

    const BaseType* type;
    int offset;
    StampList* list;
  };

  /// Return the accessor to use this stamp.
  operator Accessor() const
  {
    return Accessor(type,offset,parent);
  }
  /// Array accessor.
  Accessor operator[](int index) const
  {
    return Accessor(*this,index);
  }

  // Test if this stamp is valid
  bool valid() const
  {
    return type!=NULL && offset >= 0;
  }

  bool valid(int index) const
  {
    int o = offset;
    return valid() && type->array(index,o) != NULL;
  }

 protected:

  std::string name; ///< Unique name
  BaseType* type; ///< Data type definition
  StampList* parent; ///< StampList where this stamp belong to.
  int offset; ///< Offset in the stamps' data.
  bool dynamic;

  friend class StampList;

};



/**
 *  Specification of a list of stamp.
 *  The StampList defines a number of system stamps with the following symbolic names
 *  - it: the iteration number of the message source (type: int)
 *  - num: the number of the message (type: int)
 *  - source: the source module of the message (type: string)
 *  - sysflags: a bitmask defining flags that can be used on system level (type: int)
 *  - sysdata: auxiliary data for actions followed by sysflags (type: array[int])
 *  - userdata: a bitmask that can be used by module code and is not interpreted by the system (type: int)
 *
 *  @ingroup Messagehandling
 */

class StampList
{
 public:

  /// @name Definition
  /// @{

  /// Constructor.
  StampList();
  /// Destructor.
  ~StampList();
  /// Append a stamp to this list.
  bool add(StampInfo* stamp);

  /// Minimum data size to store the stamps.
  int minSize() const
  {
    return baseSize;
  }
  
  StampList* clone() const;

  /// @}

  /// @name System-defined stamps
  /// @{
  StampInfo source;    ///< Message source ID
  StampInfo num;       ///< Message number
  StampInfo it;        ///< Module iteration number

  enum
  {
	  SYS_FLG_NONE         = 0,
	  SYS_FLG_SCRATCH      = 1, /**< a message containing this flag can be eliminated ('scratched') by the system */
	  SYS_FLG_KEEPSEGMENTS = 2  /**< maintain segments in a message, otherwise, feel free to merge segments when cheap */
  };

  StampInfo sysflags;  ///< Flags supported by the system
  StampInfo sysdata;   ///< auxiliary structure needed by some system flags
  StampInfo userflags; ///< user defined flags

  /// @}

  /// @name Stamps access
  /// @{

  /// Number of defined stamps.
  int nbStamp() const { return stamps.size(); }

  /// Get a stamp given its index.
  StampInfo* operator[](int i) const
  {
    return stamps[i];
  }

  /// Get a stamp given its name.
  StampInfo* operator[](const std::string& name) const;

  /// @}

  /// @name XML specification
  /// @{

  /// Generate the XML specification.
  xml::DOMNode* generateXML() const;

  /// Read a XML specification and modify the stamp list accordingly.
  void updateFromXML(xml::DOMNode* xml);

  /// @}

 protected:
  typedef std::vector<StampInfo*> List;
  List stamps; ///< List of stamps definition.
  int baseSize; ///< Fixed data size (not including the dynamic std::strings size).
  BufferPool pool; ///< Pool of previously allocated buffers

  /// Resize a buffer to a specified minimum size.
  void resize(BufferWrite& buffer, int minSize);

  friend class TypeString; // a std::string can resize the stamps buffer
  friend class StampsWrite; // w stamp writer too
};

/// List of stamps used on control messages.
/// @ingroup Messagehandling
class StampListControl : public StampList
{
 public:
  StampListControl()
    : dest("dest", TypeString::create())
    , reply("reply", TypeString::create())
  {
    add(&dest);
    add(&reply);
  }

  /// @name System-defined stamps
  /// @{
  StampInfo dest; ///< destination node
  StampInfo reply; ///< reply address
  /// @}
};

/// List of stamps used on messages specifying next messages' stamps.
/// @ingroup Messagehandling
class StampListSpecification : public StampList
{
 public:
  StampListSpecification()
    : spec("spec", TypeString::create())
  {
    add(&spec);
  }

  /// @name System-defined stamps
  /// @{
  StampInfo spec; ///< XML specification of the stamps
  /// @}
};

/// Stamps data stored in a shared read-only buffer.
/// @ingroup Messagehandling
class Stamps
 {
 public:
  Stamps()
  {
  }

  Stamps(const Buffer& buf)
    : buffer(buf)
  {
  }

  // cast to buffer
  operator Buffer() const
  {
	return buffer;
  }

  bool   valid() const
  {
	  return buffer.valid();
  }

  bool empty() const
  {
	return buffer.empty();
  }

  size_t getSize() const
  {
	  return buffer.getSize();
  }

  const ubyte *readAccess() const
  {
	  return buffer.readAccess();
  }

  void clear()
  {
	  buffer.clear();
  }

  Allocator *getAllocator() const
  {
	  return buffer.getAllocator();
  }

  template<class T>
  const T *getRead( size_t offset ) const
  {
	  return (const T*)(readAccess()+offset);
  }

  void operator=(const Buffer& buf)
  {
    buffer.operator=(buf);
  }

  bool operator==( const Stamps &other ) const
  {
	  return (buffer == other.buffer);
  }

  int getStampsSize() const;

  /// Test if a stamp can be read from this buffer.
  /// @param stamp stamp's accessor provided by the stamp's definition.
  /// @return true if the stamp can be read, false otherwise.
  bool isValid(const StampInfo::Accessor& stamp) const;

  /// Read a stamp integer value.
  /// @param stamp stamp's accessor provided by the stamp's definition.
  /// @param val value read from the buffer.
  /// @return true if the stamp was read successfully, false otherwise.
  bool read(const StampInfo::Accessor& stamp, int& val) const;

  /// Read a stamp float value.
  /// @param stamp stamp's accessor provided by the stamp's definition.
  /// @param val value read from the buffer.
  /// @return true if the stamp was read successfully, false otherwise.
  bool read(const StampInfo::Accessor& stamp, float& val) const;

  /// Read a stamp std::string value.
  /// @param stamp stamp's accessor provided by the stamp's definition.
  /// @param val value read from the buffer.
  /// @return true if the stamp was read successfully, false otherwise.
  bool read(const StampInfo::Accessor& stamp, std::string& val) const;


 private:
  Buffer buffer;
};

/// Modifiable stamps data stored in a shared writable buffer.
/// @ingroup Messagehandling
class StampsWrite : public BufferWrite
{
 public:
  StampsWrite();
  StampsWrite(const BufferWrite& buf);

  int getStampsSize() const
  {
    if (getSize()< sizeof(int))
    	return 0;

    return *getRead<int>(0);
  }

  /// Clone stamps
  void clone(const Stamps& stamps, StampList* stlist);

  /// Test if a stamp can be read from this buffer.
  /// @param stamp stamp's accessor provided by the stamp's definition.
  /// @return true if the stamp can be read or written, false otherwise.
  bool isValid(const StampInfo::Accessor& stamp) const;

  /// Read a stamp integer value.
  /// @param stamp stamp's accessor provided by the stamp's definition.
  /// @param val value read from the buffer.
  /// @return true if the stamp was read successfully, false otherwise.
  bool read(const StampInfo::Accessor& stamp, int& val) const;

  /// Read a stamp float value.
  /// @param stamp stamp's accessor provided by the stamp's definition.
  /// @param val value read from the buffer.
  /// @return true if the stamp was read successfully, false otherwise.
  bool read(const StampInfo::Accessor& stamp, float& val) const;

  /// Read a stamp std::string value.
  /// @param stamp stamp's accessor provided by the stamp's definition.
  /// @param val value read from the buffer.
  /// @return true if the stamp was read successfully, false otherwise.
  bool read(const StampInfo::Accessor& stamp, std::string& val) const;

  /// Write a stamp integer value.
  /// @param stamp stamp's accessor provided by the stamp's definition.
  /// @param val value to write to the buffer.
  /// @return true if the stamp was writen successfully, false otherwise.
  bool write(const StampInfo::Accessor& stamp, int val);

  /// Write a stamp float value.
  /// @param stamp stamp's accessor provided by the stamp's definition.
  /// @param val value to write to the buffer.
  /// @return true if the stamp was writen successfully, false otherwise.
  bool write(const StampInfo::Accessor& stamp, float val);

  /// Write a stamp std::string value.
  /// @param stamp stamp's accessor provided by the stamp's definition.
  /// @param val value to write to the buffer.
  /// @return true if the stamp was writen successfully, false otherwise.
  bool write(const StampInfo::Accessor& stamp, const std::string& val);

 protected:
  /// Prepare the writing of a stamp to this buffer.
  bool startWrite(const StampInfo::Accessor& stamp);

};

} // namespace flowvr

#endif
