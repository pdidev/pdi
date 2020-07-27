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
 * File: src/stamp.cpp                                             *
 *                                                                 *
 * Contacts:                                                       *
 *  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/
#include "flowvr/stamp.h"
#include <iostream>

namespace flowvr
{

using namespace xml;

const char* TypeInt::xmlName = "int";
xml::DOMNode* TypeInt::xmlDesc() const
{
	return new xml::DOMElement(xmlName);
}

const char* TypeFloat::xmlName = "float";
xml::DOMNode* TypeFloat::xmlDesc() const
{
	return new xml::DOMElement(xmlName);
}

const char* TypeString::xmlName = "string";
xml::DOMNode* TypeString::xmlDesc() const
{
	return new xml::DOMElement(xmlName);
}

const char* TypeArray::xmlName = "array";
xml::DOMNode* TypeArray::xmlDesc() const
{
	xml::DOMElement* elem = new xml::DOMElement(xmlName);
	elem->SetAttribute("size", n);
	elem->LinkEndChild(t->xmlDesc());
	return elem;
}

const char* TypeBinary::xmlName = "binary";
xml::DOMNode* TypeBinary::xmlDesc() const
{
	xml::DOMElement* elem = new xml::DOMElement(xmlName);
	elem->SetAttribute("size", bsize);
	return elem;
}

/// Create a type definition given a xml description.
BaseType* BaseType::create(xml::DOMNode* spec)
{
	if (spec == NULL)
		return NULL;
	if (spec->getNodeType() != DOMNode::ELEMENT_NODE)
		return NULL;
	DOMElement* e = (DOMElement*) spec;
	const char* tname = e->getNodeName();
	if (!strcmp(tname, TypeInt::xmlName))
		return TypeInt::create();
	else if (!strcmp(tname, TypeFloat::xmlName))
		return TypeFloat::create();
	else if (!strcmp(tname, TypeString::xmlName))
		return TypeString::create();
	else if (!strcmp(tname, TypeArray::xmlName))
	{
		DOMAttr* asize = e->getAttributeNode("size");
		if (asize == NULL)
			return NULL;
		DOMNodeList* children = e->getElements();
		if (children->getLength() != 1)
		{
			delete children;
			return NULL;
		}
		BaseType* elemtype = create(children->item(0));
		delete children;
		if (elemtype == NULL)
			return NULL;
		return TypeArray::create(asize->IntValue(), elemtype);
	}
	else if (!strcmp(tname, TypeBinary::xmlName))
	{
		DOMAttr* asize = e->getAttributeNode("size");
		if (asize == NULL)
			return NULL;
		return TypeBinary::create(asize->IntValue());
	}
	return NULL;
}

TypeInt* TypeInt::create()
{
	static TypeInt theType;
	return &theType;
}

BaseType* TypeInt::clone() const
{
	return create();
}

void TypeInt::remove()
{ // do nothing
}

bool TypeInt::read(std::string &val, const Buffer& data, int offset) const
{
	char buf[16];
	buf[sizeof(buf) - 1] = '\0';
	snprintf(buf, sizeof(buf) - 1, "%d", *data.getRead<int> (offset));
	val = buf;
	return true;
}

bool TypeInt::write(const std::string& val, BufferWrite& data, int offset,
		StampList* list) const
{
	*data.getWrite<int> (offset) = atoi(val.c_str());
	return true;
}

TypeFloat* TypeFloat::create()
{
	static TypeFloat theType;
	return &theType;
}

BaseType* TypeFloat::clone() const
{
	return create();
}

void TypeFloat::remove()
{ // do nothing
}

bool TypeFloat::read(std::string &val, const Buffer& data, int offset) const
{
	char buf[16];
	buf[sizeof(buf) - 1] = '\0';
	snprintf(buf, sizeof(buf) - 1, "%g", *data.getRead<float> (offset));
	val = buf;
	return true;
}

bool TypeFloat::write(const std::string& val, BufferWrite& data, int offset,
		StampList* list) const
{
	*data.getWrite<float> (offset) = atof(val.c_str());
	return true;
}

TypeString* TypeString::create()
{
	static TypeString theType;
	return &theType;
}

BaseType* TypeString::clone() const
{
	return create();
}

void TypeString::remove()
{ // do nothing
}

bool TypeString::read(int& val, const Buffer& data, int offset) const
{
	std::string str;
	if (!read(str, data, offset))
		return false;
	val = atoi(str.c_str());
	return true;
}

bool TypeString::read(float& val, const Buffer& data, int offset) const
{
	std::string str;
	if (!read(str, data, offset))
		return false;
	val = atof(str.c_str());
	return true;
}

bool TypeString::read(std::string& val, const Buffer& data, int offset) const
{
	int pos = data.getRead<int> (offset)[0];
	int size = data.getRead<int> (offset)[1];
	if (size == 0)
		val.clear();
	else
	{
		if (size < 0 || (unsigned) (pos + size) > (unsigned) data.getSize())
			return false;
		val.assign(data.getRead<char> (pos), data.getRead<char> (pos) + size);
	}
	return true;
}

bool TypeString::write(int val, BufferWrite& data, int offset, StampList* list) const
{
	char buf[16];
	buf[sizeof(buf) - 1] = '\0';
	snprintf(buf, sizeof(buf) - 1, "%d", val);
	std::string str(buf);
	return write(str, data, offset, list);
}

bool TypeString::write(float val, BufferWrite& data, int offset,
		StampList* list) const
{
	char buf[16];
	buf[sizeof(buf) - 1] = '\0';
	snprintf(buf, sizeof(buf) - 1, "%g", val);
	std::string str(buf);
	return write(str, data, offset, list);
}

bool TypeString::write(const std::string& val, BufferWrite& data, int offset,
		StampList* list) const
{
	int size = val.size();
	int pos;
	if (size > 0)
	{
		if (data.getRead<int> (offset)[1] >= size)
			pos = data.getRead<int> (offset)[0]; // reuse last std::string position
		else
		{
			pos = *data.getRead<int> (0);
			if (list == NULL)
				return false;
			list->resize(data, pos + size); // data buffer must be at least pos+size long
			*data.getWrite<int> (0) = pos + size;
		}
		memcpy(data.getWrite<char> (pos), val.c_str(), size);
	}
	else
		pos = 0;
	data.getWrite<int> (offset)[0] = pos;
	data.getWrite<int> (offset)[1] = size;
	return true;
}

TypeArray* TypeArray::create(int _n, BaseType* _t)
{
	return new TypeArray(_n, _t);
}

BaseType* TypeArray::clone() const
{
	return new TypeArray(*this);
}

void TypeArray::remove()
{
	if (t != NULL)
		t->remove();
	delete this;
}

TypeBinary* TypeBinary::create(int _size)
{
	return new TypeBinary(_size);
}

BaseType* TypeBinary::clone() const
{
	return new TypeBinary(*this);
}

void TypeBinary::remove()
{
	delete this;
}

StampList::StampList()
: source( "source", TypeString::create() )
, num( "num", TypeInt::create() )
, it( "it", TypeInt::create() )
, sysflags( "sysflags", TypeInt::create() )
, sysdata("sysdata", TypeArray::create(0, TypeInt::create() ) )
, userflags( "userflags", TypeInt::create() )
, baseSize(sizeof(int))
{
	add(&source);
	add(&num);
	add(&it);
	add(&sysflags);
	add(&sysdata);
	add(&userflags);
}

StampList::~StampList()
{
	List::iterator it;
	for (it = stamps.begin(); it != stamps.end(); ++it)
	{
		StampInfo* stamp = *it;
		if (stamp->isDynamic())
			delete stamp;
		else
			stamp->offset = -1;
	}
}

bool StampList::add(StampInfo* stamp)
{
	if ((*this)[stamp->getName()] != NULL)
	{
		std::cerr << "flowvr::StampList: ERROR: stamp " << stamp->getName()
				<< " already defined." << std::endl;
		return false;
	}
	if (stamp->parent == this)
	{
		std::cerr << "flowvr::StampList: ERROR: stamp " << stamp->getName()
				<< " already assigned to this StampList." << std::endl;
		return false;
	}
	if (stamp->parent != NULL)
	{
		std::cerr << "flowvr::StampList: ERROR: stamp " << stamp->getName()
				<< " already assigned to a StampList." << std::endl;
		return false;
	}
	stamps.push_back(stamp);
	stamp->parent = this;
	stamp->offset = baseSize;
	baseSize += stamp->getSize();
	return true;
}

StampList* StampList::clone() const
{
    StampList * const newlist = new StampList;
    const int stamps_size = this->stamps.size();
    for ( int i = 0  ;  i < stamps_size  ;  i++ ) {
        newlist->add( new StampInfo( *this->stamps[i] ) );
    }
    return newlist;
}

xml::DOMNode* StampList::generateXML() const
{
	xml::DOMElement* root = new xml::DOMElement("stamplist");
	List::const_iterator it;
	for (it = stamps.begin(); it != stamps.end(); ++it)
	{
		StampInfo* stamp = *it;
		xml::DOMElement* node = new xml::DOMElement("stamp");
		node->SetAttribute("name", stamp->getName());
		node->SetAttribute("size", stamp->getSize());
		const BaseType* stamptype = stamp->getType();
		if (stamptype != NULL)
			node->LinkEndChild(stamptype->xmlDesc());
		root->LinkEndChild(node);
	}
	return root;
}

void StampList::updateFromXML(xml::DOMNode* xml)
{
	if (xml == NULL)
	{
		std::cerr << "NULL xml in StampList::updateFromXML" << std::endl;
		return;
	}
	if (xml->getNodeType() != DOMNode::ELEMENT_NODE)
	{
		std::cerr << "INVALID xml node type " << xml->getNodeType()
				<< " in StampList::updateFromXML" << std::endl;
		return;
	}
	xml::DOMNodeList* stampxml =
			((xml::DOMElement*) xml)->getElementsByTagName("stamp");
	List newstamps;
	baseSize = sizeof(int);
	// set all stamps to offset -1
	List::iterator it;
	for (it = stamps.begin(); it != stamps.end(); ++it)
	{
		StampInfo* info = *it;
		info->offset = -1;
	}
	if (stampxml)
		for (int i = 0; i < stampxml->getLength(); i++)
		{
			xml::DOMElement* e = (xml::DOMElement*) stampxml->item(i);
			std::string name = e->Attribute("name");
			int tsize = atoi(e->Attribute("size"));
			BaseType* t = BaseType::create(e->FirstChild());
			if (t == NULL)
			{
				std::cerr << "Bad stamp type specification received for stamp "
						<< name << std::endl;
				baseSize += tsize;
			}
			else
			{
				List::iterator it;
				for (it = stamps.begin(); it != stamps.end(); ++it)
				{
					if ((*it)->getName() == name)
						break;
				}
				StampInfo* info = NULL;
				if (it != stamps.end())
				{
					info = *it;
#ifdef DEBUG
					std::cerr<<"Found stamp "<<info->getName()<<" in inputs at offset "<<baseSize<<std::endl;
#endif
					if (info->type != NULL)
						info->type->remove();
					info->type = t;
				}
				else
				{
#ifdef DEBUG
					std::cerr<<"New stamp "<<name<<" in inputs at offset "<<baseSize<<std::endl;
#endif
					info = new StampInfo(name, t, true);
				}
				info->parent = this;
				info->offset = baseSize;
				newstamps.push_back(info);
				baseSize += tsize;
				if (tsize != info->getSize())
					std::cerr << "Bad stamp size specification for " << name
							<< ": " << tsize << "!=" << info->getSize()
							<< std::endl;
			}
		}
	// then add back any remaining stamps with offset -1
	for (it = stamps.begin(); it != stamps.end(); ++it)
	{
		StampInfo* info = *it;
		if (info->offset == -1)
		{
			std::cerr << "Stamp " << info->getName() << " not found in inputs."
					<< std::endl;
			newstamps.push_back(info);
		}
	}
	// and finally set the new list of stamps
	stamps = newstamps;
	delete stampxml;
}

StampInfo* StampList::operator[](const std::string& name) const
{
	List::const_iterator it;
	for (it = stamps.begin(); it != stamps.end(); ++it)
	{
		StampInfo* stamp = *it;
		if (stamp->getName() == name)
			return stamp;
	}
	return NULL;
}

int Stamps::getStampsSize() const
{
	int bufsize = getSize();
	if (bufsize < (int) sizeof(int))
		return 0;
	int stsize = *getRead<int> (0);
	return std::min(bufsize, stsize);
}

/// Test if a stamp can be read from this buffer.
bool Stamps::isValid(const StampInfo::Accessor& stamp) const
{
	if(stamp.type == NULL)
	{
#if defined(DEBUG)
		/// @todo remove output once it is clear that this can not happen / or when known when it will happen
		// definite error
		std::cerr << "Stamp type not given?" << std::endl;
#endif
		return false;
	}

	if( stamp.offset >= 0
	 && stamp.offset + stamp.type->size() <= getStampsSize() )
		return true; // ok, everything seems to be allright with this stamp

#if defined(DEBUG)

	/// @todo remove output here, maybe add an additional status() API to retrieve it in case of error
	DOMNode* desc = stamp.type->xmlDesc();
	std::cerr << "Stamp at 0x" << std::hex << stamp.offset << std::dec
			<< " of size " << stamp.type->size() << " type " << *desc
			<< " invalid in stamps buffer " << (const void*) readAccess()
			<< " of size " << getStampsSize() << std::endl;
	delete desc;
#endif
	return false;
}

/// Read a stamp integer value.
bool Stamps::read(const StampInfo::Accessor& stamp, int& val) const
{
	if (!isValid(stamp))
		return false;
	if (!stamp.type->read(val, *this, stamp.offset))
	{
		DOMNode* desc = stamp.type->xmlDesc();
		std::cerr << "Stamp at 0x" << std::hex << stamp.offset << std::dec
				<< " of type " << *desc
				<< " cannot be read as int in Stamps buffer "
				<< (const void*) readAccess() << " of size " << getStampsSize()
				<< std::endl;
		delete desc;
		return false;
	}
	return true;
}

/// Read a stamp float value.
bool Stamps::read(const StampInfo::Accessor& stamp, float& val) const
{
	if (!isValid(stamp))
		return false;
	if (!stamp.type->read(val, *this, stamp.offset))
	{
		DOMNode* desc = stamp.type->xmlDesc();
		std::cerr << "Stamp at 0x" << std::hex << stamp.offset << std::dec
				<< " of type " << *desc
				<< " cannot be read as float in Stamps buffer "
				<< (const void*) readAccess() << " of size " << getStampsSize()
				<< std::endl;
		delete desc;
		return false;
	}
	return true;
}

/// Read a stamp std::string value.
bool Stamps::read(const StampInfo::Accessor& stamp, std::string& val) const
{
	if (!isValid(stamp))
		return false;
	if (!stamp.type->read(val, *this, stamp.offset))
	{
		DOMNode* desc = stamp.type->xmlDesc();
		std::cerr << "Stamp at 0x" << std::hex << stamp.offset << std::dec
				<< " of type " << *desc
				<< " cannot be read as std::string in Stamps buffer "
				<< (const void*) readAccess() << " of size " << getStampsSize()
				<< std::endl;
		delete desc;
		return false;
	}
	return true;
}

StampsWrite::StampsWrite()
{
}

StampsWrite::StampsWrite(const BufferWrite& buf) :
	BufferWrite(buf)
{
}

void StampsWrite::clone(const Stamps& buf, StampList* stlist)
{
	if (!buf.valid())
	{
		clear();
		return;
	}
	if (getSize() > buf.getSize())
	{
		*this = BufferWrite(*this, 0, buf.getSize());
	}
	else if (getSize() < buf.getSize())
	{
		if (stlist == NULL)
			*this = buf.getAllocator()->alloc(buf.getSize());
		else
		{
			*this = BufferWrite(*this, 0, 0); // no need to keep old data
			stlist->resize(*this, buf.getSize());
		}
	}
	memcpy(writeAccess(), buf.readAccess(), buf.getSize());
}

/// Test if a stamp can be read from this buffer.
bool StampsWrite::isValid(const StampInfo::Accessor& stamp) const
{
	if(stamp.type == NULL)
		return false; // no type, no fun
	if (stamp.offset >= 0 && stamp.offset
			+ stamp.type->size() <= getStampsSize())
		return true;
	DOMNode* desc = stamp.type->xmlDesc();
	std::cerr << "Stamp at 0x" << std::hex << stamp.offset << std::dec
			<< " of type " << *desc << " invalid in StampsWrite buffer "
			<< (const void*) readAccess() << " of size " << getStampsSize()
			<< std::endl;
	delete desc;
	return false;
}

/// Read a stamp integer value.
bool StampsWrite::read(const StampInfo::Accessor& stamp, int& val) const
{
	if (!isValid(stamp))
		return false;
	if (!stamp.type->read(val, *this, stamp.offset))
	{
		DOMNode* desc = stamp.type->xmlDesc();
		std::cerr << "Stamp at 0x" << std::hex << stamp.offset << std::dec
				<< " of type " << *desc
				<< " cannot be read as int in StampsWrite buffer "
				<< (const void*) readAccess() << " of size " << getStampsSize()
				<< std::endl;
		delete desc;
		return false;
	}
	return true;
}

/// Read a stamp float value.
bool StampsWrite::read(const StampInfo::Accessor& stamp, float& val) const
{
	if (!isValid(stamp))
		return false;
	if (!stamp.type->read(val, *this, stamp.offset))
	{
		DOMNode* desc = stamp.type->xmlDesc();
		std::cerr << "Stamp at 0x" << std::hex << stamp.offset << std::dec
				<< " of type " << *desc
				<< " cannot be read as float in StampsWrite buffer "
				<< (const void*) readAccess() << " of size " << getStampsSize()
				<< std::endl;
		delete desc;
		return false;
	}
	return true;
}
/// Read a stamp std::string value.
bool StampsWrite::read(const StampInfo::Accessor& stamp, std::string& val) const
{
	if (!isValid(stamp))
		return false;
	if (!stamp.type->read(val, *this, stamp.offset))
	{
		DOMNode* desc = stamp.type->xmlDesc();
		std::cerr << "Stamp at 0x" << std::hex << stamp.offset << std::dec
				<< " of type " << *desc
				<< " cannot be read as std::string in StampsWrite buffer "
				<< (const void*) readAccess() << " of size " << getStampsSize()
				<< std::endl;
		delete desc;
		return false;
	}
	return true;
}

/// Write a stamp integer value.
bool StampsWrite::write(const StampInfo::Accessor& stamp, int val)
{
	if (!startWrite(stamp))
		return false;
	return stamp.type->write(val, *this, stamp.offset, stamp.list);
}

/// Write a stamp float value.
bool StampsWrite::write(const StampInfo::Accessor& stamp, float val)
{
	if (!startWrite(stamp))
		return false;
	return stamp.type->write(val, *this, stamp.offset, stamp.list);
}

/// Write a stamp std::string value.
bool StampsWrite::write(const StampInfo::Accessor& stamp,
		const std::string& val)
{
	if (!startWrite(stamp))
		return false;
	return stamp.type->write(val, *this, stamp.offset, stamp.list);
}

/// Prepare the writing of a stamp to this buffer.
bool StampsWrite::startWrite(const StampInfo::Accessor& stamp)
{
	if (stamp.type == NULL || stamp.list == NULL || stamp.offset < 0)
		return false;
	int reqsize = stamp.offset + stamp.type->size();
	if (getSize() < reqsize)
	{
		if (stamp.list == NULL)
			return false; // can't resize
		stamp.list->resize(*this, reqsize);
	}
	return true;
}

void StampList::resize(BufferWrite& buffer, int minSize)
{
	int oldsize = buffer.getSize();
	if (oldsize >= minSize)
		return; // nothing to do
	if (minSize < baseSize)
		minSize = baseSize; // alloc at least baseSize
	if (!buffer.valid() || buffer.expand(minSize) == false)
	{ // realloc using pool
		int size = pool.getCurrentBufferSize();
		if (size < minSize)
			size = minSize;
		BufferWrite newbuf = pool.alloc(buffer.getAllocator(), size);
		if (oldsize > 0)
		{ // copy old content
			memcpy(newbuf.writeAccess(), buffer.readAccess(), oldsize);
		}
		memset(newbuf.writeAccess() + oldsize, 0, size - oldsize); // init stamps to 0
		buffer = newbuf; // replace buffer
	}
	if (oldsize < baseSize) // make sure the size is correct
		*buffer.getWrite<int> (0) = baseSize;
}

} // namespace flowvr
