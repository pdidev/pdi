/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                       Template Library                          *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA.  ALL RIGHTS RESERVED.                                    *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Clement Menier.                                              *
*                                                                 *
*******************************************************************
*                                                                 *
* File: ./include/ftl/cmdline.h                                   *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#ifndef FTL_CMDLINE_H
#define FTL_CMDLINE_H

#include <ftl/type.h>
#include <ftl/vec.h>
#include <vector>
#include <map>
#include <string>
#include <sstream>
#include <typeinfo>

namespace flowvr
{
	namespace utils
	{

/** Base Class for describing an option. Should not be used directly.
 */
class BaseOption
{
public:
  enum ArgType {
    NO_ARG=0,
    REQ_ARG=1,
    OPT_ARG=2 };

  const char* longname;
  char shortname;
  std::string description;
  ArgType arg;
  bool hasdefault;
  std::string _argval;

  /// Counts the number of times this option was given.
  int count;

  /**
   * @param _longname a <i>persistent</i> pointer to the string describing the longname of this option
   * @param _shortname a character for abbreviation
   * @param _description a descriptive text for this option, will be printed on help or error
   * @param _arg see ArgType
   */
  BaseOption(const char* _longname, char _shortname, const char* _description, ArgType _arg);
  virtual ~BaseOption();
  virtual bool set() { ++count; return true; }
  virtual bool set(const char* argval) { ++count; return true; }

  virtual std::string get() const
  {
	  return _argval;
  }

  virtual std::string help() const
  {
	 return description;
  }
};

/** Option of type T (int, std::string, ...).
 */
template<typename T>
class Option : public BaseOption
{
public:
  /** Declaration of an option.
   * @param _longname Long name version (--) if not NULL. (pointer MUST persist)
   * @param _shortname Short name version (single character).
   * @param _description Text description of the option.
   * @param _val Pointer to a space where to store the option value (pointer MUST persist)
   * @param optional Indicates whether the argument is required.
   */
  Option(const char* _longname, char _shortname, const char* _description, T* _val, bool optional=false)
  : BaseOption(_longname, _shortname, _description, optional?BaseOption::OPT_ARG:BaseOption::REQ_ARG)
  , adopt(true)
  {
    val = _val;
    hasdefault = true;
  }

  /** Declaration of an option.
   * @param _longname Long name version (--) if not NULL (pointer MUST persist)
   * @param _shortname Short name version (single character).
   * @param _description Text description of the option (pointer MUST persist).
   * @param optional Indicates whether the argument is required.
   */
  Option(const char* _longname, char _shortname, const char* _description, bool optional=false)
  : BaseOption(_longname, _shortname, _description, optional?BaseOption::OPT_ARG:BaseOption::REQ_ARG)
  , adopt(false)
  {
    val = new T();
    hasdefault = false;
  }

  /** Declaration of an option.
   * @param _longname Long name version (--) if not NULL (pointer MUST persist)
   * @param _shortname Short name version (single character).
   * @param _description Text description of the option (pointer MUST persist)
   * @param defaultval Default value if no argument is given.
   * @param optional Indicates whether the argument is required.
   */
  Option(const char* _longname, char _shortname, const char* _description, const T& defaultval, bool optional=true)
  : BaseOption(_longname, _shortname, _description, optional?BaseOption::OPT_ARG:BaseOption::REQ_ARG)
  , adopt(false)
  {
    val = new T(defaultval);
    hasdefault = true;
  }

  ~Option()
  {
	  if(!adopt)
		  delete val;
  }

  /// Retrieve the value of the option.
  operator T&() { return *val; }
  operator const T&() const { return *val; }

  T& value() { return *val; }
  const T& value() const { return *val; }

  /// Set a default value.
  void operator=(const T& v)
  {
    *val = v;
    hasdefault = true;
  }

  std::string type() const;
  std::string defaultval() const;

  std::string get() const
  {
	  if(_argval.empty() && hasdefault)
		  return defaultval();
	  else
		  return BaseOption::get();
  }


  /// Set the value of the option from a text.
  virtual bool set(const char* argval)
  {
    if ( !ftl::Type::assignString(*val, std::string(argval)) )
    {
      return false;
    }
    else
    {
      _argval = std::string(argval); // copy assignment string
      return BaseOption::set(argval);
    }
  }

  /// Build the help line concerning this option.
  virtual std::string help() const
  {
    std::ostringstream ss;
    ss << "  ";
    if (shortname!='\0')
    {
      ss << '-' << shortname;
      if (arg == BaseOption::OPT_ARG)
	ss << " [" << type() << ']';
      else if (arg == BaseOption::REQ_ARG)
	ss << " " << type();

      if (longname!=NULL && longname[0]!='\0')
        ss << " or ";
    }
    if (longname!=NULL && longname[0]!='\0')
    {
      ss << "--" << longname;
      if (arg == BaseOption::OPT_ARG)
	ss << " [" << type() << ']';
      else if (arg == BaseOption::REQ_ARG)
	ss << " " << type();
    }

    ss << "\n          ";
    if (!description.empty())
      ss << description;


    if (arg != BaseOption::NO_ARG && hasdefault)
      ss << " (default "<<defaultval()<<")";
    return ss.str();
  }

protected:
  T* val;
  bool adopt;
};

template <typename T>
__inline__ std::string Option<T>::type() const
{
  return ftl::Type::name(ftl::Type::get<T>(*val));
}

template <>
__inline__ std::string Option<std::string>::type() const
{
  return ftl::Type::name(ftl::Type::String0);
}

template <typename T>
__inline__ std::string Option<T>::defaultval() const
{
    std::ostringstream os;
    os << *val;
    return os.str();
//  std::string res;
//  ftl::Type::Assign<std::string>::do_assign(res,ftl::Type::get<T>(*val),(const void*)val);
//  return res;
}

template <>
__inline__ std::string Option<std::string>::defaultval() const
{
  return *val;
}

/** Specialization of Option for booleans (flags).
 */
template<>
class Option<bool> : public BaseOption
{
public:
  /** Declaration of an option.
   * @param _longname Long name version (--) if not NULL (pointer MUST persist)
   * @param _shortname Short name version (single character).
   * @param _description Text description of the option (pointer MUST persist)
   * @param _val Pointer to a space where to store the option value (pointer MUST persist).
   */
  Option(const char* _longname, char _shortname, const char* _description, bool* _val)
  : BaseOption(_longname, _shortname, _description, BaseOption::NO_ARG)
  , adopt(true)
  {
    val = _val;
  }

  /** Declaration of an option.
   * @param _longname Long name version (--) if not NULL.
   * @param _shortname Short name version (single character).
   * @param _description Text description of the option.
   */
  Option(const char* _longname, char _shortname, const char* _description)
  : BaseOption(_longname, _shortname, _description, BaseOption::NO_ARG)
  , adopt(false)
  {
    val = new bool(false);
  }


  /** Declaration of an option.
   * @param _longname Long name version (--) if not NULL (pointer MUST persist)
   * @param _shortname Short name version (single character).
   * @param _description Text description of the option (pointer MUST persist)
   * @param defaultval Default value if no argument is given.
   * @param optional Indicates whether the argument is required.
   */
  Option(const char* _longname, char _shortname, const char* _description, const bool& defaultval, bool optional=true)
  : BaseOption(_longname, _shortname, _description, optional?BaseOption::OPT_ARG:BaseOption::REQ_ARG)
  , adopt(false)
  {
    val = new bool(defaultval);
    hasdefault = true;
  }


  ~Option()
  {
	  if(!adopt)
		  delete val;
  }

  /// Retrieve the value of the option (
  operator bool&() { return *val; }
  operator const bool&() const { return *val; }

  bool& value() { return *val; }
  const bool& value() const { return *val; }

  void operator=(const bool& v)
  {
    *val = v;
  }

  virtual bool set()
  {
    *val = true;
    return BaseOption::set();
  }

  std::string get() const
  {
	  if(_argval.empty())
		  return (*val ? "true" : "false");
	  else
		  return BaseOption::get();
  }


  virtual std::string help() const
  {
    std::ostringstream ss;
    ss << "  ";
    if (shortname!='\0')
    {
      ss << '-' << shortname;
      if (longname!=NULL && longname[0]!='\0')
        ss << " or ";
    }
    if (longname!=NULL && longname[0]!='\0')
    {
      ss << "--" << longname;
    }

    ss << "\n          ";
    if (!description.empty())
      ss << description;
    return ss.str();
  }

protected:
  bool* val;
  bool adopt;
};

/// A Flag option is a boolean option.
typedef Option<bool> FlagOption;

/// Helper class to parse command line arguments
class CmdLine
{
public:

  CmdLine(const std::string &_description = std::string());
  ~CmdLine();


  /**
   *  Adds a flag option.
   *  Note that the option is created and inserted into the cmdline object by a side effect.
   *  Note that flag options can be tested using the Option.count member for being passed.
   *  @param longname persisting pointer to the longname for this option
   *  @param description persisting pointer to the description for this option
   */
  void opt(const char* longname, char shortname, const char* description)
  {
    new FlagOption(longname, shortname, description);
  }

  /// Adds a typed option.
  template <class T>
  void opt(const char* longname, char shortname, const char* description, T* val)
  {
    new Option<T>(longname, shortname, description, val);
  }

  /// Adds a typed option with specifying if an argument is required.
  template <class T>
  void opt(const char* longname, char shortname, const char* description, T* val, bool optional)
  {
    new Option<T>(longname, shortname, description, val, optional);
  }

  /** Parse the arguments.
   * @param error Pointer to a boolean for retrieving if the command line has errors.
   * @return True if the program should continue, false if the program should end
   *  (either because the option --help was used or because an error occured).
   */
  bool parse(int argc, char** argv, bool* error=NULL);

  /// Remaining arguments.
  std::vector<std::string> args;

  /// Create usage help message.
  std::string help() const;

  /// Return an option given its longname (NULL if does not exists)
  BaseOption* getOpt(const std::string &longname_) const;

  /// Set or change  the cmdline description using the std::string given in parameter
  void setDesc(const std::string &desc);

  /// List of available options (static for a program).
  static std::vector<BaseOption*>& opts();

  /// retrieve the set of possible options, their current values
  /// as well as a flag whether these values were actually set or
  /// derived from a default value as a string that the user
  /// can just print. This is a macro function, it is intended for
  /// a quick debugging on the validity of the options passed to a module,
  /// and not for advanced interaction with the command line.
  std::string getValueString() const;

  /// get all options given with name maps to value
  /// by pointing to the option object directly
  /// @param storage the map to store the query results to
  void getValueMap( std::map<std::string, BaseOption*> &storage );
 protected:
  std::string description;
};

} // namespace utils
} // namespace flowvr

#endif

