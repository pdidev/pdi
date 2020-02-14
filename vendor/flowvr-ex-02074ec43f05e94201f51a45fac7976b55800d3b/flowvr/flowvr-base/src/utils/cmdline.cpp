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
* File: src/ftlm/cmdline.cpp                                      *
*                                                                 *
* Contacts: 10/26/2005 Jeremie Allard <Jeremie.Allard@imag.fr>    *
*                                                                 *
******************************************************************/
#include <flowvr/utils/cmdline.h>
#include <getopt.h>
#include <iostream>
#include <vector>

namespace
{
  flowvr::utils::FlagOption* helpoption = NULL;
  std::vector<flowvr::utils::BaseOption*>      _opts;
}

namespace flowvr
{
	namespace utils
	{

FlagOption* helpoption = NULL;

BaseOption::BaseOption(const char* _longname, char _shortname, const char* _description, ArgType _arg)
: longname(_longname)
, shortname(_shortname)
, description(_description)
, arg(_arg)
, hasdefault(false)
, count(0)
{
  std::vector<BaseOption*>& opts = CmdLine::opts();
  opts.push_back(this);
}

BaseOption::~BaseOption()
{
  std::vector<BaseOption*>& opts = CmdLine::opts();
  std::vector<BaseOption*>::iterator it;
  for (it = opts.begin(); it != opts.end(); ++it)
  {
    opts.erase(it);
    break;
  }
}

CmdLine::CmdLine(const std::string &(_description))
: description(_description)
{
	if( helpoption == NULL )
		helpoption = new FlagOption("help",'h',"Display available options");
}

CmdLine::~CmdLine()
{
}




bool CmdLine::parse(int argc, char** argv, bool* error)
{
	// using a side effect to prevent getopt to output a string to stdout
	// when an error is envountered. Not very nice, but documented in the
	// manuals for using getopt.
	opterr = 0;
	optind = 1;

  // First create the list of options
	int nopts = opts().size();
	std::vector<struct option> ops(nopts + 1);

	struct option* options = &ops[0];
	std::string shortoptions;
	for (int i = 0; i < nopts; i++)
	{
		options[i].name = opts()[i]->longname;
		options[i].has_arg = opts()[i]->arg;
		options[i].flag = NULL;
		options[i].val = 256 + i;
		if (opts()[i]->shortname != '\0')
		{
			shortoptions += opts()[i]->shortname;
			if (opts()[i]->arg == BaseOption::REQ_ARG)
				shortoptions += ':';
			else if (opts()[i]->arg == BaseOption::OPT_ARG)
				shortoptions += "::";
		}
	}
	options[nopts].name = NULL;
	options[nopts].has_arg = 0;
	options[nopts].flag = NULL;
	options[nopts].val = 0;

	int option_index = -1;
	int c;

	const char* program = strrchr(argv[0], '/');
	if (program != NULL)
		++program;
	else
		program = argv[0];

#ifndef __APPLE__
	while ((c = getopt_long_only(argc, argv, shortoptions.c_str(), options,
			&option_index)) != -1)
#else
	while( (c = getopt_long(argc,argv,shortoptions.c_str(),options,&option_index)) != -1)
#endif
	{
		if (c == '?' || c == ':')
		{
			if (error != NULL)
				*error = true;
			return false;
		}
		bool longopt = true;
		if (c != 0 && (unsigned) option_index >= (unsigned) nopts)
		{
			longopt = false;
			for (int i = 0; i < nopts; i++)
				if (opts()[i]->shortname == c)
				{
					option_index = i;
					break;
				}
		}
		if ((unsigned) option_index >= (unsigned) nopts)
		{
			std::cerr << "Error parsing options" << std::endl;
			if (error != NULL)
				*error = true;
			return false;
		}

		BaseOption* opt = opts()[option_index];

		if (optarg && *optarg)
		{
			if (opt->arg == BaseOption::NO_ARG)
			{
				std::cerr << program << ": option ";
				if (longopt)
					std::cerr << "--" << opt->longname;
				else
					std::cerr << '-' << opt->shortname;
				std::cerr << " requires no value while \"" << optarg
						<< "\" was specified." << std::endl;
				if (error != NULL)
					*error = true;
				return false;
			}
			else
			{
				if (!opt->set(optarg))
				{
					std::cerr << program << ": incorrect value for option ";
					if (longopt)
						std::cerr << "--" << opt->longname;
					else
						std::cerr << "-" << opt->shortname;
					std::cerr << " .\n";
					if (error != NULL)
						*error = true;
					return false;
				}
			}
		}
		else
		{
			if (opt->arg == BaseOption::REQ_ARG)
			{
				std::cerr << program << ": option ";
				if (longopt)
					std::cerr << "--" << opt->longname;
				else
					std::cerr << '-' << opt->shortname;
				std::cerr << " requires a value." << std::endl;
				if (error != NULL)
					*error = true;
				return false;
			}
			else
			{
				if (!opt->set())
				{
					std::cerr << program << ": error while treating option ";
					if (longopt)
						std::cerr << "--" << opt->longname;
					else
						std::cerr << '-' << opt->shortname;
					std::cerr << " .\n";
					if (error != NULL)
						*error = true;
					return false;
				}
			}
		}
		option_index = -1;
	}
	for (int i = optind; i < argc; i++)
	{
		args.push_back(argv[i]);
	}
	if (error != NULL)
		*error = false;
	if (helpoption != NULL && helpoption->count > 0)
	{
//		if (description.empty())
//			std::cout << program << "\n";
//		std::cout << help() << std::endl;
		return false;
	}
	return true;
}

std::string CmdLine::help() const
{
  std::ostringstream ss;
  if (!description.empty())
    ss << description << std::endl;

  ss << "Available options:\n";
  for (unsigned int i=0;i<opts().size();i++)
    ss << opts()[i]->help() << std::endl;
  return ss.str();
}


std::vector<BaseOption*>& CmdLine::opts()
{
  return _opts;
}

BaseOption* CmdLine::getOpt(const std::string &longname_) const
  {
    for (std::vector<BaseOption*>::const_iterator it = opts().begin(); it
			< opts().end(); ++it)
	{
		if (strcmp((*it)->longname, longname_.c_str()) == 0)
			return (*it);
	}
	return NULL;
  }

  void CmdLine::setDesc(const std::string &desc)
  {
	  description = desc;
    //    strcpy(description,desc.c_str()); 
  }


  std::string CmdLine::getValueString() const
  {
	std::string sRet;
	for (std::vector<BaseOption*>::const_iterator it = opts().begin(); it
			< opts().end(); ++it)
	{
		if( (*it) == helpoption)
			continue;


		sRet += std::string((*it)->longname) + std::string(":\t\t[") + (*it)->get()
			  + "]\t\t"
			  + ((*it)->count ? "SET" : "DEFAULT")
			  + std::string("\n");
	}

	return sRet;
  }

  void CmdLine::getValueMap( std::map<std::string, BaseOption*> &storage )
  {
		for (std::vector<BaseOption*>::const_iterator it = opts().begin(); it
				< opts().end(); ++it)
		{
			if( (*it) == helpoption)
				continue;

			storage[std::string((*it)->longname)] = (*it);
		}
  }

} // namespace utils
} // namespace flowvr
