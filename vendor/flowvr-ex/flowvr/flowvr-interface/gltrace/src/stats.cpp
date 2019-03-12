/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                             Utils                               *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490) ALL RIGHTS RESERVED.                                 *
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
*    Clement Menier,                                              *
*    Bruno Raffin,                                                *
*    Sophie Robert,                                               *
*    Emmanuel Melin.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: src/log/stats.cpp                                         *
*                                                                 *
* Contacts:                                                       *
*  28/05/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include <flowvr/trace.h>
#include <iostream>
#include <fstream>
#include <vector>
#include <map>
#include <string>
#include <math.h>
#include <sys/time.h>

using namespace flowvr;

typedef flowvr::Trace::cycle_t cycle_t;

class LogHost
{
public:
  std::vector<double> timeTable;
  std::vector<cycle_t> cycleTable;
  double freq;
};

typedef std::map<std::string,LogHost> HostMap;

HostMap hosts;

double lasttime;

class LogEntry
{
public:
  double t;
  int val;
  LogEntry(double _t=0, int _val=0) : t(_t), val(_val) {}
};

class LogTrace
{
public:
  //int id;
  std::string obj;
  std::string name;
  std::vector<LogEntry> evt;
};

std::vector<LogTrace> traces;

void addHostTime(const std::string& hostname, int num, double t, cycle_t c)
{
  std::cout << "Adding time "<<num<<" from "<<hostname<<": t="<<t<<std::endl;//" c="<<c<<std::endl;
  LogHost& host = hosts[hostname];
  if (host.timeTable.size()<=(unsigned)num)
    host.timeTable.resize(num+1);
  host.timeTable[num] = t;
  if (host.cycleTable.size()<=(unsigned)num)
    host.cycleTable.resize(num+1);
  host.cycleTable[num] = c;
}

void addTrace(int num, const std::string& name, const std::string& from)
{
  std::cout << "Adding trace "<<num<<" from "<<from<<" name "<<name<<std::endl;
  if(traces.size()<=(unsigned)num)
    traces.resize(num+1);
  traces[num].name = name;
  traces[num].obj = from;
}

bool init(const char* resultsfile, const char* masterhost = NULL)
{
  lasttime=0;
  std::ifstream results(resultsfile);
  if (!results.is_open())
  {
    std::cerr << "ERROR opening file "<<resultsfile<<std::endl;
    return false;
  }
  std::string line;
  while (getline(results,line))
  {
    xml::DOMParser parser;
    if (parser.parseString(line.c_str()))
    {
      std::cerr << "ERROR parsing line "<<line<<std::endl;
      return false;
    }
    xml::DOMElement* result = parser.getDocument()->RootElement()->FirstChildElement();
    if (result == NULL)
    {
    }
    else if (!strcmp(result->getNodeName(),"time"))
    {
      const char* tname = result->Attribute("name");
      if (tname != NULL && tname[0])
      {
	++tname;
	int num = atoi(tname);
	while (*tname>='0' && *tname<='9') ++tname;
	if (*tname=='-')
	{
	  ++tname;
	  std::string hostname = tname;
	  unsigned int sec = atoi(result->Attribute("sec"));
	  unsigned int usec = atoi(result->Attribute("usec"));
	  double t = sec+usec/1000000.0;
	  cycle_t c;// atoll(result->Attribute("cycle"));
	  addHostTime(hostname,num,t,c);
	}
      }
    }
    else if (!strcmp(result->getNodeName(),"trace"))
    {
      const char* name = result->Attribute("name");
      const char* id = result->Attribute("id");
      const char* from = result->Attribute("from");
      if (name && name[0] && id && id[0] && from && from[0])
      {
	addTrace(atoi(id),name,from);
      }
    }
  }

  // sync all times to the given master
  if (masterhost && *masterhost)
  {
    LogHost& master = hosts[masterhost];
    for (HostMap::iterator it = hosts.begin(); it != hosts.end(); ++it)
    {
      LogHost& host = it->second;
      if (host.timeTable.size()>master.timeTable.size())
      {
	host.timeTable.resize(master.timeTable.size());
	host.cycleTable.resize(master.timeTable.size());
      }
      for (unsigned int i=0;i<host.timeTable.size();i++)
	host.timeTable[i] = master.timeTable[i];
    }
  }

  for (HostMap::iterator it = hosts.begin(); it != hosts.end(); ++it)
  {
    LogHost& host = it->second;
    // substract the first time to all times
    for (unsigned int i=1;i<host.timeTable.size();i++)
    {
      host.timeTable[i] -= host.timeTable[0];
    }
    host.timeTable[0] = 0;
    // then compute the frequency
/*    if (host.timeTable.size()>1)
      host.freq = (host.cycleTable[host.cycleTable.size()-1]-host.cycleTable[0])/host.timeTable[host.timeTable.size()-1];
    else
        host.freq = 1000000000.0; // default: 1 GHz*/
    std::cout << "CPU Frequency "<<it->first<<": "<<(int)(host.freq/1000000)<<" Mhz"<<std::endl;
  }

  return true;
}

int operator<(const LogEntry& a, const LogEntry &b)
{
  return a.t < b.t;
}

bool readLogs(const char* logprefix)
{
  for (HostMap::iterator it = hosts.begin(); it != hosts.end(); ++it)
  {
    std::string hostname = it->first;
    std::cout << "Processing log from "<<hostname<<std::endl;
    LogHost& host = it->second;
    std::string fname = logprefix;
    fname += hostname;
    std::ifstream log(fname.c_str(),std::ifstream::in|std::ifstream::binary);
    if (!log.is_open())
    {
      std::cerr << "ERROR opening file "<<fname<<std::endl;
      return false;
    }
    cycle_t c0 = host.cycleTable[0];
    double freq = host.freq;
    int nrec = 0;
    while (log.good())
    {
      unsigned int size = ((unsigned)log.get())*4;
      if (size>1024) break;
      if (size!=16)
      {
	std::cerr << "Unsupported size 0x"<<std::hex<<size<<std::dec<<std::endl;
	log.seekg(size-1,std::ifstream::cur);
      }
      else
      {
	cycle_t c;//=0;
	log.read((char*)&c,sizeof(cycle_t)-1);
	int id = 0;
	log.read((char*)&id,sizeof(int));
	int val = 0;
	log.read((char*)&val,sizeof(int));
	double t =0;// (double)(c-c0)/freq;
	if ((unsigned)id<(unsigned)traces.size())
	{
	  traces[id].evt.push_back(LogEntry(t,val));
	  ++nrec;
	  if (t>lasttime) lasttime=t;
	}
	else
	  std::cerr << "Invalid log ID "<<id<<std::endl;
      }
    }
    std::cout << nrec << " entries read."<<std::endl;
  }

  std::cout << "Sorting entries..."<<std::endl;

  for (unsigned int i=0;i<traces.size();i++)
  {
    std::sort(traces[i].evt.begin(),traces[i].evt.end());
    unsigned int nval = traces[i].evt.size();
    if (nval>0)
    {
      int base = traces[i].evt[0].val;
      bool linear = true;
      bool valid = true;
      for (unsigned int j=1;j<nval;j++)
      {
	if (traces[i].evt[j].val!=base+(int)j) linear = false;
	if (traces[i].evt[j].val<traces[i].evt[j-1].val)
	{
	  std::cerr << "ERROR(Trace "<<traces[i].obj<<":"<<traces[i].name<<"): invalid value at event "<<j<<std::endl;
	  valid = false;
	  break;
	}
	if (traces[i].evt[j].t<traces[i].evt[j-1].t)
	{
	  std::cerr << "ERROR(Trace "<<traces[i].obj<<":"<<traces[i].name<<"): invalid time at event "<<j<<std::endl;
	  valid = false;
	  break;
	}
      }
      if (valid)
      {
	if (linear)
	  std::cout << "Trace "<<traces[i].obj<<":"<<traces[i].name<<" val(it)=it+ "<<base<<std::endl;
	else
	  std::cout << "Trace "<<traces[i].obj<<":"<<traces[i].name<<" val(it)=val(it-1)+D"<<std::endl;
      }
    }
  }

  return true;
}

int findTrace(std::string name)
{
  for (unsigned int i=0;i<traces.size();i++)
  {
    std::string s = traces[i].obj+":"+traces[i].name;
    if (s == name) return i;
  }
  return -1;
}

double calcSpeed(std::string name, double tmin=0, double tmax=0)
{
  int id = findTrace(name);
  if (id<0) return 0;
  int it0,it1;
  it0 = 0;
  if (tmin>0)
  {
    while (it0<(int)traces[id].evt.size()-1 && traces[id].evt[it0].t<tmin)
      ++it0;
  }
  it1 = traces[id].evt.size()-1;
  if (tmin>0)
  {
    while (it1>0 && traces[id].evt[it1].t>tmax)
      --it1;
  }
  if (it0>=it1)
    return 0;
  double t = traces[id].evt[it1].t-traces[id].evt[it0].t;
  double r = (it1-it0)/t;
  std::cout << name<<" "<<it1-it0+1<<" events in "<<t<<" sec: "<<r<<" iter/s"<<std::endl;
  return r;
}

int split(const std::string& input, const std::string& delimiter, std::vector<std::string>& results)
{
  int iPos = 0;
  int newPos = -1;
  int sizeS2 = delimiter.size();
  int isize = input.size();

  std::vector<int> positions;

  newPos = input.find (delimiter, 0);

  if( newPos < 0 ) { return 0; }

  int numFound = 0;

  while( newPos > iPos )
  {
    numFound++;
    positions.push_back(newPos);
    iPos = newPos;
    newPos = input.find (delimiter, iPos+sizeS2+1);
  }

  for( int i=0; i <= (int)positions.size(); i++ )
  {
    std::string s;
    if( i == 0 ) { s = input.substr( i, positions[i] ); }
    else
    {
      int offset = positions[i-1] + sizeS2;
      if( offset < isize )
      {
	if( i == (int)positions.size() )
	  s = input.substr(offset);
	else if( i > 0 )
	  s = input.substr( positions[i-1] + sizeS2, 
			    positions[i] - positions[i-1] - sizeS2 );
      }
    }
    if( s.size() > 0 )
      results.push_back(s);
  }
  return numFound;
}

double calcLatency(std::string path,double tmin=0,double tmax=0)
{
  std::vector<std::string> tracenames;
  split(path,"=",tracenames);
  if (tracenames.size()<=1) return calcSpeed(path,tmin,tmax);
  std::vector<int> traceids;
  traceids.resize(tracenames.size());
  std::cout << "Latency of ";
  bool valid = true;
  double val = 0;
  for (unsigned int i=0;i<tracenames.size();i++)
  {
    int id;
    if (tracenames[i][0] == '-')
    {
      id = atoi(tracenames[i].c_str());
      if (id >= 0) valid = false;
    }
    else
    {
      id = findTrace(tracenames[i]);
      if (id<0) valid = false;
    }
    traceids[i] = id;
    if (i) std::cout << "<-";
    std::cout << tracenames[i]<<"("<<id<<")";
  }
  if (valid)
  {
    int id = traceids[0];
    int it0,it1;
    it0 = 0;
    if (tmin>0)
    {
      while (it0<(int)traces[id].evt.size()-1 && traces[id].evt[it0].t<tmin)
	++it0;
    }
    it1 = traces[id].evt.size()-1;
    if (tmin>0)
    {
      while (it1>0 && traces[id].evt[it1].t>tmax)
	--it1;
    }
    if (it0>=it1)
      return 0;

    int idiff = 0;
    double tdiff = 0;
    int nb = 0;
    for (int num=it0;num<=it1;num++)
    {
      int n = num;
      int i;
      double t1 = 0;
      //std::cout<<n<<" ";
      for (i=0;i<(int)traceids.size()-1;i++)
      {
	if (traceids[i]<0)
	  n += traceids[i];
	else
	{
	  if (!((unsigned)n<traces[traceids[i]].evt.size()))
	    break;
	  double t2 = traces[traceids[i]].evt[n].t;
	  if (t1 && t2>t1)
	  {
	    std::cerr << "Temporal causality destroyed at "<<tracenames[i]<<"["<<n<<"]: "<<t2<<">"<<t1<<std::endl;
	  }
	  n = traces[traceids[i]].evt[n].val;
	  t1=t2;
	}
	//std::cout << n<<" ";
      }
      //std::cout << std::endl;
      if (i==(int)traceids.size()-1)
      {
	idiff += (num-n);
	tdiff += (traces[id].evt[num].t-traces[traceids[i]].evt[n].t);
	++nb;
      }
    }
    if (nb>0)
    {
      val = 1000*tdiff / nb;
      double ival = (double)idiff/(double)nb;
      std::cout <<" "<<nb<<" events: "<<val<<" ms ("<<ival<<" iter)";
    }
    
  }
  std::cout << std::endl;
  return val;
}

int main ( int argc, char ** argv )
{

  if (argc < 3)
  {
    std::cout << "Usage: "<<argv[0]<<" resultsfiles logfileprefix [timemasterhost [tmin tmax traces...]]]"<<std::endl;
    return -1;
  }

  if (!init(argv[1],(argc>=4?argv[3]:NULL)))
  {
    return 10;
  }

  if (!readLogs(argv[2]))
  {
    return 20;
  }

  if (argc>=7)
  { // stat computation mode
    double tmin = atof(argv[4]);
    double tmax = atof(argv[5]);
    for (int a=6;a<argc;a++)
    {
      std::cerr << '\t' << calcLatency(argv[a],tmin,tmax); // << std::endl;
    }
  }

  return 0;
}
