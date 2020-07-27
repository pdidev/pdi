// -*- c++ -*-

#ifndef TRACES_H_INCLUDED
#define TRACES_H_INCLUDED

#include <vector>
#include <map>
#include <string>

using namespace std;




class Traces {

public:

  struct Event { // event vectors always sorted by time
    double t;     // time (between 0 and t_max)
    int ev_id;    // index into tracelist table
    int val;      // associated value
  }; 

  struct Link;

  struct TracedEvent {
    string obj;   // id of object this relates to 
    string name;  // event name

    vector<int> nextEvents; // corresponding ending tracedEvents
    vector<Link*> links;    // corresponding links

  };

  
  struct Link { // the defined links
    string source, sourceport; 
    string dest, destport;

    vector<Event> events; // associated events
  };
  

  void print(); // dump all traces


  // begin / end time (in seconds since epoch...)
  double t0, t1;   
  
  // current time
  double t; 

  bool parse(const char *filename, int verbose = 0);  // parse a .gltrace.xml file

  // 0: waiting
  // 1: running
  // -1: unknown
  int getModuleState(const std::string & name); 
  
  // 0 <= x <= 1: a message is this far between the source and the destination port 
  // -1: nothing is happening.
  double getLinkState(const std::string & source, const std::string & sourceport, 
                      const std::string & dest, const std::string & destport);

  
  // "event time" = time where each interval between 2 events lasts 1   
  double toEventTime(double et);   
  double fromEventTime(double t); 

private:
  vector<TracedEvent> tracelist;  // event id -> Event definition
  vector<Link> links;             // links

  vector<double> timestamps;      // all timestamps (used for non-uniform play)

  int find_event(string obj, string name); 
  
  
};







#endif

