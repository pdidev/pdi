
#include <cstdio>
#include <cstring>
#include <cmath>
#include <cassert>

//extern "C" {
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <libxml/xmlreader.h>
//}

#include <iostream>
#include <algorithm>

#include "traces.h"





static xmlNodePtr get_child_element(xmlNodePtr node, const char *tag) {
  for(xmlNodePtr child = node->children; child; child = child->next) {
    //    printf("get_child_element %s\n", node->name);
    if(child->type == XML_ELEMENT_NODE && 
       !strcmp((const char*)child->name, tag)) return child; 
  }
  return NULL; 
}

static const char *get_element_attribute(xmlNodePtr node, const char *attrname) {
  for(xmlAttrPtr attr = node->properties; attr; attr = attr->next) 
    if(!strcmp((const char*)attr->name, attrname) && 
       attr->children) return (const char*)attr->children->content; 
  return NULL; 
}







int Traces::find_event(string obj, string name) {
  for(int i = 0; i < tracelist.size(); i++) 
    if(tracelist[i].obj == obj &&
       tracelist[i].name == name) return i; 
  return -1;
}

typedef unsigned int uint32; 
typedef unsigned long uint64;

struct Chunk { // chunks, as read from the trace logs
  uint32 size; 
  uint64 tv_sec, tv_usec; 
  uint32 ev_id;
  uint32 val;
} __attribute__((packed));

static bool cmp_chunk_time(const Chunk &i, const Chunk &j) {
  return i.tv_sec < j.tv_sec || (i.tv_sec == j.tv_sec && i.tv_usec < j.tv_usec);
}


// static void find_involved_link(

static void chunks_to_evlists(vector<Chunk> &chunks, 
                              vector<Traces::TracedEvent> &tracelist,
                              double &t0, double &t1) {
  // sort all by time
  sort(chunks.begin(), chunks.end(), cmp_chunk_time); 

  int nchunk = chunks.size();
  
  for(int i = nchunk - 1; i >= 0; i--) {
    chunks[i].tv_sec -= chunks[0].tv_sec;
  }
  
  if(nchunk < 2) {
    t0 = 0.0;
    t1 = 1.0;
  } else {
    t0 = chunks[0].tv_sec + chunks[0].tv_usec * 1e-6;   
    t1 = chunks[nchunk - 1].tv_sec + chunks[nchunk - 1].tv_usec * 1e-6;   
  }

  printf("time range %.6f %.6f\n", t0, t1);

  for(int i = 0; i < nchunk; i++) {
    const Chunk &chunk = chunks[i];
    Traces::Event ev = {
      chunk.tv_sec + chunk.tv_usec * 1e-6, 
      chunk.ev_id, 
      chunk.val};
    Traces::TracedEvent &tev = tracelist[chunk.ev_id];    

    // see where we are going to file this event...
    for(int j = 0; j < tev.links.size(); j++) {
      Traces::Link &link = *tev.links[j];
      link.events.push_back(ev);
    }
    
  }
  
}

#define XMLASSERT(cond) if(!(cond)) { \
    fprintf(stderr, "XML bad structure in %s, parser at %s:%d\n", filename, __FILE__, __LINE__); \
    goto err;                                                  \
}



bool Traces::parse(const char *filename, int verbose) {
  xmlDocPtr doc = xmlParseFile(filename);
  
  if (doc == NULL) {
    cout << "Error parsing the "<< filename << " !" << endl;
    goto err;
  }
  
  xmlNodePtr node;
  
  node = doc->children; 
  
  XMLASSERT(node->type == XML_ELEMENT_NODE && 
            !strcmp((const char*)node->name, "gltrace"));
  
  { // parse tracelist
    xmlNodePtr xmllist = get_child_element(node, "tracelist"); 
    
    XMLASSERT(xmllist);
    
    for(xmlNodePtr tracenode = xmllist->children; tracenode; tracenode = tracenode->next) {
      if(tracenode->type == XML_TEXT_NODE) continue; 
      XMLASSERT(tracenode->type == XML_ELEMENT_NODE && 
                !strcmp((const char*)tracenode->name, "trace"));
      
      const char *object = get_element_attribute(tracenode, "object"); 
      const char *name = get_element_attribute(tracenode, "name"); 
      XMLASSERT(object && name);
      xmlNodePtr shot = get_child_element(tracenode, "shot"); 
      XMLASSERT(shot);
      const char *id_str = get_element_attribute(shot, "id"); 
      XMLASSERT(id_str); 
      int id; 
      XMLASSERT(sscanf(id_str, "%d", &id) == 1); 
      while((int)tracelist.size() <= id) {
        tracelist.push_back(TracedEvent());           
      }
      tracelist[id].name = name;
      tracelist[id].obj = object;        
    }        
    if(verbose) printf("loaded %ld traces\n", tracelist.size());
  }      
  
  { // load links      
    xmlNodePtr linklist = get_child_element(node, "linklist");
    XMLASSERT(linklist);
    for(xmlNodePtr linknode = linklist->children; linknode; linknode = linknode->next) {
      if(linknode->type == XML_TEXT_NODE) continue; 

      Link link;
      
      XMLASSERT(linknode->type == XML_ELEMENT_NODE && 
                !strcmp((const char*)linknode->name, "link"));
      
      xmlNodePtr source = get_child_element(linknode, "source");
      xmlNodePtr destination = get_child_element(linknode, "destination");
      XMLASSERT(source && destination); 
      
      string sourcecol = get_element_attribute(source, "name"); 
      string destcol = get_element_attribute(destination, "name");       
      XMLASSERT(sourcecol.size() && destcol.size()); 
      

      link.source = sourcecol.substr(0, sourcecol.find(':'));
      link.sourceport = sourcecol.substr(sourcecol.find(':') + 1);
      link.dest = destcol.substr(0, destcol.find(':'));
      link.destport = destcol.substr(destcol.find(':') + 1);

      // printf("size %d %p\n", links.size(), &links[0]);
      links.push_back(link);
      // printf("osize %d %s\n", links.size(), links[0].source.c_str());
    }
    if(verbose)  printf("%ld links\n", links.size());
  }
  
  { // fill in nextEvents
    for(int i = 0; i < tracelist.size(); i++) {
      TracedEvent &ev = tracelist[i];
      Link *waitBeginLink = NULL; 
      bool recorded = false; 
      for(int l = 0; l < links.size(); l++) {
        Link & link = links[l];

        if(link.source == ev.obj && link.sourceport == ev.name) {
          int dest_ev = find_event(link.dest, link.destport); 
          ev.links.push_back(&link);
          if(dest_ev >= 0) {
            ev.nextEvents.push_back(dest_ev); 
            recorded = true;
          }
        }
        if(link.source == ev.obj && link.sourceport == "waitBegin")
          waitBeginLink = &link;

        if(link.dest == ev.obj && link.destport == ev.name &&
           link.destport != "waitEnd") {
          ev.links.push_back(&link);
          recorded = true;
        }
      }
      
      if(!recorded) { //  probably a user event
        if(waitBeginLink) {
          ev.links.push_back(waitBeginLink);
        } else {
          printf("could not find waitBegin/waitEnd where trace %d %s:%s fits\n", 
                 i, ev.obj.c_str(), ev.name.c_str());
        }
      }

      if(verbose) {
        printf("tev %d %s:%s, nextEvents = [", i, ev.obj.c_str(), ev.name.c_str()); 
        for(int j = 0; j < ev.nextEvents.size(); j++) 
          printf("%d ", ev.nextEvents[j]);
        printf("] links = [");
        for(int j = 0; j < ev.links.size(); j++) {
          Link & link = *ev.links[j];
          printf("%s:%s->%s:%s ", 
                 link.source.c_str(), link.sourceport.c_str(), 
                 link.dest.c_str(), link.destport.c_str());
        }
        printf("]\n");
      }
    }
  }
  
  { // load actual trace data
    xmlNodePtr xmlfiles = get_child_element(node, "filelist");
    XMLASSERT(xmlfiles);
    vector<Chunk> chunks;  

    for(xmlNodePtr filenode = xmlfiles->children; filenode; filenode = filenode->next) {
      
      if(filenode->type == XML_TEXT_NODE || 
         (filenode->type == XML_ELEMENT_NODE && 
          !strcmp((const char*)filenode->name, "pingresults"))) continue;
      
      XMLASSERT(filenode->type == XML_ELEMENT_NODE && 
                !strcmp((const char*)filenode->name, "tracefile"));
      
      const char *chunkfilename = get_element_attribute(filenode, "file"); 
      XMLASSERT(chunkfilename);
      
      {
        FILE *chunkfile = fopen(chunkfilename, "r"); 
        XMLASSERT(chunkfile);
        size_t file_size; 
        {
          struct stat sbuf;
          fstat(fileno(chunkfile), &sbuf);
          file_size = sbuf.st_size;
        }
        XMLASSERT(file_size % sizeof(Chunk) == 0);
        size_t nelt = file_size / sizeof(Chunk);
        size_t n0 = chunks.size();
        chunks.resize(n0 + nelt); 
        fread(&chunks[n0], nelt, sizeof(Chunk), chunkfile);
        fclose(chunkfile);
      }

    }
    if(verbose) printf("loaded %ld chunks\n", chunks.size());
    
    chunks_to_evlists(chunks, tracelist, t0, t1);

    {// also record all timestamps
      double t_prev = -10; 
      for(int i = 0; i < chunks.size(); i++) {
        double t = chunks[i].tv_sec + chunks[i].tv_usec * 1e-6;
        if(t > t_prev) 
          timestamps.push_back(t); 
        t_prev = t;        
      }
    }


  }
  xmlFreeDoc(doc);  

  return true;
 err:
  xmlFreeDoc(doc);
  return false; 
}

void Traces::print() {

  for(int ev_id = 0; ev_id < tracelist.size(); ev_id++) {
    const TracedEvent tev = tracelist[ev_id]; 
    
    printf("TracedEvent %s:%s, corresponding Links:\n",
           tev.obj.c_str(), tev.name.c_str());
    
    for(int j = 0; j < tev.links.size(); j++) {
      const Link & link = *tev.links[j];
      printf("  %s:%s->%s:%s, events=[ ", 
             link.source.c_str(), link.sourceport.c_str(), 
             link.dest.c_str(), link.destport.c_str());
      for(int k = 0; k < link.events.size(); k++) {        
        const Event &ev  = link.events[k];
        printf("    %.6fs tev %s:%s val %d\n", 
               ev.t - t0, 
               tracelist[ev.ev_id].obj.c_str(), 
               tracelist[ev.ev_id].name.c_str(), 
               ev.val);              
      }
      printf("  ]\n");
    }
  }
    

}

static int find_prev_event(const vector<Traces::Event> events, double t) {
  if(t < events[0].t) return -1;
  int i0 = 0; 
  int i1 = events.size();  
  while(i0 + 1 < i1) { // bissect
    int imed = (i0 + i1) / 2;
    if(events[imed].t <= t) i0 = imed;
    else i1 = imed; 
  }
  return i0;
}


int Traces::getModuleState(const std::string & name) {
  // find the beginWait of this name
  int i; 
  for(i = 0; i < links.size(); i++) {
    if(links[i].source == name && links[i].sourceport == "waitBegin") 
      break;
  } 

  if(i == links.size()) {
    // printf("could not find waitBegin of %s\n", name.c_str());
    // probably a filter or synchronizer
    return -1;
  }
  const vector<Event> &events = links[i].events; 

  if(events.size() == 0) {
    printf("nothing did happen with %s\n", name.c_str());
    return -1;
  }

  int nev = events.size();
  int prev = find_prev_event(events, t); 
/*
  printf("%s event = %d / %d\n", 
         name.c_str(),
         prev, events.size());
*/
  int j = prev;
  while(j > 0) {
    TracedEvent &tev = tracelist[events[j].ev_id]; 
    if(tev.name == "waitBegin") return 0; 
    if(tev.name == "waitEnd") return 1; 
    // else user event
    j--; 
  }

  j = prev + 1; 
  while(j < nev) {
    TracedEvent &tev = tracelist[events[j].ev_id]; 
    if(tev.name == "waitBegin") return 1; 
    if(tev.name == "waitEnd") return 0; 
    // else user event
    j++; 
  }
  
  // really can't tell....
  return -1;
}


double Traces::getLinkState(const std::string & source, const std::string & sourceport, 
                            const std::string & dest, const std::string & destport) {


  int i; 
  for(i = 0; i < links.size(); i++) {
    if(links[i].source == source && links[i].dest == dest && 
       (!sourceport.size() || links[i].sourceport == sourceport) &&
       (!destport.size() || links[i].destport == destport))
      break;
  }

  if(i == links.size()) {
    // printf("could not find link %s -> %s\n", source.c_str(), dest.c_str());
    return -1;
  }
  const vector<Event> &events = links[i].events; 
  
  if(events.size() == 0) {
    // printf("nothing did happen with %s -> %s\n", source.c_str(), dest.c_str());
    return -1;
  }
  
  int nev = events.size();
  int prev = find_prev_event(events, t); 
/*
  printf("%s->%s event = %d / %d\n", 
         source.c_str(), dest.c_str(),
         prev, events.size());
*/
  if(prev == -1) return -1;

  // find last send event

  i = prev; 

  while(i >= 0) {
    if(tracelist[events[i].ev_id].obj == source) 
      break;
    i--; 
  }

  if(i < 0) return -1;
  // then there is a message flowing ...
  // find arrival of message 

  int val = events[i].val;
  int j = prev + 1; 
  while(j < nev) {        
    if(tracelist[events[j].ev_id].obj == dest && 
       events[j].val == val) {
      double u = (t - events[i].t) / (events[j].t - events[i].t); 
      assert( 0 <= u && u <= 1);
      return u;
    }
    j++;
  }
  
  return -1;
}


double Traces::toEventTime(double t) {
  size_t nts = timestamps.size(); 
  if(nts < 2 || t < timestamps[0] || t >= timestamps[nts - 1]) return -1;
  
  size_t i0 = 0;
  size_t i1 = nts - 2;
  while(i0 + 1 < i1) {
    size_t imed = (i0 + i1) / 2; 
    if(timestamps[imed] <= t) i0 = imed; 
    else i1 = imed;
  }
  
  return i0 + (t - timestamps[i0]) / (timestamps[i0 + 1] - timestamps[i0]);
}

double Traces::fromEventTime(double et) {
  int i = int(floor(et)); 
  if(i < 0 || i + 1 >= timestamps.size()) return -1;
  double ft = et - i;
  return timestamps[i] * (1 - ft) + timestamps[i + 1] * ft;
}
