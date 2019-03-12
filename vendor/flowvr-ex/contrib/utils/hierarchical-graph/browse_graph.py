import os,cPickle,sys,random,pdb,time
import xml.dom.minidom


from BaseHTTPServer import *
from SocketServer import *
import time,types

tag_with_children = ("composite", "metamodule", "Shell", "comtree", "patternparallelfromports", "Communication", "patternparallelfromhosts")

tag_primitive = ("module", "filter", "synchronizer", "connection", "connectionstamps")

def print_tree(root,outf):
  name=root.getAttribute('id')
  # "&nbsp;"*(4*name.count("/")),
  print >> outf,"<a href=\"/%s\"><small><small>%s</small></small>%s</a><br>"%(
    name,
    name[:name.rfind('/')+1],
    name[name.rfind('/')+1:])
  for e in root.childNodes:
    if e.__class__==xml.dom.minidom.Element and e.tagName in (tag_with_children + tag_primitive):
      print_tree(e,outf)


def all_components_adl(e):
  # if e.__class__==xml.dom.minidom.Element and e.tagName not in (tag_with_children + tag_primitive):
  #  print "unknown tag", e.tagName
  if e.__class__==xml.dom.minidom.Element and e.tagName in (tag_with_children + tag_primitive):
    yield e
    for e1 in e.childNodes:
      for c in all_components_adl(e1):
        yield c

def all_links_adl(e):
  if e.__class__==xml.dom.minidom.Element:
    if e.tagName=='link':
      yield e
    if e.tagName in tag_with_children:
      for e1 in e.childNodes:
        for c in all_links_adl(e1):
          yield c


def get_child(e,tagName=None):
  # get child node with this tag name
  for ee in e.childNodes:
    if (ee.__class__==xml.dom.minidom.Element and (not tagName or tagName==ee.tagName)):
      return ee
  return None

class BadRoute(Exception):
  pass
  
def parse_addroute(e):
  assert e.__class__==xml.dom.minidom.Element and e.tagName=='addroute'
  routeid=e.getAttribute('id')
  for e1 in  e.childNodes:
    if e1.__class__!=xml.dom.minidom.Element: continue
    if e1.tagName=='source':
      compA=e1.getAttribute('id')
      portA=e1.getAttribute('port')
      messagetype=e1.getAttribute('messagetype')
    elif e1.tagName=='action':      
      compB=e1.getAttribute('id')
      if compB=='/NET': raise BadRoute("NET actions not supported")
      e2=get_child(e1,'port')
      if not hasattr(e2,'firstChild'): pdb.set_trace()
      portB=e2.firstChild.data
  return (compA,portA,compB,portB,messagetype)
      
  
def parse_connection(e):
  assert e.__class__==xml.dom.minidom.Element
  if e.tagName=='connection':
    messagetype='full'
    suf=''
  elif e.tagName=='connectionstamps':
    messagetype='stamps'
    suf='stamps'
  routeid=e.getAttribute('id')
  for e1 in  e.childNodes:
    if e1.__class__!=xml.dom.minidom.Element: continue    
    if e1.tagName=='source'+suf:
      src=get_child(e1)
      compA=src.getAttribute('id')      
      portA=src.getAttribute('port')
    elif e1.tagName=='destination'+suf:
      dst=get_child(e1)
      compB=dst.getAttribute('id')      
      portB=dst.getAttribute('port')
  return (compA,portA,compB,portB,messagetype)
  
  

class Index:

  def add_link(self,e):
    try:
      if e.tagName=='link': 
        # adl
        compA=e.getAttribute('source')
        compB=e.getAttribute('dest')
        portB=e.getAttribute('inport')
        portA=e.getAttribute('outport')      
        messagetype=None
        info_from='adl'      
      elif e.tagName=='addroute': 
        # cmd      
        (compA,portA,compB,portB,messagetype)=parse_addroute(e)
        info_from='cmd'
      elif e.tagName in ('connection','connectionstamps'):
        # net
        (compA,portA,compB,portB,messagetype)=parse_connection(e)
        info_from='net'
      else:
        assert False


      self.add_link_1(compA,(compA,portA,compB,portB,messagetype),info_from)
      self.add_link_1(compB,(compA,portA,compB,portB,messagetype),info_from)
    except BadRoute:
      pass

  def add_link_1(self,to,(compA,portA,compB,portB,messagetype),info_from):
    if to not in self.link_map: self.link_map[to]=[]
    # check duplicates
    for linkno, (compAp,portAp,compBp,portBp,messagetypep,info_fromp) in enumerate(self.link_map[to]):
      if (compAp,portAp,compBp,portBp)==(compA,portA,compB,portB):        
        self.link_map[to][linkno]=(compA,portA,compB,portB,
                                   max(messagetypep,messagetype),
                                   info_fromp+" "+info_from)
        break
    else:
      self.link_map[to].append((compA,portA,compB,portB,messagetype,info_from))
    
      
      


  def __init__(self,dom1,runfile,cmdfile,netxmlfile):
    self.root=dom1.documentElement
    self.name_map={}
    self.link_map={}
    
    for ce in all_components_adl(self.root): 
      self.name_map[ce.getAttribute("id")]=ce
  
    print "name_map size ",len(self.name_map)

    for ce in all_links_adl(self.root):
      self.add_link(ce)
       
    self.run_map={}

    if runfile:
      if not os.access(runfile,os.R_OK):
        print "WARN, cannot read runfile",runfile
      else: 
        domrun=xml.dom.minidom.parse(runfile)
        # pdb.set_trace()

        e=domrun.documentElement
        assert e.nodeName=='commands'
        for e1 in e.childNodes:        
          if e1.__class__==xml.dom.minidom.Element and e1.tagName=="run":
            self.run_map[e1.getAttribute('metamoduleid')]=('run',e1.firstChild.data)

    if cmdfile:
      if not os.access(cmdfile,os.R_OK):
        print "WARN, cannot read cmdfile",cmdfile
      else: 
        dom=xml.dom.minidom.parse(cmdfile)
        
        e=dom.documentElement
        # pdb.set_trace()
        assert e.nodeName=='commands'      
        for e1 in e.childNodes:        
          if e1.__class__!=xml.dom.minidom.Element: continue
          if e1.tagName=='dest':
            dest=e1.firstChild.data
          if e1.tagName=="addobject":
            compid=e1.getAttribute('id')
            if compid in self.run_map:
              print "weird, for %s, run says %s and cmd says %s"%(
                compid, self.run_map[compid][1], e1.getAttribute('class'))
              continue
            self.run_map[compid]=('cmd',dest,e1.getAttribute('class'))
          if e1.tagName=="addroute":
            self.add_link(e1)
            
    if netxmlfile:
      if not os.access(cmdfile,os.R_OK):
        print "WARN, cannot read cmdfile",netxmlfile
      else: 
        dom=xml.dom.minidom.parse(netxmlfile)
        
        e=dom.documentElement
        # pdb.set_trace()
        for e1 in e.childNodes:
          if e1.__class__==xml.dom.minidom.Element and e1.tagName in ("connection","connectionstamps"):
            self.add_link(e1)
      
      

class SearchRequestHandler(BaseHTTPRequestHandler):

  def __init__(self,index,*args,**kwargs):
    self.index=index
    BaseHTTPRequestHandler.__init__(self,*args,**kwargs)
   

  def do_GET(self):
    pathcomp=self.path[self.path.find('/')+1:]

    self.send_response(200)
    self.send_header("Content-type", "text/html")
    self.end_headers()
    name_map=self.index.name_map

    if pathcomp=='':
      pathcomp=self.index.root.getAttribute('id')
    
    if pathcomp=='index.html':
      print  >> self.wfile,"<h1>All components</h1>"
      print_tree(self.index.root,self.wfile)
    
      # for name in sorted(self.name_map):
      #  print >> self.wfile,"&nbsp;"*(4*name.count("/")),"<a href=\"/%s\">%s</a><br>"%(name,name)
    
    elif pathcomp in name_map:
      e=name_map[pathcomp]
      print >>self.wfile,"<h1>Component %s</h1>"%str(e.getAttribute('id'))

      print >>self.wfile,"<a href=\"/index.html\">Component index</a><br>"
      
      
      print >>self.wfile,"Component type: %s (%s)<br>" % (e.tagName, "primitive" if e.tagName in tag_primitive else "composite")
      
      if e!=self.index.root:
        pname=pathcomp[:pathcomp.rfind('/')]
        print >>self.wfile,"parent <a href=\"/%s\">%s</a>"%(
          pname,pname)
      
      print >>self.wfile,"<h2>Parameters</h2>"
      for e1 in e.childNodes:
        if e1.__class__==xml.dom.minidom.Element and e1.tagName=="parameters":
          for e2 in e1.childNodes:
            if e2.__class__==xml.dom.minidom.Element:
              
              print >>self.wfile,"<b>%s</b> = %s (set in %s)<br>"%(
                e2.tagName, e2.hasChildNodes() and e2.firstChild.data or "", e2.getAttribute("from"))

      print >>self.wfile,"<h2>Declared ports</h2>"
      for e1 in e.childNodes:
        if e1.__class__==xml.dom.minidom.Element and e1.tagName=="port":
          print >>self.wfile, "<b>%s</b> type=%s msgtype=%s block=%s <br>" % (
            e1.getAttribute("id"),
            e1.getAttribute("type"),
            e1.getAttribute("msgtype"),           
            e1.getAttribute("blockstate"))
        


      if pathcomp in self.index.run_map:
        what_to_run=self.index.run_map[pathcomp]
        if what_to_run[0]=='run': 
          print >>self.wfile,"<h2>Run</h2>"          
          print >>self.wfile,what_to_run[1]
        elif what_to_run[0]=='cmd':
          print >>self.wfile,"<h2>Filter/synchronizer</h2>"          
          print >>self.wfile,what_to_run[2],"(on %s)"%what_to_run[1]
          
                    
      if e.tagName not in tag_primitive:
        print >>self.wfile,"<h2>Children</h2>"
        for e1 in e.childNodes:
          if e1.__class__==xml.dom.minidom.Element and e1.tagName in (tag_primitive + tag_with_children):
            cname=str(e1.getAttribute('id'))
            print >> self.wfile,"<a href=\"/%s\">%s</a><br>"%(cname,cname)

      link_map=self.index.link_map

      if pathcomp in link_map:

        ins=''
        outs=''

        for (compA,portA,compB,portB,messagetype,info_from) in link_map[pathcomp]:
            
          if compA==pathcomp:
            outs+="%s &rarr; <a href=\"/%s\">%s</a>:%s [%s from %s]<br>\n"%(
              portA, compB, compB, portB,messagetype or "", info_from)
          elif compB==pathcomp:
            ins+="<a href=\"/%s\">%s</a>:%s &rarr; %s [%s from %s]<br>\n"%(
              compA,compA,portA,portB,messagetype or "", info_from)
        if ins:
          print >>self.wfile,"<h2>Incoming links</h2>"
          print >>self.wfile,ins
        if outs:
          print >>self.wfile,"<h2>Outgoing links</h2>"
          print >>self.wfile,outs         

    else:             
      print >>self.wfile,"Unknown path",pathcomp

      


class ThreadingHTTPServer(ThreadingMixIn,HTTPServer):
  pass


def usage():
  print """Displays nodes of the FlowVR graph as web pages. Usage:

python browse_graph.py [options]

Then point a browser on http://localhost:8080 

options:


[-prefix path_to_flowvr_files]  sets adl, run, cmd to prefix.adl, prefix.run, prefix.cmd
[-adl my_application.adl.xml]   adl file generated by flowvr (mandatory)
[-run my_application.run.xml]   run file generated by flowvr
[-cmd my_application.cmd.xml]   cmd file generated by flowvr

"""

  sys.exit(1)

port=8080
args=sys.argv[1:]
adlfile='/users/huron/mdouze/scratch/ref/install/vvgate.adl.out.xml'
runfile=None
cmdfile=None
netfile=None
while args:
  a=args.pop(0)
  if a in ('-h','--help'): usage()
  elif a=='-port':        port=int(args.pop(0))
  elif a=='-prefix':
    prefix=args.pop(0)
    adlfile=prefix+".adl.out.xml"
    runfile=prefix+".run.xml"
    cmdfile=prefix+".cmd.xml"
    netfile=prefix+".net.xml"
  elif a=='-adl':         adlfile=args.pop(0)
  elif a=='-run':         runfile=args.pop(0)
  elif a=='-cmd':         cmdfile=args.pop(0)
  elif a=='-net':         netfile=args.pop(0)
  else:
    print >> sys.stderr,"unknown option",a
    usage()

adlstring=open(adlfile,"r").read()
adlstring=adlstring.replace("<4","<_4")
adlstring=adlstring.replace("</4","</_4")

dom1=xml.dom.minidom.parseString(adlstring)


index=Index(dom1,runfile,cmdfile,netfile)


httpd = ThreadingHTTPServer(('', port), lambda *a: SearchRequestHandler(index, *a))
httpd.serve_forever()
