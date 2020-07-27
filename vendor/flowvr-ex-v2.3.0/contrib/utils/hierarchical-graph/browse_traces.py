import sys, struct, xml.dom.minidom, pdb



def usage():
  print """Displays trace logs
  
python browse_traces.py [options]

Then point a browser on http://localhost:8080

options:
[-port port_no]

"""

  sys.exit(1)

port=8080
args=sys.argv[1:]
xmlname = "ref_xmls/EricTracesPrimesXML/primes.gltrace.xml"

while args:
  a=args.pop(0)
  if a in ('-h','--help'): usage()
  elif a=='-port':        port=int(args.pop(0))
  else:
    xmlname = a


doc = xml.dom.minidom.parse(xmlname)

elt_tracelist = doc.documentElement.getElementsByTagName("tracelist")[0]

# pdb.set_trace()


id_map = {}

for elt in elt_tracelist.getElementsByTagName("trace"):
  obj = elt.getAttribute("object")
  name = elt.getAttribute("name")
  _id = int(elt.getElementsByTagName("shot")[0].getAttribute("id"))

  id_map[_id] = (obj, name)
  


class Port:
  def __init__(self, owner, name, direction, messagetype = 'full'):
    self.owner = owner             # a primitive
    self.name = name
    self.direction = direction
    self.messagetype = messagetype
    if direction == 'out':
      # can link a port to several destinations
      self.destinations = []
    else:
      # can link only from a single port 
      self.source = None    

  def link(self, portB):

    # a few checks...
    assert self.direction == 'out', "port %s is input" % self
    assert portB.direction == 'in', "port %s is output" % portB
    assert portB.source == None, "cannot link %s from %s: already linked from %s" % (portB, self, portB.source)
    assert portB.messagetype == 'stamps' or self.messagetype == 'full', "cannot link stamps %s to full %s" % (self, portB)

    # ok, works    
    self.destinations.append(portB)
    portB.source = self

  
class Primitive:
  def __init__(self, t, name):
    self.t = t
    self.name = name
    self.ports = {}
    self.events = []

# pdb.set_trace()
objects = doc.documentElement.getElementsByTagName("objectlist")[0].childNodes
primitives = {}
for obj in objects:
  if not isinstance(obj, xml.dom.minidom.Element): continue
  name = obj.getAttribute("id")
  primitives[name] = Primitive(obj.nodeName, name)

links = doc.documentElement.getElementsByTagName("linklist")[0].getElementsByTagName("link")

for link in links:
  sourceid, sourceport = link.getElementsByTagName("source")[0].getAttribute("name").split(':')
  if sourceport == "waitBegin": continue
  source = primitives[sourceid]
  if sourceport not in source.ports:
    source.ports[sourceport] = Port(source, sourceport, direction = 'out')
  sourceport = source.ports[sourceport]
                                    
  destid, destport = link.getElementsByTagName("destination")[0].getAttribute("name").split(':')
  dest = primitives[destid]
  if destport not in dest.ports:
    dest.ports[destport] = Port(dest, destport, direction = 'in')
  destport = dest.ports[destport]
  sourceport.link(destport)

tracefiles = doc.documentElement.getElementsByTagName("filelist")[0].getElementsByTagName("tracefile")

datafiles = [elt.getAttribute("file") for elt in tracefiles ]

fmt = '=iQQii'
chunksize = struct.calcsize(fmt)

all_events = []

for datafile in datafiles:
  
  f = open(datafile, 'r')

  while True:
    chunk = f.read(chunksize)
    if len(chunk) == 0: break
    else: assert len(chunk) == chunksize
    (size, tv_sec, tv_usec, ev_id, val) = struct.unpack(fmt, chunk)
    assert size == 28, "found size = %d" % size
    t = tv_sec + tv_usec * 1e-6

    # print (t, ev_id, val), f.tell(), chunksize
    
    all_events.append((t, ev_id, val))

all_events.sort()
t0 = all_events[0][0]

# dispatch events to primitives

for t, ev_id, val in all_events:
  obj, ev_name = id_map[ev_id]
  primitives[obj].events.append((t - t0, ev_name, val))
  

from BaseHTTPServer import *
from SocketServer import *


class ThreadingHTTPServer(ThreadingMixIn,HTTPServer):
  pass

def threedigits(x):
  # format a number between 1 and 1000 with 3 digits
  if x < 10: return '%.2f' % x
  if x < 100: return '%.1f' % x
  if x < 1000: return '%.0f' % x
  

def format_timediff(t):
  if t == 0: return '0 s'
  if t > 0: sign = '+'
  else: sign = '-'
  t = abs(t)
  if t > 1: return '%s%.2f s' % (sign, t)
  if t > 1e-3: return '%s%s ms' % (sign, threedigits(t * 1e3))
  return '%s%s us' % (sign, threedigits(t * 1e6))


class RequestHandler(BaseHTTPRequestHandler):

  def __init__(self, primitives, *args, **kwargs):
    self.primitives = primitives
    BaseHTTPRequestHandler.__init__(self, *args, **kwargs)

  def do_GET(self):
    pathcomp = self.path[self.path.find('/')+1:]
    
    self.send_response(200)
    self.send_header("Content-type", "text/html")
    self.end_headers()
    
    if pathcomp == '':
      print >> self.wfile, '<h1>All primitives</h1>'
      for primitive_name in sorted(self.primitives.keys()):        
        print >> self.wfile, '<a href="/%s">%s</a><br>' % (primitive_name, primitive_name)
    elif '?' not in pathcomp:
      prim_name = pathcomp

      print >> self.wfile, '<h1>Primitive %s</h1>' % prim_name
      if prim_name not in self.primitives:
        print >> self.wfile, 'unknown ????'
        return
      primitive = self.primitives[prim_name]
      print >> self.wfile, 'type %s' % primitive.t
      print >> self.wfile, '<h2>Ports</h2>'
      for port_name, port in sorted(primitive.ports.items()):
        print >> self.wfile, '%s (%s)<br>' % (port_name, port.direction)
      
      print >> self.wfile, '<h2>Events</h2>'
      for ev_no, (t, ev_name, val) in enumerate(primitive.events):
        print >> self.wfile, '<a href="/%s?ev=%d">+%.6f s, %s</a><br>' % (prim_name, ev_no, t, ev_name)
      
    else:
      prim_name, args = pathcomp.split('?')
      # if prim_name[0] != '/': prim_name = '/' + prim_name
      args = dict([(key, int(val)) for (key, val) in [arg.split('=') for arg in args.split('&')]])
      self.describe_events(prim_name, **args)


  def find_event(self, events, t):
    i0 = 0
    i1 = len(events)
    while i0 + 1 < i1:
      imed = (i0 + i1) / 2
      if events[imed][0] <= t: i0 = imed
      else:                    i1 = imed
    return i0
    

  def find_event_before(self, port, t):
    name = port.name
    events = port.owner.events
    i = self.find_event(events, t)
    while i >= 0:
      if events[i][1] == name: return i
      i -= 1  
    return -1

  def find_event_after(self, port, t):
    name = port.name
    events = port.owner.events
    i = self.find_event(events, t)
    while i < len(events):
      if events[i][1] == name: return i
      i += 1  
    return -1


  def describe_events(self, prim_name, ev = 0, depth = 0, neighbors = 0):
    print >> self.wfile, '<h1>Primitive %s</h1>' % prim_name
    primitive = self.primitives[prim_name]
    print >> self.wfile, '<a href="/%s">about primitive</a><br>' % prim_name
    
    print >> self.wfile, '<h2>Events </h2>'
    print repr(ev)
    t0 = primitive.events[ev][0]
    print >> self.wfile, 'time relative to %.6f s<br>' % t0

    print >> self.wfile, 'depth:'
    print >> self.wfile, '<a href="/%s?ev=%d&neighbors=%d&depth=%d">more</a>' % (
      prim_name, ev, neighbors, depth + 1)
    print >> self.wfile, '<a href="/%s?ev=%d&neighbors=%d&depth=%d">less</a><br>' % (
      prim_name, ev, neighbors, depth - 1)
    print >> self.wfile, '<br>'


    print >> self.wfile, 'neighbors:'
    print >> self.wfile, '<a href="/%s?ev=%d&neighbors=%d&depth=%d">more</a>' % (
        prim_name, ev, neighbors + 1, depth)
    print >> self.wfile, '<a href="/%s?ev=%d&neighbors=%d&depth=%d">less</a><br>' % (
        prim_name, ev, neighbors - 1, depth)
    print >> self.wfile, '<br>'

    
    for ev_no in range(ev - neighbors, ev + neighbors + 1):
      if not (0 <= ev_no < len(primitive.events)): continue

      (t, ev_name, val) = primitive.events[ev_no]      

      # handle input dependency
      if depth > 0 and ev_name in primitive.ports:
        port = primitive.ports[ev_name]
        if port.direction == 'in' and port.source != None:
          other_ev_no  = self.find_event_before(port.source, t)
          other_primitive = port.source.owner
          (other_t, other_ev_name, other_val) = other_primitive.events[other_ev_no]                
          prefix = '&nbsp;' * (depth - 1) * 4
          print >> self.wfile, '%s<a href="/%s?ev=%d&neighbors=%d&depth=%d">%s, %s:%s, val=%d</a><br>' % (
            prefix, other_primitive.name, other_ev_no, neighbors, depth, format_timediff(other_t - t0),
            other_primitive.name, other_ev_name, other_val)
                 
      if ev_no == ev: print >> self.wfile, "<b>"

      prefix = '&nbsp;' * depth * 4

      print >> self.wfile, '%s<a href="/%s?ev=%d&neighbors=%d&depth=%d">%s, %s, val=%d</a><br>' % (
        prefix, prim_name, ev_no, neighbors, depth, format_timediff(t - t0), ev_name, val)
                                                                
      if ev_no == ev: print >> self.wfile, "</b>"

      # handle output dependency
      if depth > 0 and ev_name in primitive.ports:
        port = primitive.ports[ev_name]
        if port.direction == 'out':
          for other_port in port.destinations: 
            other_ev_no  = self.find_event_after(other_port, t)
            other_primitive = other_port.owner
            (other_t, other_ev_name, other_val) = other_primitive.events[other_ev_no]                
            prefix = '&nbsp;' * (depth + 1) * 4
            print >> self.wfile, '%s<a href="/%s?ev=%d&neighbors=%d&depth=%d">%s, %s:%s, val=%d</a><br>' % (
              prefix, other_primitive.name, other_ev_no, neighbors, depth, format_timediff(other_t - t0),
              other_primitive.name, other_ev_name, other_val)


      


httpd = ThreadingHTTPServer(('', port), lambda *a: RequestHandler(primitives, *a))
print "listening on port", port

httpd.serve_forever()
