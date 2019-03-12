

import flowvrapp
import filters


def select_primitives(prefixes):
  # collect primitives to be logged
  primitives = []
  for primitive in flowvrapp.app.primitives:
    for prefix in prefixes:
      if primitive.name.startswith(prefix): break
    else: continue
    primitives.append(primitive)     
  
  return primitives

def select_all_primitives():
  primitives = []
  for primitive in flowvrapp.app.primitives:
    primitives.append(primitive)
  return primitives

def add_traces(primitives, xml_prefix):

  events = []
  loggers = {}  

  for primitive in primitives:

    # prepare a logger if not ready
    host = primitive.host
    if host not in loggers:
      logger = filters.DefaultLogger('trace/logger_' + host, host = host)
      fwrite = filters.FWrite('trace/fwrite_' + host, host = host, filename = '/tmp/trace_log_' + host, raw = True)
      logger.getPort('log').link(fwrite.getPort('in'))        
      loggers[host] = logger
    else:
      logger = loggers[host]

    # find events to log
    if primitive.t == 'module': 
      events.append((primitive, "waitBegin", logger))
      events.append((primitive, "waitEnd", logger))         
    else:
      primitive.setParameter('trace', 1)

    # all ports get an event
    for port_name in primitive.ports:
      events.append((primitive, port_name, logger))

    # add user-defined events
    for trace_ev, ev_type in primitive.traces.iter():
      # there does not seem to be a way to give the type...
      print "add custom trace", trace_ev
      events.append((primitive, trace_ev, logger))

      
  # now make the 3  XML files... A bit verbose.
  
  prolog = open(xml_prefix + '.prolog.xml', 'w')
  epilog = open(xml_prefix + '.epilog.xml', 'w')

  print >> prolog, '<?xml version="1.0" ?>\n<commands>'
  print >> epilog, '<?xml version="1.0" ?>\n<commands>'
  
  cur_host = ''
  count = 1
  for primitive, ev_name, logger in events:
    host = primitive.host
    if host != cur_host:
      print >> prolog, "<dest>%s</dest>" % host
      print >> epilog, "<dest>%s</dest>" % host
      cur_host = host
    print >> prolog, '  <action id="%s"><traceStart name="%s" id="%d" logger="%s" /></action>' % (
      primitive.name, ev_name, count, logger.name)
    print >> epilog, '  <action id="%s"><traceStop name="%s" /></action>' % (
      primitive.name, ev_name)    
    count += 1

  print >> prolog, " <flush/> </commands>"    
  print >> epilog, "<flush/> </commands> "
  for host in loggers:
    print >> epilog, "<dest>%s</dest> <killall/>" % host
  
  del prolog
  del epilog

  # .gltrace.xml, warning: hairy monster!
  gltrace = open(xml_prefix + '.gltrace.xml', 'w'); 
  
  print >> gltrace, '<?xml version="1.0" ?>\n<gltrace>'
    
  print >> gltrace, '  <filelist>'
  print >> gltrace, '      <pingresults file="log-results"/>'
  for host in loggers:
    print >> gltrace, '      <tracefile file="/tmp/trace_log_%s" />' % host
  print >> gltrace, '  </filelist>'


  print >> gltrace, '  <hostlist>'
  for host, logger in loggers.items():
    print >>gltrace, '     <host id="%s" method="FASTEST"> <logger id="%s" /> </host> ' % (host, logger.name)
  print >> gltrace, '  </hostlist>'

  print >> gltrace, '  <objectlist>'
  line_colors = {'module': 'GREEN', 'filter': 'ORANGE', 'synchronizer': 'PINK'}
  
  objectlist_pattern = """
  <module id="%(name)s" host="%(host)s">
    <objdisplay active="YES">
      <objtext text="%(name)s:%(host)s" color="%(color1)s" />
      <objline color="%(color2)s" %(thickstring)s />
    </objdisplay>
  </module>"""

  for primitive in primitives:
    print >> gltrace, objectlist_pattern % {
      'color1': "WHITE", 'color2': line_colors[primitive.t],
      'name' : primitive.name, 'host' : primitive.host,
      'thickstring' : 'width="thin"' if primitive.t != 'module' else ''}

  print >> gltrace, '  </objectlist>'


  print >> gltrace, '  <tracelist>'
  count = 1
  for primitive, ev_name, logger in events:
    print >> gltrace, '      <trace object="%s" name="%s">' % (primitive.name, ev_name)
    print >> gltrace, '          <shot id="%d" />' % count
    print >> gltrace, '      </trace>'
    count += 1
  print >> gltrace, '  </tracelist>'
  
  print >> gltrace, '  <eventlist>'
  ev_colors = {"beginIt": "YELLOW", "endIt": "BLUE",
               "waitBegin": "RED", "waitEnd": "GREEN"}                
  eventlist_pattern = """
        <event name="%(name)s:%(ev_name)s">
            <trace object="%(name)s" name="%(ev_name)s" />
            <evtdisplay active="YES">
                <evtline color="%(color)s" />
            </evtdisplay>
        </event> """

  for primitive, ev_name, logger in events:
    color = ev_colors[ev_name] if ev_name in ev_colors else "YELLOW"
    print >> gltrace, eventlist_pattern % {
      'name' : primitive.name, 'ev_name' : ev_name,
      'color' : color}
    count += 1
  print >> gltrace, '  </eventlist>'


  print >> gltrace, '  <linklist>'

  linklist_wait_pattern = """
  <link name="%(name)s:wait">
            <source name="%(name)s:waitBegin" />
            <destination name="%(name)s:waitEnd" />
            <match>
                <DestFromSrc />
            </match>
            <lnkdisplay active="YES">
                <objline color="RED" />
            </lnkdisplay>
        </link>"""


  for primitive in primitives:
    if primitive.t != 'module': continue
    print >> gltrace, linklist_wait_pattern % {'name': primitive.name}

  linklist_pattern = """
        <link name="%(linkname)s">
            <source name="%(sourcename)s:%(sourceport)s" />
            <destination name="%(destname)s:%(destport)s" />
            <match>
                <SrcFromDest />
            </match>
            <lnkdisplay active="YES">
                <lnkline srccolor="%(dark)sYELLOW" destcolor="%(dark)sBLUE" />
            </lnkdisplay>
        </link>"""

  for primitive in primitives:
    for port in primitive.ports.values():
      if port.direction == 'out' or not port.source: continue
      other = port.source.owner
      print >> gltrace, linklist_pattern % {
        'linkname': primitive.name + '.' + port.name,   # not clear how link name is generated...
        'sourcename': other.name, 'sourceport': port.source.name,
        'destname': primitive.name, 'destport': port.name,
        'dark' : 'DARK_' if port.messagetype == 'stamps' else ''}
      

  print >> gltrace, '  </linklist>'

  print >> gltrace, '</gltrace>'

  del gltrace
