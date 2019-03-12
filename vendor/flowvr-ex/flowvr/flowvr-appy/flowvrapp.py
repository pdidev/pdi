"""
Definition of basic FlowVRApp types and generation of the XML files.

"""



import pdb, types, sys

# should be FlowvrApp instance (see below). Records all defined primitives

app = None


class Port:

  def __init__(self, owner, name, direction='out', messagetype='full', blockstate='blocking'):
    self.owner = owner             # a primitive
    self.name = name               # aka id
    self.direction = direction     # 'in' or 'out'
    self.messagetype = messagetype # 'full' or 'stamps'
    self.blockstate = blockstate   # 'blocking' or 'nonblocking' (only relevant for input  ports)

    if direction == 'out':
      # can link a port to several destinations
      self.destinations = []
    else:
      # can link only from a single port
      self.source = None

  def link(self, portB):

    # special case: portB is actually multiple ports (useful for composites)
		# previously type(portB) == type(()), changed to type(portB) == type([]) in v6521
		# need to handle both list and tuple, not sure about this
    if type(portB) == type([]) or type(portB) == type(()):
      for b in portB: self.link(b)
      return

    # a few checks...
    assert self.direction == 'out', "port %s is input" % self
    assert portB.direction == 'in', "port %s is output" % portB
    assert portB.source == None, "cannot link %s from %s: already linked from %s" % (portB, self, portB.source)
    assert portB.messagetype == 'stamps' or self.messagetype == 'full', "cannot link stamps %s to full %s" % (self, portB)

    # ok, works
    self.destinations.append(portB)
    portB.source = self

  def __str__(self):
    return "Port(%s:%s)" % (self.owner.name, self.name)


class Run:
  " a means of running a module "
  pass


class FlowvrRunSSH(Run):

  def __init__(self, cmdline, name = '', host = '', cores = '', ownShmem = False, bindReg = True):
    self.cmdline = cmdline

    self.options = '-v '
    if cores != '':
      self.options = self.options + '-c \' %s \' ' % cores
    if bindReg and cores != '':
      self.options = self.options + '-e FLOWVR_MODULE_HWBIND \' %s \' ' % cores

    if ownShmem:
      self.options = self.options + '-e FLOWVR_MODULE_OWNSHMEM 1 '

    # if name and host are not set, they will be filled in from the module
    self.name = name
    self.host = host

  def add_environment_variable(self, variable, value = ""):
    if len(value) > 0:
      self.options = self.options + '-e ' + variable + ' ' + value + ' '
    else:
      self.options = self.options + '-x ' + variable + ' '

  def get_cmdline(self):
    return "flowvr-run-ssh %s ' %s ' %s" % (self.options, self.host, self.cmdline)


class FlowvrRunSSHMultiple(Run):
  """ the id of the modules must be built from the prefix, suffixed by
  /0, /1, etc. hosts and name will not be populated automatically from
  the corresponding module."""

  def __init__(self, cmdline, prefix, hosts, cores = '', ownShmem = False, bindReg = True):
    self.cmdline = cmdline
    self.name = prefix

    self.options = '-v '

    if cores != '':
      self.options = self.options + '-c \' %s \' ' % cores
    if bindReg and cores != '':
      self.options = self.options + '-e FLOWVR_MODULE_HWBIND \' %s \' ' % cores

    if ownShmem:
      self.options = self.options + '-e FLOWVR_MODULE_OWNSHMEM 1 '

    if app.current_path: self.name = app.current_path + self.name
    self.hosts = hosts

  def add_environment_variable(self, variable, value = ""):
    if len(value) > 0:
      self.options = self.options + '-e ' + variable + ' ' + value + ' '
    else:
      self.options = self.options + '-x ' + variable + ' '

  def get_cmdline(self):
    return "flowvr-run-ssh %s -p ' %s ' %s" % (self.options, self.hosts, self.cmdline)


def FlowvrRunMPI(cmdline, prefix, hosts, bindobject = "core", bindnumber = "1", ownShmem = False, mpistack = "openmpi", prefixcmd = "", mpirunargs = "", **kwargs):
  if mpistack == "openmpi":
    return FlowvrRunOpenMPI( cmdline = cmdline, prefix = prefix, hosts = hosts, bindobject = bindobject, bindnumber = bindnumber, ownShmem = ownShmem, prefixcmd = prefixcmd, mpirunargs = mpirunargs, **kwargs)
  elif mpistack == "mpich":
    return FlowvrRunMPICH( cmdline = cmdline, prefix = prefix, hosts = hosts, bindobject = bindobject, bindnumber = bindnumber, ownShmem = ownShmem, prefixcmd = prefixcmd, mpirunargs = mpirunargs, **kwargs )
  elif mpistack == "mvapich":
    return FlowvrRunMVAPICH( cmdline = cmdline, prefix = prefix, hosts = hosts, bindobject = bindobject, bindnumber = bindnumber, ownShmem = ownShmem, prefixcmd = prefixcmd, mpirunargs = mpirunargs, **kwargs )
  elif mpistack == "intel":
    print "WARNING : Launcher Intel is under construction, switching to openmpi"
    return FlowvrRunOpenMPI( cmdline = cmdline, prefix = prefix, hosts = hosts, bindobject = bindobject, bindnumber = bindnumber, ownShmem = ownShmem, prefixcmd = prefixcmd, mpirunargs = mpirunargs, **kwargs )
  else:
      raise NameError('Unknown MPI stack requested('+mpistack+'). Please use openmpi, mpich, mvapich or intel')

class FlowvrRunOpenMPI(Run):
  """ the id of the modules must be built from the prefix, suffixed by
  /0, /1, etc. hosts and name will not be populated automatically from
  the corresponding module.
  Optionnal parameters :
   rankfile : is used to pass a rankfile to fix the affinity of the MPI processes
  """

  def __init__(self, cmdline, prefix, hosts, bindobject = "core", bindnumber = "1", ownShmem = False, prefixcmd = "", mpirunargs = "", **kwargs):
    self.cmdline = cmdline
    self.name = prefix
    self.bindobject = bindobject
    self.bindnumber = bindnumber
    self.prefixcmd = prefixcmd
    self.mpirunargs = mpirunargs

    self.options = '-v -x FLOWVR_MODNAME -x FLOWVR_PARENT '

    if bindobject != ''  and  bindnumber != '':
      self.options = self.options + '-x FLOWVR_MODULE_HWBIND=%s ' % bindobject

    if ownShmem:
      self.options = self.options + '-x FLOWVR_MODULE_OWNSHMEM=1 '

    if app.current_path: self.name = app.current_path + self.name
    self.hosts = hosts
    self.np = len(hosts.split(","))

    self.rankfile = kwargs.pop("rankfile", "")
    if self.rankfile != "":
      print "Rankfile is provided, argument bindobject and bindnumber will be ignored in the command line."

  def add_environment_variable(self, variable, value = ""):
      self.options = self.options + ' -x ' + variable
      if len(value) > 0:
        self.options = self.options +'='+value
      self.options = self.options +' '

  #Note : For now we don't use the binding object here to avoid undetermined behavior
  #http://www.open-mpi.org/faq/?category=tuning#using-paffinity-v1.4
  def get_cmdline(self):
    if self.rankfile != "":
      return "%s mpirun %s -np %d --host %s --rankfile %s %s %s" % (self.prefixcmd, self.options, self.np, self.hosts, self.rankfile, self.mpirunargs, self.cmdline)
    else:
      #return "%s mpirun %s --host %s -cpus-per-proc %s %s %s" % (self.prefixcmd, self.options, self.hosts, self.bindnumber, self.mpirunargs, self.cmdline)
      return "%s mpirun %s -np %d --host %s --map-by node:pe=%s --bind-to hwthread %s %s" % (self.prefixcmd, self.options, self.np, self.hosts, self.bindnumber, self.mpirunargs, self.cmdline)

class FlowvrRunMVAPICH(Run):
  """ the id of the modules must be built from the prefix, suffixed by
  /0, /1, etc. hosts and name will not be populated automatically from
  the corresponding module."""

  def __init__(self, cmdline, prefix, hosts, bindobject = "core", bindnumber = "1", ownShmem = False, prefixcmd = "", mpirunargs = "", **kwargs):
    self.cmdline = cmdline
    self.name = prefix
    self.bindobject = bindobject
    self.bindnumber = bindnumber
    self.prefixcmd = prefixcmd
    self.mpirunargs = mpirunargs

    self.options = '-v -envlist FLOWVR_MODNAME,FLOWVR_PARENT '

    if bindobject != ''  and  bindnumber != '':
      self.options = self.options + '-genv FLOWVR_MODULE_HWBIND \' %s \' ' % bindobject

    if ownShmem:
      self.options = self.options + '-genv FLOWVR_MODULE_OWNSHMEM 1 '

    if app.current_path: self.name = app.current_path + self.name
    self.hosts = hosts

  #Note : MV2_ENABLE_AFFINITY disable the default behavior to bind each rank to a core
  def get_cmdline(self):
    return "%s mpirun -genv MV2_ENABLE_AFFINITY 0 %s --host %s --bind-to %s:%s %s %s" % (self.prefixcmd, self.options, self.hosts,self.bindobject, self.bindnumber, self.mpirunargs, self.cmdline)

class FlowvrRunMPICH(Run):
  """ the id of the modules must be built from the prefix, suffixed by
  /0, /1, etc. hosts and name will not be populated automatically from
  the corresponding module."""

  def __init__(self, cmdline, prefix, hosts, bindobject = 'cores', bindnumber = '1', ownShmem = False, prefixcmd = "", mpirunargs = "", **kwargs):
    self.cmdline = cmdline
    self.name = prefix
    self.bindobject = bindobject
    self.bindnumber = bindnumber
    self.prefixcmd = prefixcmd
    self.mpirunargs = mpirunargs

    self.options = '-v -envlist FLOWVR_MODNAME,FLOWVR_PARENT '
    if bindobject != ''  and  bindnumber != '':
      self.options = self.options + '--bind-to %s ' % (bindobject)
      self.options = self.options + '-genv FLOWVR_MODULE_HWBIND \' %s \' ' % bindobject

    if ownShmem:
      self.options = self.options + '-genv FLOWVR_MODULE_OWNSHMEM 1 '

    if app.current_path: self.name = app.current_path + self.name
    self.hosts = hosts

  def get_cmdline(self):
    return "%s mpirun %s --host %s %s %s" % (self.prefixcmd, self.options, self.hosts, self.mpirunargs, self.cmdline)


class Component:
  """ A Component can communicate via ports. """


  def __init__(self):
    self.ports = {}

  def getPort(self, name):
    assert name  in self.ports, "no port '%s' in '%s'" % (name, self)
    return self.ports[name]


class Primitive(Component):

  def __init__(self, name, run, host = ''):
    Component.__init__(self)

    self.name = name  # aka id
    self.host = host
    self.run = run    # how to run the primitive

    app.addPrimitive(self)

    self.traces = {}  # user-defined trace events

  def addPort(self, name, direction='out', messagetype='full',blockstate='blocking'):
    assert name not in self.ports, "port '%s' already exists on '%s'" % (name, self)
    port = self.ports[name] = Port(self, name, direction, messagetype,blockstate)
    return port

  def addTrace(self, name, ty = None):
    assert name not in self.traces
    assert ty in (None, types.IntType, types.StringType)
    self.traces[name] = ty

  def __str__(self):
    return self.name



class Module(Primitive):

  def __init__(self, name, cmdline = None, host = '', cores = '', run = None):

    if not run:
      assert cmdline, "need either run or cmdline"
      run = FlowvrRunSSH(cmdline, cores = cores)

    Primitive.__init__(self, name, run, host)

    if isinstance(run, FlowvrRunSSH):
      if not run.name: run.name = self.name
      if not run.host: run.host = self.host

    # default ports
    self.addPort('beginIt', direction='in', messagetype='stamps')
    self.addPort('endIt', direction='out', messagetype='stamps')


class FlowvrdPrimitive(Primitive):
  " primitive that runs on flowvrd. The run should be a string like flowvr.plugins.xxx "

  def __init__(self, name, run, host = ''):
    Primitive.__init__(self, name, run, host)

    self.parameters = { 'trace': 0 }

  def setParameter(self, key, value):
    self.parameters[key] = value


class Filter(FlowvrdPrimitive):
  pass

class Synchronizer(FlowvrdPrimitive):
  pass


class Composite(Component):
  """ Encapsulates a set of primitives.
  Only primitives can create new ports, the ports of a Composite must
  come from a Primitive."""
  pass


class FlowvrApp:
  """ records all primitives and generates the xml files for them """

  def __init__(self):
    self.primitives = []

    # automatically prefixed to name
    self.current_path = ''

    # current default host
    self.default_host = 'localhost'



  def addPrimitive(self, primitive):

    primitive.name = self.current_path + primitive.name

    if not primitive.host:
      primitive.host = self.default_host

    for p2 in self.primitives:
      assert p2.name != primitive.name, "a primitive with name %s already exists in the graph" % (
        primitive.name)

    primitive.t = ("module" if isinstance(primitive, Module) else
                   "filter" if isinstance(primitive, Filter) else
                   "synchronizer" if isinstance(primitive, Synchronizer) else
                   "invalid")

    assert primitive.t != "invalid", "primitive %s has an invalid type" % primitive.name

    self.primitives.append(primitive)


  def checkLocalhostExclusivity(self):
    localhostPresence = False
    everythingIsLocalhost = True

    localhostModuleName = ""

    for primitive in self.primitives:
      if primitive.host == "localhost":
        localhostPresence = True
        localhostModuleName = primitive.name
      else:
        everythingIsLocalhost = False

    if localhostPresence and not everythingIsLocalhost:
      print " -----------------------------------"
      print " !! WARNING !! Localhost discrepancy ( component", localhostModuleName, ")."
      print " -----------------------------------"
      print "   Your application appears to be distributed, yet at least one module is declared as running on 'localhost'."
      print "   Modules on distant machines may not be able to send messages to this particular module."
      print "   To fix this, please specify a public hostname for", localhostModuleName, "."
      print ""


  def generate_xml(self, classname, startup_wait = 0.0):
    """ generates the .cmd.xml, .net.xml and .run.xml files.
    startup_wait is a number of seconds to wait between each module is
    spawned (may avoid flovwrd crashes)
    """

    self.checkLocalhostExclusivity()

    prefix = classname.lower()

    self.generate_cmd_xml(prefix, startup_wait)
    self.generate_run_xml(prefix)

    #self.generate_net_xml_flat(prefix)

    self.generate_net_xml_hierarchy(prefix)


  def generate_cmd_xml(self, prefix, startup_wait = 0.0):
    """ Generate the .cmd.xml file  """

    cmdfile = open(prefix + ".cmd.xml", "w")

    # primitives

    print >>cmdfile, """<?xml version="1.0" encoding="ISO-8859-1" standalone="yes" ?>\n<commands>"""
    #i = 0
    #delta = 20
    for primitive in self.primitives:
      if primitive.t == 'module':
        print >> cmdfile,"""  <dest>%s</dest>\n  <addobject id="%s" class="flowvr.plugins.Regulator" />""" % (
          primitive.host, primitive.name)
        if startup_wait > 0:
          print >> cmdfile, """  <wait duration=%g /> """ % startup_wait
      else:
        print >> cmdfile,"""  <dest>%s</dest>\n  <addobject id="%s" class="%s" >""" % (
          primitive.host, primitive.name, primitive.run)
        print >> cmdfile,"""     <parameters>"""
        for k, v in primitive.parameters.iteritems():
          print >> cmdfile,"""        <%s>%s</%s>"""%(k,v,k)
        print >> cmdfile,"""     </parameters>\n  </addobject>"""
      #i = i+1
      #if i % delta == 0:
      #  print >> cmdfile,"""  <wait duration = 1 />"""
    print >> cmdfile,"  <flush/>"

    # connections

    comno=0


    # first make a /NET's for all links between different hosts
    slash_nets = {}

    for primitive in self.primitives:
      for port in primitive.ports.values():
        if port.direction != 'in' or not port.source : continue
        portA = port.source
        portB = port
        if portA.owner.host == portB.owner.host: continue

        messagetype = portB.messagetype

        key = (portA, portB.owner.host)

        if key not in slash_nets:
          slash_nets[key] = messagetype
        elif messagetype == "full" and slash_nets[key] == "stamps":
          # upgrade connection
          slash_nets[key] = "full"

    for (portA, hostB), messagetype in sorted(slash_nets.items()):

      print >> cmdfile,"""  <dest>%s</dest>
  <addroute id="com%d">
      <source id="%s" port="%s" messagetype="%s" />
      <action id="/NET" messagetype="%s">
          <dest>%s</dest>
      </action>
  </addroute>""" % (portA.owner.host, comno,
                    portA.owner.name, portA.name, portA.messagetype,
                    messagetype, hostB)
      comno+=1
      #i = i+1
      #if i % delta == 0:
      #  print >> cmdfile,"""  <wait duration = 1 />"""

    # add routes

    for primitive in self.primitives:
      for port in primitive.ports.values():
        if port.direction != 'in' or not port.source : continue
        portA = port.source
        portB = port

        portA_messagetype = portA.messagetype

        # special case for /NET's: the messagetype is that of the /NET's
        if portA.owner.host != portB.owner.host:
          portA_messagetype = slash_nets[(portA, portB.owner.host)]

        print >> cmdfile,"""  <dest>%s</dest>
  <addroute id="com%d">
      <source id="%s" port="%s" messagetype="%s" />
      <action id="%s" messagetype="%s">
          <port>%s</port>
      </action>
  </addroute>""" % (portB.owner.host, comno,
                    portA.owner.name, portA.name, portA_messagetype,
                    portB.owner.name, portB.messagetype,
                    portB.name)
        comno+=1
	#i = i+1
	#if i % delta == 0:
	#  print >> cmdfile,"""  <wait duration = 1 />"""

    print >> cmdfile,"  <flush/>"

    # start

    for primitive in self.primitives:
      print >> cmdfile,"""  <dest>%s</dest>\n  <action id="%s">\n        <start />\n  </action>""" % (
        primitive.host, primitive.name)
      #i = i+1
      #if i % delta == 0:
      #  print >> cmdfile,"""  <wait duration = 1 />"""

    print >> cmdfile,"  <flush/>"

    print >> cmdfile, "</commands>"


  def generate_run_xml(self, prefix):
    """ Generate the .run.xml file  """
    seen_runs = set()

    for primitive in self.primitives:
      if primitive.t == 'module':
        seen_runs.add(primitive.run)

    runs = list(seen_runs)
    runs.sort(lambda x, y: cmp(x.name, y.name))

    runfile = open(prefix + ".run.xml", "w")

    print >> runfile, "<commmands>"

    for run in runs:
      print >> runfile, """  <run metamoduleid="%s">%s</run>""" % (run.name, run.get_cmdline())

    print >> runfile, "</commmands>"

  def generate_net_xml_flat(self, prefix):
    """ Generate the .net.xml file  """

    netfile = open(prefix + ".net.xml", "w")

    # primitives

    print >>netfile, """<network>"""

    for primitive in self.primitives:

      t = primitive.t

      print >> netfile,"""  <%s id="%s" host="%s">""" % (t, primitive.name, primitive.host)

      print >> netfile,"""     <input>"""
      for port in primitive.ports.values():
        if port.direction == 'in':
          print >> netfile,"""       <port id="%s" blockstate="%s" />""" % (port.name, port.blockstate)
      print >> netfile,"""     </input>"""

      print >> netfile,"""     <output>"""
      for port in primitive.ports.values():
        if port.direction == 'out':
          print >> netfile,"""       <port id="%s" />""" % port.name
      print >> netfile,"""     </output>"""


      if t in ("filter", "synchronizer"):

        print >> netfile,"""     <parameters>"""
        for k, v in primitive.parameters.iteritems():
          print >> netfile,"""        <%s>%s</%s>""" % (k,v,k)
        print >> netfile,"""     </parameters>"""

        print >> netfile,"""     <%sclass>%s</%sclass>""" % (t, primitive.run, t)

      print >>  netfile,"""  </%s>""" % t

    # connections

    for primitive in self.primitives:
      for port in primitive.ports.values():
        if port.direction != 'in' or not port.source : continue
        portA = port.source
        portB = port

        messagetype = portB.messagetype

        tag= "stamps" if messagetype=="stamps" else ""

        print >> netfile,"""  <connection%s id="dummy">
        <source%s>
            <%sid id="%s" port="%s" />
        </source%s>
        <destination%s>
            <%sid id="%s" port="%s" />
        </destination%s>
  </connection%s>
        """ % (
        tag, tag,
        portA.owner.t, portA.owner.name, portA.name,
        tag, tag,
        portB.owner.t, portB.owner.name, portB.name,
        tag, tag)


    print >> netfile, "</network>"


  def generate_net_xml_hierarchy(self, prefix):
    """ Generate the proto-.net.xml file  """

    """
    Classes to describe the hierarchy
    Composite pattern
    """
    class HierarchyElement:
      def __init__(self, name):
        self.name = name

    class Metamodule(HierarchyElement):
      def __init__(self, name):
        HierarchyElement.__init__(self, name)
        self.children = {}

    class ModulePlaceholder(HierarchyElement):
      def __init__(self, name, module):
        HierarchyElement.__init__(self, name)
        self.module = module


    def generateMetamoduleBlock(netfile, m, level):
      if (level != 0):
        netfile.write("    " * level)
        print >> netfile,"""  <composite id="%s">""" % (m.name)

      for key in m.children:
        netfile.write("    " * level)
        print >> netfile,""""""

        if (isinstance(m.children[key], Metamodule)):
          generateMetamoduleBlock(netfile, m.children[key], (level+1))
        else:
          primitive = m.children[key].module
          t = primitive.t

          netfile.write("    " * (level+1))
          print >> netfile,"""  <%s id="%s" host="%s">""" % (t, primitive.name, primitive.host)

          netfile.write("    " * (level+1))
          print >> netfile,"""     <input>"""

          for port in primitive.ports.values():
            if port.direction == 'in':
              netfile.write("    " * (level+1))
              print >> netfile,"""       <port id="%s" blockstate="%s" />""" % (port.name, port.blockstate)

          netfile.write("    " * (level+1))
          print >> netfile,"""     </input>"""

          netfile.write("    " * (level+1))
          print >> netfile,"""     <output>"""

          for port in primitive.ports.values():
            if port.direction == 'out':
              netfile.write("    " * (level+1))
              print >> netfile,"""       <port id="%s" />""" % port.name

          netfile.write("    " * (level+1))
          print >> netfile,"""     </output>"""


          if t in ("filter", "synchronizer"):

            netfile.write("    " * (level+1))
            print >> netfile,"""     <parameters>"""

            for k, v in primitive.parameters.iteritems():
              netfile.write("    " * (level+1))
              print >> netfile,"""        <%s>%s</%s>""" % (k,v,k)

            netfile.write("    " * (level+1))
            print >> netfile,"""     </parameters>"""

            netfile.write("    " * (level+1))
            print >> netfile,"""     <%sclass>%s</%sclass>""" % (t, primitive.run, t)

          netfile.write("    " * (level+1))
          print >>  netfile,"""  </%s>""" % t


      if (level != 0):
        netfile.write("    " * level)
        print >> netfile,"""  </composite>"""


    """
    Generating a module tree from the list of primitives
    A new level is introduced every time a "/" is encountered
    """
    root = Metamodule(prefix)

    for primitive in self.primitives:
      name = primitive.name.split('/')

      # Descending from root to leaf, finding a place for our module
      currentLevel = root

      i = 1
      # Iterate through the name sections
      for nameSection in name:
        nextLevel = None

        # Check if element exists already
        if currentLevel.children.has_key(nameSection):

          # {Element exists}
          if len(name) == i:
            # Trying to insert a module but element already exists
            print "  /!\\"
            print "CONFLICTING NAME : ", name
            print "  /!\\"
            break
          else:
            # Going deeper in the hierarchy
            #print "METAMODULE, descending"
            nextLevel = currentLevel.children[nameSection]

        else:

          # {It doesn't exist, create it, append it}
          if len(name) == i:
            #print "MODULE LEVEL, appending"
            currentLevel.children[nameSection] = ModulePlaceholder(nameSection, primitive)
          else:
            #print "METAMODULE, creating and descending"
            nextLevel = Metamodule(nameSection)
            currentLevel.children[nameSection] = nextLevel

        # Browsing the hierarchical level below
        currentLevel = nextLevel

        i = i+1



    """
    Filling a corresponding .net.xml file
    """

    netfile = open(prefix + ".net.xml", "w")
    print >>netfile, """<network>"""


    """
    Modules
    """

    generateMetamoduleBlock(netfile, root, 0)

    """
    Connections
    """

    for primitive in self.primitives:
      for port in primitive.ports.values():
        if port.direction != 'in' or not port.source : continue
        portA = port.source
        portB = port

        messagetype = portB.messagetype

        tag= "stamps" if messagetype=="stamps" else ""

        print >> netfile,"""  <connection%s id="dummy">
        <source%s>
            <%sid id="%s" port="%s" />
        </source%s>
        <destination%s>
            <%sid id="%s" port="%s" />
        </destination%s>
  </connection%s>
        """ % (
        tag, tag,
        portA.owner.t, portA.owner.name, portA.name,
        tag, tag,
        portB.owner.t, portB.owner.name, portB.name,
        tag, tag)


    print >> netfile, "</network>"




app = FlowvrApp()

