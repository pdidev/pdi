import sys

from flowvrapp import *
from filters import *




class FilterMerge2D(Filter):
   """Merges messages received on input port into one message sent on output port.
   """

   def __init__(self, name, host = ''):
     Filter.__init__(self, name, run = 'flowvr.plugins.Merge2D', host = host)
     self.addPort('in0', direction = 'in')
     self.addPort('in1', direction = 'in')
     self.addPort("out", direction = 'out')




class FluidMPI(Composite):
  """ several instances of a compute module, that compute primes
  together, and output them to the primesOut port"""

  def __init__(self, hosts, nx, ny):
    Composite.__init__(self)

    prefix = "fluid"
    # hosts: string with host names, separated by spaces
    fluidrun = FlowvrRunMPI("bin/fluid_insitu %d %d" % (nx, ny), hosts = hosts, prefix = prefix, mpistack = "openmpi") #use openmpi, mpich, mvapich or intel

    # hosts_list: convert hosts to a list
    hosts_list = hosts.split(",")

    # nb of instances
    ninstance = len(hosts_list)

    # collect ports
    all_positionsin = []
    all_densityout = []
    all_velocityout = []

    # collect ports
    host_positionsin = []
    host_densityout = []
    host_velocityout = []

    for i in range(ninstance):
      fluid = Module(prefix + "/" + str(i), run = fluidrun, host = hosts_list[i])
      fluid.addPort("positions", direction = 'in');
      fluid.addPort("density", direction = 'out');
      fluid.addPort("velocity", direction = 'out');

      all_positionsin.append(fluid.getPort("positions"))
      all_velocityout.append(fluid.getPort("velocity"))
      all_densityout.append(fluid.getPort("density"))

    self.ports["positionsin"] =  list(all_positionsin);
    self.ports["velocity"] =  list(all_velocityout);
    self.ports["density"] =  list(all_densityout);




class FilterDensity(Module):
  "One Module simplifying data density data"

  def __init__(self, name, host, factor, cores):
    Module.__init__(self, name, host = host, cmdline = "bin/filter-density "+str(factor), cores=cores)
    self.addPort("sync", direction = 'in');
    self.addPort("in", direction = 'in');
    self.addPort("out", direction = 'out');


class FilterAllDensity(Composite):
  """ several instances of a compute module, that compute primes
  together, and output them to the primesOut port"""

  def __init__(self, all_density, factor, cores=''):
    "Module DensityFilter simplify data"
    Composite.__init__(self)
    all_f_densityout = []
    all_syncin = []
    all_endit = []

    for i in range( len(all_density) ):
      filter = FilterDensity( "densfilter/"+str(i), all_density[i].owner.host, factor, cores=cores )
      all_density[i].link( filter.getPort( "in" ) )
      all_f_densityout.append( filter.getPort( "out" ) )
      all_syncin.append( filter.getPort( "sync" ) )
      all_endit.append( filter.getPort( "endIt" ) )

    self.ports["out"] =  list(all_f_densityout);
    self.ports["sync"] =  list(all_syncin);
    self.ports["endit"] =  list(all_endit);



class Isolines(Module):
  "Module Isolines compute isolines of density"

  def __init__(self, name, host, nx,ny, factor):
    Module.__init__(self, name, host = host, cmdline = "bin/isolines %d %d %d" %(nx,ny,factor))
    self.addPort("density", direction = 'in');
    self.addPort("isolines", direction = 'out');






class Button(Module):
  "..."

  def __init__(self, host, nstep):

    # Switch Linux/ MacOSX
    from sys import platform as _platform

    if _platform == "linux" or _platform == "linux2":
      # linux
      Module.__init__(self, name="Button", host = host, cmdline = "xterm -e python bin/fluid_insitu_freq.py %d"%nstep)
    elif _platform == "darwin":
      # OS X
     Module.__init__(self, name="Button", host = host, cmdline = "osascript src/macosx_terminal_fluid_insitu_freq.scpt")

    self.addPort("out", direction = 'out');
    self.run.options += '-x DISPLAY'




class SyncAllDensityFilters(Composite):
  """ several instances of a compute module, that compute primes
  together, and output them to the primesOut port"""

  def __init__(self, nstep, button_host, greedy_host, filter_sync, filter_endit):
    "Module DensityFilter simplify data"
    Composite.__init__(self)

    # for user to decide of the frequency
    button = Button( button_host, nstep )
    # to always give the last frequency to all modules
    greedy = Greedy( "greedy", host = greedy_host )
    button.getPort( "out" ).link( greedy.getPort('in') )
    filter_endit.link( greedy.getPort( "sync" ) )

    generate1toN("freq_sync", greedy.getPort('out'), filter_sync, max_arity = 2, node_class = BroadcastNode)

    self.ports["sync"] =  list(filter_sync);




class GLDens(Module):
  "Module Gldens display the fluid and capture mouse events"

  def __init__(self, name, host):
    Module.__init__(self, name, host = host, cmdline = "bin/gldens 0 0 1")
    self.addPort("positions", direction = 'out');
    self.addPort("density", direction = 'in');
    self.addPort("velocity", direction = 'in');
    self.addPort("isolines", direction = 'in');
    self.run.options += '-x DISPLAY'






# Main starts here ###########
host_visu =   "localhost" # host for visualization
host_simu = [ "localhost" ] #hosts for simulation

simu_per_host = 3 # number of simulation processes per host
helper_core='3' # bind in-situ process on this core.


nx, ny = 1*128*simu_per_host, 1*128*simu_per_host
factor = 1*4


hosts_fluid = ",".join( [ ",".join(simu_per_host*[h]) for h in host_simu ] )


print "Map ", simu_per_host, "fluid simulation processes on each host of ", host_simu
print "Bind in-situ processes on core ", helper_core," on each host  host of ", host_simu
print "Map 1 visualisation process on host ", host_visu

if len(sys.argv) >= 3:
  nx, ny = int(sys.argv[1]), int(sys.argv[2])

####### The simulation itself
fluidmpi = FluidMPI(hosts_fluid, nx, ny)

####### in-situ filter density
density = fluidmpi.getPort("density")
filter =  FilterAllDensity( density, factor, cores=helper_core )
filtered_density =  filter.getPort( "out" )

last_endIt = filter.getPort("endit")[ -1 ]
last_host = last_endIt.owner.host
SyncAllDensityFilters( nstep=50, button_host=host_visu, greedy_host=last_host, filter_sync=filter.getPort("sync"), filter_endit=last_endIt )

####### merge filtered density
treeOut = generateNto1(prefix="comNto1FDensityMerge", in_ports = filtered_density, arity = 2, node_class = FilterMerge2D)

####### visualize density
gldens = GLDens("gldens", host=host_visu)
#gldens.getPort("positions").link(fluidmpi.getPort("positionsin"))

greedy = Greedy( "greedy_visu", host = host_visu )
treeOut.link( greedy.getPort('in') )
gldens.getPort("endIt").link( greedy.getPort( "sync" ) )
treeOut = greedy.getPort('out')

treeOut.link( gldens.getPort("density") )

####### compute isolines on visualization node
isolines = Isolines( "isolines", host_visu, nx, ny, factor )
treeOut.link( isolines.getPort("density") )
isolines.getPort("isolines").link( gldens.getPort("isolines") )




app.generate_xml("fluid_insitu")
