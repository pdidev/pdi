from select import select
import sys, time
import flowvr

# create the vectopr of ports
port = flowvr.OutputPort('out')
ports = flowvr.vectorPort()
ports.push_back(port)
# create the flowvr module
module = flowvr.initModule(ports);

# get default value on the command line
default_value = 10
if len(sys.argv) > 1:
    try:
        number = int(sys.argv[1])
        default_value = number
    except:
        print "Bad command line argument.", sys.exc_info()[0]
print "\nThe simulation will process the data received from the simulation every 'n' iteration, specified by this module (default value is '%d').\nWith a value of '0', every iteration is discarded.\n" % default_value

# first, send default value
if module.wait():
    message = flowvr.MessageWrite()
    message.data = module.allocString( str(default_value) )
    port.put( message )

while module.wait():
    # check stdin is ready
    readable, wlist, xlist = select( [sys.stdin], [], [], 0 )
    if ( not readable ):
        time.sleep( 0.1 )
        continue
    # get line from user
    line = raw_input()
    if ( line == "" or line == "\n"  ):
        continue
    # convert to a positive number
    number = -1
    try:
        number = int(line)
    except ValueError, e:
        print "Couldn't convert input to an integer."
        continue
    except:
        print "Unexpected exception.", sys.exc_info()[0]
        continue
    if ( number < 0 ):
        print "Can't use a negative integer."
        continue
    # put new value
    message = flowvr.MessageWrite()
    message.data = module.allocString( str(number) )
    port.put(message)
    print "set to ", number

module.close()
