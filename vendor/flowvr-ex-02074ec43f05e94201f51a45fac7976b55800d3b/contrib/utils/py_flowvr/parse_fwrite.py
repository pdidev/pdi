"""
Routines to load files dumped by fwrite. Example usage:

f = open("/tmp/dump.fwrite", "r")

stamps_descriptions = parse_header(f)

while True:
  messages = parse_message(f, stamps_descriptions)
  if not messages: break

  # loop over recorded ports
  for stamps, data in messages:
    # do someting useful...


FWrite dumps are binary files laid out like

file:
FlowVRdump                            text, 10 bytes
number_of_ports                       int, 4 bytes
stamps_description[number_of_ports]   see below
message[number_of_ports][*]            message (see below) repeated until end of file


stamps_description:
size                                  int, 4 bytes = size of following text
text                                  XML description of stamps (same as sent on port initialization)

message:
stamps_size                           int, 4 bytes = size of stamps
data_size                             int, 4 bytes = size of data
stamps                                size stamps_size = stamps serialized like in flowvr link
data                                  size data_size = arbitrary message content

"""

import array, struct
import xml.dom.minidom


def read1(f, fmt):
  ss = struct.calcsize(fmt)
  s = f.read(ss)  
  if len(s) != ss:
    return None
  return struct.unpack(fmt, s)[0]


def parse_header(f):
  
  assert f.read(10) == "FlowVRdump"
  nbp = read1(f, 'i')
  print "number of ports = ", nbp

  stamps_descriptions = []

  for i in range(nbp):
    ssize = read1(f, 'i')
    portdescription = f.read(ssize)
    print "port ", i, ":"

    elt = xml.dom.minidom.parseString(portdescription).documentElement

    stamps_descriptions.append(elt)

  # print elt.toprettyxml()

  return stamps_descriptions

  

def sint(s):
  a = array.array('i')
  a.fromstring(s)
  return a[0]


def parse_stamp_node(c2, s, i):
  if c2.tagName == 'string':
    ofs = sint(s[i:i+4])
    length = sint(s[i+4:i+8])
    # return "(string len %d, un %d)" % (length, unknown), i + 8
    return s[ofs:ofs+length], i + 8
  elif c2.tagName == 'int':
    return sint(s[i:i+4]), i + 4
  elif c2.tagName == 'array':
    nelt = int(c2.getAttribute("size"))
    values = []
    for j in range(nelt):
      v, i = parse_stamp_node(c2.firstChild, s, i)
      values.append(v)
    return values, i
  else:
    return "(unknown type %s)" % c2.tagName, i
 

def parse_stamps(desc, s):
  ret = {}
  assert sint(s[0:4]) == len(s)
  i = 4
  for c in desc.childNodes:
    assert c.tagName == 'stamp'
    name = c.getAttribute("name")
    size = int(c.getAttribute("size"))
    if size > 0:     
      value, i0 = parse_stamp_node(c.childNodes[0], s, i)
      # print "%s = %s" % (name, value)
      ret[name] = value
    i += size
  return ret



def parse_message(f, stamps_descriptions):
  nbp = len(stamps_descriptions)

  msg = []
  
  for stamps_description in stamps_descriptions:
    
    ssize = read1(f, 'i')
    if ssize == None:
      return None
    dsize = read1(f, 'i')

    stamps = parse_stamps(stamps_description, f.read(ssize))
    data = f.read(dsize)

    msg.append((stamps, data))

  return msg
