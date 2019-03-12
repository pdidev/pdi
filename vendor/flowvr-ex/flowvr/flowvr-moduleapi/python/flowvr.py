"""
A few extensions to the C++ classes. Hopefully simplifies the
interface.

There is no simple way of adding Python methods to a C++ class, other
than inheriting (but this does not work when a function returns a new
instance). Therefore, we add methods to some classes.

"""

import array
import cflowvr

def pytype_to_flowvr_type(ty):
  if ty == type(1): 
    return cflowvr.TypeInt.create()
  elif ty == type(''): 
    return cflowvr.TypeString.create()
  elif ty == type(1.0):
    return cflowvr.TypeFloat.create()
  assert False, "unhandled type %s" % ty
  
class OutputPort(cflowvr.OutputPort):
  
  def addStamp(self, name, ty, nb = -1):
    " adds a Stamp of Python type ty "
    if nb == -1:
      si = cflowvr.StampInfo(name, pytype_to_flowvr_type(ty))  
    else:
      si = cflowvr.StampInfo(name, cflowvr.TypeArray.create(nb, pytype_to_flowvr_type(ty)))
    si.this.own(False) # ownership transferred to stamplist
    self.stamps.add(si)

  def put(self, message):
    " simplified interface "
    self.getModule().put(self, message)

# make types local
vectorPort = cflowvr.vectorPort
initModule = cflowvr.initModule

def stamp_getStamp(self, name, index = -1):
  """ direct access to the stamps buffer. Not very optimal, but stamps
  are small..."""  
  si = self.stampList[name]
  assert si, "could not find stamp named " + name
  s = self.stamps.asString()
  ty = si.getType()
  typename = ty.typeName()
  ss = s[si.getOffset() :  si.getOffset() + si.getSize()]
  if typename in ("int", "float"):
    a = array.array(typename[0])
    a.fromstring(ss)
    return a[0]
  elif typename == "string":
    a = array.array('i')
    a.fromstring(ss)
    return s[a[0]:a[0]+a[1]]
  elif typename == "binary":
    return ss
  elif typename == "array":
    ta = ty.toTypeArray()
    subt = ta.t.typeName()
    # works only for int and float?    
    if subt not in ("int", "float"): return None
    a = array.array(subt[0])
    a.fromstring(ss)
    if index == -1: 
      return list(a)
    else:
      return a[index]
  else: assert False
    
def stamp_getStamps(self):
  " make a dictionary with all stamps "
  ret = {}
  for i in range(self.stampList.nbStamp()):
    name = self.stampList[i].getName()
    ret[name] = self.getStamp(name)
  return ret

cflowvr.Message.getStamp = stamp_getStamp
cflowvr.Message.getStamps = stamp_getStamps

class MessageWrite(cflowvr.MessageWrite):
  """ add a stampList field to make sure all messages are typed. """

  def __init__(self, stampList = None):
    cflowvr.MessageWrite.__init__(self)
    if stampList == None:
      # dummy StampList with only defaults
      stampList = cflowvr.StampList()
    self.stampList = stampList

  def getStamp(*args):
    return stamp_getStamp(*args)

  def setStamp(self, name, value):
    """ sets stamp value. For arrays, may pass (name, index) to set a
    single value """
    if type(name) == type(()):
      name, index = name
      si = self.stampList[name]      
      self.stamps.writeArray(si, index, value)
    else:
      si = self.stampList[name]
      if si.getType().typeName() == "array":
        for i in range(si.getType().toTypeArray().n):
          self.stamps.writeArray(si, i, value[i])          
      else: 
        self.stamps.write1(si, value)
    

class InputPort(cflowvr.InputPort):
  """ make sure that the stampList is set """
  
  def get(self):
    message = cflowvr.Message()
    self.getModule().get(self, message)    
    message.stampList = self.stamps
    return message
    
