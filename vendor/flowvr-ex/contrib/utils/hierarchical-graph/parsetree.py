
import sys,math
import xml.dom.minidom

#####################################
# Do not touch this


def gc(self,name):
  for ee in self.childNodes:
    if (ee.__class__==xml.dom.minidom.Element and
        unicode(name)==ee.tagName):
      return ee
  return None

def gcn(self,name):
  cn=[]
  for ee in self.childNodes:
    if (ee.__class__==xml.dom.minidom.Element and
        unicode(name)==ee.tagName):
      cn.append(ee)
  return cn

def gt(self):
  return str(self.childNodes[0].data)


xml.dom.minidom.Element.getChild=gc
xml.dom.minidom.Element.getChildren=gcn
xml.dom.minidom.Element.getText=gt

#####################################
# Touch this




class ComponentTree:
  """ Node of a tree, with input and output ports """
  
  def __init__(self):
    self.host=self.ntype=''
    self.children={}  # maps name of child -> ComponentTree of child 
    self.inputs=[]    # list of input port names
    self.outputs={}   # maps output port name -> triplet (destination,destinationport,link)

  def walk(self,path):
    """ find child with name path (a tuple with components that may contain '/')"""
    if not path:
      return self
    else:      
      name=path[0]
      i=1
      while name not in self.children:
        name+='/'+path[i]
        i+=1
      return self.children[name].walk(path[i:])

  def walk_with_intermediate(self,path):
    """ same as walk, but return intermediate components """
    if not path:
      return [self]
    else:      
      name=path[0]
      i=1
      while name not in self.children:
        name+='/'+path[i]
        i+=1
      return [self]+self.children[name].walk_with_intermediate(path[i:])
    

  def make(self,path):
    """ add a hierachy of children to the node """
    if not path:
      assert not self.ntype
      return self
    else:
      if path[0] not in self.children:
        self.children[path[0]]=ComponentTree()
      return self.children[path[0]].make(path[1:])
        
      
  def pr(self,prefix=''):
    """ print the tree """    
    for name,child in sorted(self.children.items()):
      print prefix,"%s, %s, host %s, in [%s] out [%s]"%(
        name,
        child.ntype,
        child.host,
        ', '.join([k for k in child.inputs]),
        ', '.join([k for k in child.outputs.keys()]))
      child.pr(prefix+'  ')

  def fill(self,path,name):
    """ count nb of leaves in subtree """
    self.name=name
    if path and path[0]=='/':
      self.path=name
    else:
      self.path=path+'/'+name
    if len(self.children)==0: 
      self.nleaves=1
    else: 
      self.nleaves=0
      for name2,child in self.children.items():
        child.fill(self.path,name2)
        self.nleaves+=child.nleaves

  def __repr__(self):
    return repr(self.name)
  
  # def __cmp__(self,other): 
  #   return cmp(self.nleaves,other.nleaves)


def parse_net_xml(fname):
  """ Parses a .net.xml file and builds a ComponentTree structure """
  
  dom1=xml.dom.minidom.parse(fname)
  root=ComponentTree()


  # pass 1: build nodes 


  for node in dom1.documentElement.childNodes:
    if node.__class__==xml.dom.minidom.Text:
      assert not node.data.strip()      # whitespace
      # print "text: %s"%node.data.strip
      continue 

    if node.nodeName in ('routingnode','module','filter','synchronizer'):
      thepath=str(node.getAttribute('id'))    
      cnode=root.make(thepath.split('/'))
      cnode.host=str(node.getAttribute('host'))
      cnode.ntype=str(node.nodeName)

      if cnode.ntype in ('filter', 'synchronizer'):
        cnode.classname = node.getChild(cnode.ntype + 'class').getText()
      else:
        cnode.classname = cnode.ntype                                      

      for innode in node.getChild('input').getChildren('port'):
        portname=str(innode.getAttribute('id'))
        cnode.inputs.append(portname) 

      for outnode in node.getChild('output').getChildren('port'):
        portname=str(outnode.getAttribute('id'))
        cnode.outputs[portname]=[]

      if node.nodeName == 'filter':
        cnode.filterclass = node.getChild('filterclass').childNodes[0].data
        print cnode.filterclass
        
    elif node.nodeName in ('connection','connectionstamps'):
      pass

    else:
      assert False,"unknown node %s"%str(node.nodeName)



  root.fill('','')
  # pass 2: build connections


  for node in dom1.documentElement.childNodes:
    if node.__class__==xml.dom.minidom.Text:
      continue 

    if node.nodeName in ('routingnode','module','filter','synchronizer'):
      pass
    elif node.nodeName in ('connection','connectionstamps'):

      if node.nodeName == 'connectionstamps':
        source=node.getChild('sourcestamps')
        destination=node.getChild('destinationstamps')
      else: 
        source=node.getChild('source')
        destination=node.getChild('destination')

      assert source and destination

      for ee in source.childNodes:
        if ee.__class__==xml.dom.minidom.Element: 
          ntypeid=ee.nodeName
          path=str(ee.getAttribute('id'))
          source=root.walk(path.split('/'))
          sourceport=str(ee.getAttribute('port'))
          assert source.ntype+'id'==ntypeid
          break
      else:
        assert False


      for ee in destination.childNodes:
        if ee.__class__==xml.dom.minidom.Element: 
          ntypeid=ee.nodeName
          path=str(ee.getAttribute('id'))
          destination=root.walk(path.split('/'))
          destinationport=str(ee.getAttribute('port'))
          assert destination.ntype+'id'==ntypeid
          break
      else:
        assert False

      link=node.nodeName=='connection' and "DATA" or "STAMP"

      source.outputs[sourceport].append((destination,destinationport,link))
      assert destinationport in destination.inputs   

    else:
      assert False,"unknown node %s"%str(node.nodeName)


  return root



def squeeze_levels(root):
  """ merges together nodes that have a single child """

  for name,child in root.children.items():
    del root.children[name]
    while len(child.children)==1:
      # remove child
      [(name2,child2)]=child.children.items()
      name+='/'+child2.name
      print name
      child=child2
      child2.name=name
    root.children[name]=child
    
    squeeze_levels(child)
  
