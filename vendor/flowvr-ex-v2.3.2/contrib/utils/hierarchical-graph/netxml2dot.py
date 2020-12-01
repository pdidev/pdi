import sys,math
import xml.dom.minidom


import parsetree

def usage():
  print >>sys.stderr,"""

usage: %s [options] in.net.xml out.dot
  
Generates a hierarchical graph in dot format from a .net.xml file
output by flowvw.

Options:

-scalef x   scale factor, boxes get scaled by x ** -depth
-scalet     also scale box titles
-merge      detect and merge sequences of boxes
  
"""%sys.argv[0]
  sys.exit(1)

args=sys.argv[1:]

netxml_name=None
dot_name=None
scalef_base=1.0
scalet=False
merge=False

while args:
  a=args.pop(0)
  if a in ('-h','--help'):  usage()
  elif a=='-scalef':        scalef_base=float(args.pop(0))
  elif a=='-scalet':        scalet=True
  elif a=='-merge':         merge=True
  elif not netxml_name:     netxml_name=a
  elif not dot_name:        dot_name=a
  else:
    print >>sys.stderr,"unknown arg "+a
    usage()
 


root=parsetree.parse_net_xml(netxml_name)
dot_file=open(dot_name, "w")

  
# root.pr()

import simplify_tree

if merge: simplify_tree.merge_series(root)

def sanitize(name):
  # make a valid dot identifier
  name=name.replace('/','_')
  name=name.replace('-','_minus_')
  return name

def render_nodes(node, dot_file, prefix='', name_prefix=''):
  """ render a node and call recursively on children """
  
  scalef=scalef_base**(-len(prefix)/2.0) # len(prefix)/2 is the depth of the tree
  titlefs=(math.log(node.nleaves) if scalet else 1.0)*14
      
  if node.children:
    # compoent with subcomponents (does not have ports)
    if len(node.children)>1: 
      print >>dot_file, prefix,"subgraph cluster_%s {"%sanitize(node.path)
      print >>dot_file, prefix,"  fontsize=%.1f; "%(titlefs)
      print >>dot_file, prefix,"  nodesep=%.1f; "%(scalef*0.25)
      print >>dot_file, prefix,"  label=\"%s\" ;"%(name_prefix+node.name)
      for name2,node2 in sorted(node.children.items()):
        render_nodes(node2, dot_file, prefix+"  ")
      print >>dot_file, prefix,"}"
    else: 
      # don't make an extra enclosing box for single children
      [(name2,node2)]=node.children.items()      
      render_nodes(node2, dot_file, prefix, name_prefix+node.name+'/')      

  else:
    # module with inputs and outputs
    inports='|'.join(["<%s> %s"%(sanitize(i),i) for i in node.inputs])
    outports='|'.join(["<%s> %s"%(sanitize(i),i) for i in sorted(node.outputs.keys())])
    if node.ntype=='module':
      shape='shape=record,  style=filled, fillcolor=green1'
    elif node.ntype=='filter':
      shape='shape=Mrecord, style=filled, fillcolor=purple'
    elif node.ntype=='synchronizer':
      shape='shape=Mrecord,  style=filled, fillcolor=lightsalmon'
    elif node.ntype=='routingnode':
      shape='shape=Mrecord, style=filled, fillcolor=white'
    else:
      assert False
    shape+=", fontsize=%.1f, height=%g, width=%g, margin=\"%g,%g\", penwidth=%g"%(
        14*scalef, 0.5*scalef, 0.75*scalef, 0.11*scalef, 0.55*scalef, 2.0*scalef)
          
    print >>dot_file, prefix,"%s [%s, label=\"{{%s} | %s | {%s}}\"] ;"%(
        sanitize(node.path), shape, inports, name_prefix+node.name, outports)


def render_links(node, dot_file, prefix=''):
  """ add all links for nodes """

  if node.children:
    for name2,node2 in sorted(node.children.items()):
      render_links(node2, dot_file, prefix+"  ")

  for port,l in sorted(node.outputs.items()):
    for node2,port2,link in l:
      # if link=="STAMP": continue  
      print >>dot_file, prefix, "%s:%s -> %s:%s %s ; "%(
        sanitize(node.path),sanitize(port),
        sanitize(node2.path),sanitize(port2),
        link=="STAMP" and "[weight=0.1,style=dashed]" or "")


# find node with several children

node=root
while len(node.children)==1:
  node=node.children.values()[0]


print >>dot_file, "digraph G {"
# render_nodes and _links are separate because dot seems to crash when they are mixed
render_nodes(node, dot_file, ' ')
render_links(node, dot_file, ' ')
print >>dot_file, "}"
