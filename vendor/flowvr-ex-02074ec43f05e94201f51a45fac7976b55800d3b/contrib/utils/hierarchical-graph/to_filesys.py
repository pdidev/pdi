import sys,os,errno

import parsetree

def usage():
  print >>sys.stderr,"""to_filesys.py in.net.xml outputdir
Transforms the component tree into a directory tree
links become symlinks"""
  sys.exit(1)

if len(sys.argv) < 2:
  usage()  
  

infile=sys.argv[1]
outdir = sys.argv[2] + '/' if len(sys.argv) == 3 else 'net_xml_to_filesys/'

os.system("rm -rf %s"%outdir)

def nodes_to_fs(node):
  os.mkdir(outdir+'/'+node.path)

  if node.ntype and node.host: 
    # touch a file
    open('%s/%s/_%s on %s' % (outdir, node.path, node.classname, node.host), 'w')

  for name,node2 in node.children.iteritems():
    nodes_to_fs(node2)

def links_to_fs(node):

  for name,node2 in node.children.iteritems():
    links_to_fs(node2)

  for outport,links in node.outputs.items():
    for dest,destport,link in links:
      lno=0
      while True:
        try:
          os.symlink(dest.path+'/'+destport+'(in)',
                     outdir+node.path+'/'+outport+'(out%s%s)'%(lno if lno else '', link=="STAMP" and ' stamps' or ''))
          break
        except OSError,e:
          if e.errno!=errno.EEXIST:
            raise
        lno+=1
      lno=0
      while True:
        try:
          os.symlink(node.path+'/'+outport+'(out)',
                     outdir+dest.path+'/'+destport+'(in%s)'%(lno if lno else ''))
          break
        except OSError,e:
          if e.errno!=errno.EEXIST:
            raise
        lno+=1
      
      



tree=parsetree.parse_net_xml(infile)

nodes_to_fs(tree)

links_to_fs(tree)
