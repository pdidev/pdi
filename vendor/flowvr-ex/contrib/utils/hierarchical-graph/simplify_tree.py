



def find_series_names(names):
  " find a series in a list of names "  
  for name2 in names: 
    r=set()
    for name3 in names: 
      if name3[:-1]==name2[:-1]: 
        r.add(ord(name3[-1])-ord('0'))
    print name2,r
    rr=list(r)
    rr.sort()
    nelt=rr[-1]+1
    if nelt<2: continue
    if rr!=range(nelt): continue
    yield (name2[:-1],nelt)
      
def find_1series_names(names):
  " find the longest series in a list of names "
  bestseries=(None,1)
  for radical,nelt in find_series_names(names): 
    if nelt>bestseries[1]: 
      bestseries=radical,nelt
  return bestseries





def prepare_tree_for_series(node):
  " add information (redundant) to the tree nodes "
  node.series_e=0
  if not hasattr(node,'input_map'): 
    node.input_map={}
  
  for port,l in node.outputs.items():
    for node2,port2,link in l:
      if not hasattr(node2,'input_map'): 
        node2.input_map={}
      if port2 not in node2.input_map: 
        node2.input_map[port2]=[]
      node2.input_map[port2].append((node,port,link))
  for node2 in node.children.values():
    node2.parent=node
    prepare_tree_for_series(node2)



def find_destnodes(links,port,refnode): 
  # find the same node
  port2list=[]
  for l in links: 
    for node2,port2,link in l: 
      if node2==refnode: break
    else: 
      break
    port2list.append(port2)
  else:
    return "to_1_node",port2list # they all link to the same node
    
  # find different nodes with the same port name
  node2list=[]
  for l in links: 
    for node2,port2,link in l: 
      if port2==port: break
    else: 
      break
    node2list.append(node2)
  else:
    return "to_1_port",node2list # they all link to the same port on different nodes
 
  return "not_homogeneous",None

class NotASeries(Exception): 
  pass

def extract_path_series(model,path): 
  for i in range(len(model)): 
    if model[i]!=path[i]: break
  else: 
    return 0 # model == path
  if not ('0'<=model[i]<='1'): return -1 # accept 1-based numbering
  if model[i+1:]!=path[i+1:]: return -2
  return ord(path[i])-ord(model[i])
  

def parallel_explore(snodes,cur_e):
  """ Follows the links of a series of nodes in parallel, to see if
  they have ecactly the same connections. If this is not the case,
  bail out with an exception."""
  
  print "    parallel_explore",[node.path for node in snodes]
  
  if min([node.series_e for node in snodes])==cur_e: return [] # all visited 
  for node in snodes: 
    node.series_e=cur_e
  visited=[snodes]
  node0=snodes[0]

  # check names
  for i,node in enumerate(snodes): 
    if extract_path_series(node0.path,node.path)!=i: 
      raise NotASeries("node %d: path names %s %s not compatible"%(
          i,node0.path,node.path))
  
  # check inputs & outputs 
  
  for node in snodes: 
    assert hasattr(node,'input_map'),"node %s unknown"%node.path
  
  print "      inputs, outputs=",node0.inputs,node0.outputs.keys()
  
  for direction,all_node_ports in [
      ('input',[node.input_map for node in snodes]),
      ('output',[node.outputs for node in snodes])]: 
    node0_ports=all_node_ports[0]
    portset0=set(node0_ports.keys())
    for node,node_ports in zip(snodes,all_node_ports): 
      if portset0 != set(node_ports.keys()): 
        raise NotASeries("node %s does not have the same %s ports as %s (%s!=%s)"%(
            node.path,direction,node0.path,
            node0_ports.keys(),node_ports.keys()))
    for port,l in node0_ports.items(): 
      for node02,port2,link in l: 
        ltype,ld=find_destnodes(
            [node_ports[port] for node_ports in all_node_ports],port2,node02)
        print "      ",direction,port,port2,ltype

        # call recursively
        if ltype=="to_1_node":
          pass
        elif ltype=="to_1_port":
          print "        ",[node.path for node in ld]
          visited+=parallel_explore(ld,cur_e)
        elif ltype=="not_homogeneous":
          raise NotASeries("node0 %s %s %s %s is not similar to other series "%(
              node0.path,port,direction=='input' and '<-' or '->',port2))
        else: 
          assert False
  
  # check children
  
  if node0.children: 
    childset0=set(node0.children.keys())
    for i,node in enumerate(snodes):
      childset=set(node.children.keys())
      if childset0!=childset:
        raise NotASeries("node0 %s chidren %s, node %s children %s",
              node0.path,childset0, node.path,childset)
    for child in childset0: 
      visited+=parallel_explore([node.children[child] for node in snodes],cur_e)
      
  return visited

def break_link((node1,port1),(node2,port2)): 
  for i,(node1x,port1x,linkx) in enumerate(node2.input_map[port2]): 
    if (node1x,port1x) == (node1,port1): 
      del node2.input_map[port2][i]
      break
  else: raise ValueError('link not found')

  for i,(node2x,port2x,linkx) in enumerate(node1.outputs[port1]): 
    if (node2x,port2x) == (node2,port2): 
      del node1.outputs[port1][i]
      return
  else: raise ValueError('link not found')
  
def add_link((node,port),(node2,port2),ltype): 
  if (node2,port2,ltype) not in node.outputs[port]: 
    node.outputs[port].append((node2,port2,ltype))
  if (node,port,ltype) not in node2.input_map[port2]: 
    node2.input_map[port2].append((node,port,ltype))


def merge_into(node0,node):
  
  # remove node from parents
  pnode=node
  print "      rm",pnode.path
  while True: 
    ppnode=pnode.parent
    for k,v in ppnode.children.items(): 
      if v==pnode: 
        del ppnode.children[k]
        break
    else: assert False,"did not find %s in %s %s"%(pnode.path,ppnode.path,ppnode.children.keys())
    if ppnode.children: 
      break
    pnode=ppnode
  
  # copy inputs to node0: 
  
  toremove=[(port,(node2,port2),ltype) for port,l in node.input_map.items() for node2,port2,ltype in l]
  for port,(node2,port2),ltype in toremove:
    break_link((node2,port2),(node,port))
    add_link((node2,port2),(node0,port),ltype)
    
  toremove=[(port,(node2,port2),ltype) for port,l in node.outputs.items() for node2,port2,ltype in l]
  for port,(node2,port2),ltype in toremove:
    break_link((node,port),(node2,port2))
    add_link((node0,port),(node2,port2),ltype)
  

def find_series(node,cur_e=0):
  " check if there is a series of nodes in the children "
  if node.children:
    radical,nelt=find_1series_names(node.children.keys())   
    print "%s %s -> %s %s"%(node.path,node.children.keys(), radical,nelt)
    if nelt>1: 
      cur_e+=1
      print "node %s: found series %s"%(
          node.path,[("%s%d"%(radical,i)) for i in range(nelt)])
      try: 
        visited=parallel_explore([node.children["%s%d"%(radical,i)] 
              for i in range(nelt)],cur_e)
      except NotASeries,e:
        print "  ",e
      else:
        # this series can be merged
        for snodes in visited: 
          print "  merge",[node2.path for node2 in snodes]
          
          node0=snodes[0]
          if node0.children: continue # will be merged by children
          for node2 in snodes[1:]: 
            merge_into(node0,node2)
          node0.name+="*%d"%nelt
          
    for node2 in node.children.values(): 
      cur_e=find_series(node2)
  return cur_e
    
def merge_series(root): 
  root.parent=None
  prepare_tree_for_series(root)
  find_series(root,0)
