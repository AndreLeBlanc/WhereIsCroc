heuristic = function(src,dest,points) {
  srcx=points[[src,1]]
  srcy=points[[src,2]]
  destx=points[[dest,1]]
  desty=points[[dest,2]]
  x=sqrt(abs(srcx-destx))
  y=sqrt(abs(srcy-desty))
  return(ceiling(x+y))
}

# Calculate the f(n) value for A* of a node.
nodeVal = function(src,curr,dest,dist,points) {
  cost=dist
  heuristic=heuristic(src,dest,points)
  return(list(f=cost+heuristic,g=cost,h=heuristic))
}

createNode = function(src,curr,f) {
  return(list(f,curr,src))
}

# Insert a f(n) value of a node into the sorted frontier list.
insertFrontier = function(frontier,node) {
  insert=F
  len=length(frontier)
  
  for(i in 1:len) {
    if(len==0) { break }
    e=frontier[[i]]
    if (node[[1]]<e[[1]]) {
      frontier=append(frontier,list(node),after=(i-1))
      insert=T
      break
    }    
  }
  if (!insert) {
    frontier=append(frontier,list(node))
  }
  
  return(frontier)
}

# Traverse the graph from the destination to the start to determine the path the A* algorithm found.
traverse = function(graph,dest) {
  curr=graph[[dest]]
  prev=curr$prev
  if(prev==0) {
    return(dest)
  }
  while(T) {
    temp=prev
    curr=graph[[prev]]
    prev=curr$prev
    if(arr==0) {
      return(temp)
    }
  }
  return(0)
}

astar = function(tree,points,edges,src,dest) {
  numNodes=length(tree)
  graph=list()
  frontier=list()
  root=createNode(0,src,0)
  graph[[src]]=root
  
  curr=src
  dist=1
  while(curr!=dest) {
    edgeNodes=getOptions(curr,edges)
    numEdges=length(edgeNodes)
    for(i in 1:numEdges) {
      edge=edgeNodes[[i]]
      val=nodeVal(src,edge,dest,dist,points)
      node=createNode(src,edge,val$f)
      frontier=insertFrontier(frontier,node)
      
      old=graph[[i]]
      if(!is.null(old)) {
        if(old[[1]]>node[[1]]) {
          graph[[i]]=node
        }
      }
      else {
        graph[[i]]=node
      }
    }
    curr=(frontier[[1]])[[2]]
    frontier=frontier[-1]
    dist=dist+1
  }
  
  move=traverse(graph,dest)
  return(c(move,0))
}