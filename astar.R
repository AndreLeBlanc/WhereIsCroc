#heuristic = function(src,dest,points) {
#  srcx=points[[src,1]]
#  srcy=points[[src,2]]
#  destx=points[[dest,1]]
#  desty=points[[dest,2]]
#  x=sqrt(abs(srcx-destx))
#  y=sqrt(abs(srcy-desty))
#  return(ceiling(x+y))
#}

heuristic = function(curr, dest) {
  diff = abs(curr-dest)
  if(diff <= 8) {
    return (1)
  }
  else if (diff <= 14) {
    return (2)
  }
  else if (diff <= 19) {
    return (3)
  }
  else if (diff <= 24) {
    return (4)
  }
  else if (diff <= 29) {
    return (5)
  }
  else if (diff <= 34) {
    return (6)
  }
  else {
    return (7)
  } 
}

# Calculate the f(n) value for A* of a node.
nodeVal = function(src,curr,dest,dist) {
  cost=dist
  #heuristic=heuristic(src,dest,points)
  heuristic=heuristic(curr,dest)
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
  prev=curr[[3]]
  if(prev==0) {
    return(list(dest))
    #return(dest)
  }
  i = 1
  path=list()
  while(T) {
    list[[ curr[[2]] ]]
    temp=prev
    curr=graph[[prev]]
    prev=curr[[3]]
    if(prev==0) {
      #return(temp)
      return(path)
    }
  }
  #return(0)
  return(list())
}

astar = function(tree,edges,src,dest) {
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
      #val=nodeVal(src,edge,dest,dist,points)
      val=nodeVal(src,edge,dest,dist)
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
  #return(c(move,0))
  return(move)
}