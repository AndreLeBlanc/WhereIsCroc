# Calculates the Euclidean distance between two points.
euclidean = function(src,dest) {
  # a=abs(src[[1]])^2
  # b=abs(src[[2]])^2
  # c=sqrt(a+b)
  # return(c)
  return(0)
}

# Calculate the f(n) value for A* of a node.
nodeVal = function(src,curr,dest,dist) {
  cost=dist
  heuristic=euclidean(src,dest)
  return(list(f=cost+heuristic,g=cost,h=heuristic))
}

createNode = function(src,curr,f) {
  return(list(list(f=f,node=curr,prev=src)))
}

# Insert a f(n) value of a node into the sorted frontier list.
insertFrontier = function(frontier,node) {
  insert=F
  len=length(frontier)
  
  for(i in 1:len) {
    e=frontier[[i]]
    if (f<e$f) {
      frontier=append(frontier,node,after=(i-1))
      insert=T
      break
    }    
  }
  if (!insert) {
    frontier = append(frontier,node)
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

astar = function(tree,edges,src,dest) {
  edgeNodes=getOptions(src,edges)
  numNodes=length(tree)
  numEdges=length(edgeNodes)
  graph=list(rep(0,numNodes))
  frontier=list()
  root=createNode(0,src,0)
  graph[[src]]=root
  
  curr=src
  dist=1
  while(curr!=dest) {
    for(i in 1:numEdges) {
      edge=edgeNodes[[i]]
      val=nodeVal(src,edge,dest,dist)
      node=createNode(src,edge,val)
      insertFrontier(frontier,node,val)
      
      old=graph[[i]]
      if(old==0 | old$f<node$f) {
        graph[[i]]=node 
      }
    }
    curr=frontier[[1]]
    frontier=frontier[-1]
    dist=dist+1
  }
  
  move=traverse(graph,dest)
  return(move)
}