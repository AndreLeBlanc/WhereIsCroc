# Calculates the Euclidean distance between two points.
euclideanD = function(srcX, srcY, destX, destY) {
  a = abs(srcX - destX) ^ 2
  b = abs(srcY - destY) ^ 2
  c = sqrt(a + b)
  return (c)
}


# Calculate the f(n) value for A* of a node.
nodeVal = function(roads, src, srcX, srcY, destX, destY) {
  cost=1
  heuristic=manhattanD(srcX, srcY, destX, destY)
  return (list(g = cost, h = heuristic))
}


# Insert a f(n) value of a node into the sorted frontier list.
insertFrontier = function(frontier, f, x, y) {
  i = 1
  insert = F
  len = length(frontier)
  
  while (i <= len) {
    e = frontier[[i]]
    if (f < e[[1]]) {
      frontier = append(frontier, list(list(f, x, y)), after = (i - 1))
      insert = T
      break
    }
    i = i + 1
  }

  if (!insert) {
    frontier = append(frontier, list(list(f, x, y)))
  }
  
  return (frontier)
}

# Traverse the graph from the destination to the start to determine the path the A* algorithm found.
traverseArrow = function(mat, dest) {
  prev = mat[[dest[[1]], dest[[2]]]]
  curr = prev
  arr = curr$arrow
  
  if (arr[[1]] == 0) {
    return (prev$arrow)
  }

  depth = 1  
  while (T) {
    temp = mat[[arr[[1]], arr[[2]]]]
    arr = temp$arrow
    if (arr[[1]] == 0) {
      if (depth == 1) {
        return (dest)
      }
      return (prev$arrow)
    }
    
    prev = curr
    curr = temp
    depth = depth + 1
  }
  
  return (NULL)
}

astar = function(tree,edges,src,dest) {
  edgeNodes=getOptions(src,edges)
  numNodes=length(tree)
  numEdges=length(edgeNodes)
  graph=list(rep(0,numNodes))
  frontier=list()
  
  curr=src
  dist=1
  while(curr!=dest) {
    for(i in 1:numEdges) {
      node=edgeNodes[[i]]
      val=nodeVal(edges,src,node,dist)
      insertFrontier(frontier,node,val)
      graph[[i]]=val
    }
    curr=frontier[[1]]
    frontier=frontier[-1]
    dist=dist+1
  }
  
  move=traverse(graph,dest)
  return(move)
}