source("astar.R")

# Calculates normal distribution probablity of obs within the 
# interval [obs-sqrt(dev), obs+sqrt(dev)].
probability = function(mean,dev,obs) {
  sqdev=sqrt(dev)
  upper=obs+sqdev
  lower=obs-sqdev
  return(pnorm(upper,mean,dev,lower.tail=TRUE)-pnorm(lower,mean,dev,lower.tail=TRUE))
}

statTourist = function(pos) {
  for(i in 1:2) {
    if(!is.na(pos[[i]])) {
      if(pos[[i]]<0) {
        return(pos[[i]])
      } 
    } 
  }
  return(0)
}

distribution = function(mat,obs) {
  l=list(rep(0,nrow(mat)))
  for(i in 1:40) {
    l[[i]]=probability(mat[[i,1]],mat[[i,2]],obs)
  }
  return(l)
}

sumDist = function(salDist,phoDist,nitDist) {
  salNorm=normalize(salDist)
  phoNorm=normalize(phoDist)
  nitNorm=normalize(nitDist)
  
  len=length(salDist)
  sumDist=list(rep(0,len))
  for(i in 1:len) {
    # sumDist[[i]]=salDist[[i]]+phoDist[[i]]+nitDist[[i]]
    sumDist[[i]]=salNorm[[i]]*phoNorm[[i]]*nitNorm[[i]]
  }
  return(sumDist)
}

normalize = function(sumDist) {
  len=length(sumDist)
  sum=0
  for(i in 1:len) {
    sum=sum+sumDist[[i]]
  }
  norm=list(rep(0,len))
  for(i in 1:len) {
    norm[[i]]=sumDist[[i]]/sum
  }
  
  return(norm)
}

forward = function(prevf,trans,obs) {
  len=length(trans)
  forwDist=list(rep(0,len))
  for(i in 1:len) {
    edges=trans[[i]]
    numEdges=length(edges)
    val=1/numEdges
    forw=0
    
    for(k in 1:numEdges) {
      pss=1
      if(!is.null(prevf)) {
        pss=prevf[[ edges[[k]] ]]
      }
      forw=forw+pss*val
    }
    forwDist[[i]]=forw*obs[[i]]
  }
  
  return(forwDist)
}

findNode = function(forw) {
  len=length(forw)
  max=0
  index=20
  for(i in 1:len) {
    val=forw[[i]]
    if(val>max) {
      max=val
      index=i
    }
  }
  
  return(index)
}

markovMoves = function(moveInfo,readings,positions,edges,probs) {
  mem=moveInfo$mem
  if(length(mem)==0) {
    mem=list(trans=transitionMatrix(),points=getPoints(),prevf=NULL,path=NULL,dest=0)
  }

  eaten=statTourist(positions)
  obs=NULL
  if(eaten>0) {
    obs=list(rep(0,length(trans)))
    obs[[eaten]]=1
  }
  else {
    salDist=distribution(probs$salinity,readings[[1]])
    phoDist=distribution(probs$phosphate,readings[[2]])
    nitDist=distribution(probs$nitrogen,readings[[3]])
    sumDist=sumDist(salDist,phoDist,nitDist)
    obs=normalize(sumDist)
  }
  
  forwDist=forward(mem$prevf,mem$trans,obs)
  norm=normalize(forwDist)
  mem$prevf=norm
  
  node=findNode(forwDist)
  print(paste("HMM predicts croc at: ",node))
  if(node!=mem$dest) {
    mem$dest=node
    mem$path=shortestPath(mem$trans,positions[[3]],node)
  }
  
  move=nextMove(mem$path,mem$dest)
  mem$path=move[[1]]
  moveInfo$mem=mem
  moveInfo$moves=move[[2]]
  #moveInfo$moves=c(sample(getOptions(positions[3],edges),1),0)
  return(moveInfo)
}

nextMove = function(path,dest) {
  len=length(path)
  if(len>=3) {
    return(list(path[3:len],c(path[[1]],path[[2]])))
  }
  else if(len==2) {
    return(list(list(),c(path[[1]],path[[2]])))
  }
  else if(len==1) {
    return(list(list(),c(path[[1]],0)))
  }
  else {
    return(list(list(),c(dest,0)))
  }
}

shortestPath = function(trans,src,dest) {
  path=getPath(trans,src,dest,1,NULL,list())
  return(path[[2]])
}

getPath = function(trans,src,dest,dist,min,seq) {
  if(src==dest) {
    return(list(0,list(src)))
  }
  if(dist>10) {
    return(NULL)
  }
  
  l=trans[[src]]
  found=match(dest,l)
  if(!is.null(min)) {
    if(length(seq)>min) {
      return(NULL)
    } 
  }
  
  path=NULL
  if(!is.na(found)) {
    seq=append(seq,dest)  
    return(list(dist,seq))
  }
  else {
    numEdges=length(l)
    for(i in 1:numEdges) {
      if(l[[i]]==src) {}
      else if(is.na(match(l[[i]],seq))) {
        tseq=append(seq,l[[i]])
        tpath=getPath(trans,l[[i]],dest,dist+1,min,tseq)
        if(!is.null(tpath)) {
          if(is.null(min)) {
            path=tpath
            min=path[[1]]
          }
          else if(tpath[[1]]<min) {
            path=tpath
            min=path[[1]]
          }
        }
      }
    }
  }
  return(path)
}

transitionMatrix = function() {
  trans=list()
  trans=append(trans,list(list(1,2,4,6)))
  trans=append(trans,list(list(1,2,4,5)))
  trans=append(trans,list(list(3,5,7)))
  trans=append(trans,list(list(1,2,4,6,8)))
  trans=append(trans,list(list(2,3,5,7,9)))
  trans=append(trans,list(list(1,4,6,12)))
  trans=append(trans,list(list(3,5,7,11,13)))
  trans=append(trans,list(list(4,8,9,10)))
  trans=append(trans,list(list(5,8,9,11)))
  trans=append(trans,list(list(8,10,12,14)))
  trans=append(trans,list(list(7,9,11,13,15)))
  trans=append(trans,list(list(6,10,12,16)))
  trans=append(trans,list(list(7,11,13,18)))
  trans=append(trans,list(list(10,14,15,16)))
  trans=append(trans,list(list(11,14,15,17)))
  trans=append(trans,list(list(12,14,16,19,22)))
  trans=append(trans,list(list(15,17,18,19,20)))
  trans=append(trans,list(list(13,17,18,21)))
  trans=append(trans,list(list(16,17,19,20,22)))
  trans=append(trans,list(list(17,19,20,23)))
  trans=append(trans,list(list(18,21,23,29)))
  trans=append(trans,list(list(16,19,22,24,27)))
  trans=append(trans,list(list(20,21,23,24,25)))
  trans=append(trans,list(list(22,23,24,25,27)))
  trans=append(trans,list(list(23,24,25,26,27,28)))
  trans=append(trans,list(list(25,26,28,29)))
  trans=append(trans,list(list(22,24,25,27,30,31)))
  trans=append(trans,list(list(25,26,28,31,32)))
  trans=append(trans,list(list(21,26,29,32,35)))
  trans=append(trans,list(list(27,30,31,34)))
  trans=append(trans,list(list(27,28,30,31,33,34)))
  trans=append(trans,list(list(28,29,32,33,35)))
  trans=append(trans,list(list(31,32,33,35,36,37)))
  trans=append(trans,list(list(30,31,34,36,38)))
  trans=append(trans,list(list(29,32,33,35,40)))
  trans=append(trans,list(list(33,34,36,37,39)))
  trans=append(trans,list(list(33,36,37,39,40)))
  trans=append(trans,list(list(34,38,39)))
  trans=append(trans,list(list(36,37,38,39)))
  trans=append(trans,list(list(35,37,40)))
  return(trans)
}