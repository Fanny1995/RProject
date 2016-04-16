#matrix is like a vector rownames colnames-->names
#dataframe 's subset is a dataframe  rownames colnames-->rownames,colnames
#apply(c(1,2),1,fun)   error
# if null,use length to judge, is.na is not approiate
#sapply return a matrix like ,,,,

mydist<-function(x,y){
  return (sqrt(sum((x-y)^2)));
}

vectordist<-function(x,data){
  return (apply(data,1,mydist,x))
}

mydistmatrix<-function(data){
  return (apply(data,1,vectordist,data))  #dim c(n,n)
}

calcorepoint<-function(distmatrix,Eps,Minpts){
  return (
    apply(distmatrix,1,function(x){
       return (ifelse(sum(x<Eps)>=Minpts,TRUE,FALSE))
    })
  )
}

#c(0,-1) 表示不是边界点 caledgepoint return a matrix
caledgepoint<-function(distmatrix,Eps,Minpts){
  idxcore<-apply(distmatrix,1,function(x){
    return (ifelse(sum(x<Eps)>=Minpts,TRUE,FALSE))
  })
  return (apply(distmatrix,1,function(x){
    if(sum(x<Eps)>=Minpts) return (c(FALSE,-1))
    xx<-ifelse(sum(x<Eps & idxcore)>=1,TRUE,FALSE)   
    if(xx==FALSE) return (c(FALSE,-1))
    yy<-which.min(x[x<Eps & idxcore])  # yy表示最近的那个核心点
    return (c(xx,yy))
  }))
}

calnoisepoint<-function(distmatrix,Eps,Minpts){
  idxcore<-apply(distmatrix,1,function(x){
    return (ifelse(sum(x<Eps)>=Minpts,TRUE,FALSE))
  })
  return (apply(distmatrix,1,function(x){#函数内部可以直接使用外部变量（已赋值）
    return (ifelse(sum(x<Eps & idxcore)==0,TRUE,FALSE))
  }))                #check three function right or wrong
}

dbscan<-function(data,Eps,Minpts){ 
  distmatrix<-mydistmatrix(data)
  noiseidx<-calnoisepoint(distmatrix,Eps,Minpts)#处理完噪声点之后数据可能为空
  cat("noiseidx: ",which(noiseidx),"length: ",length(which(noiseidx)),"\n")
  if(sum(noiseidx)!=0)
    distmatrix<-distmatrix[-which(noiseidx),-which(noiseidx)] #必须加which
  if(length(distmatrix)==0){
    print("the data are all noise,you may need to adjust the Eps and Minpts")
    return ()
  }
  cluster<-rep(NA,nrow(distmatrix))
  names(cluster)<-rownames(distmatrix)
  
  coreidx<-calcorepoint(distmatrix,Eps,Minpts)
  if(sum(coreidx)==0){
    print("there is no corepoint,you may need to adjust the Eps and Minpts")
    return ()
  }
  edgematrix<-caledgepoint(distmatrix,Eps,Minpts)
  cat("coreidx: ",which(coreidx),"       ","length: ",length(which(coreidx)),"\n")
  cat("edgematrix:....","        ","length: ",length(which(edgematrix[1,]==1)),"\n")
  # show(edgematrix)
  if(length(edgematrix)!=0){
    edgeidx<-which(edgematrix[1,]==1)
    edgefor<-edgematrix[2,edgeidx]
  }
  s0<-which(coreidx)    #s0 不变，s1表示现在已经处理过的点了
  kk<-1
  while(1){
    s1<-s0[1]
    s2<-c()
    while(length(s1)!=length(s2)){
      newadd<-c()
      for(i in setdiff(s1,s2)){
        newadd<-c(newadd,setdiff(which(distmatrix[i,]<Eps & coreidx),newadd))
      }
      s2<-s1
      s1<-unique(c(s1,newadd))
    }
    cluster[s1]<-kk
    kk<-kk+1
    s0<-s0[-which(s0 %in% s1)]
    if(length(s0)==0) break;
  }
  if(length(edgematrix)!=0)
    cluster[edgeidx]<-cluster[edgefor]
  return (cluster)
}

distmatrix<-mydistmatrix(data0)
myfunc<-function(distmatrix,k){
  return (apply(distmatrix,1,function(x){
    x<-x[order(x)]
    return (mean(x[1:k+1]))
  }))
}
res<-myfunc(distmatrix,k=3)
plot(res[order(res)],xlab="point number")

# for(i in 100:120){ 我有写错吗
#   for(j in 100:150){
#     res<-dbscan(data0,i,j)
#     show(as.data.frame(t(res)))
#   }
# }

res<-dbscan(data0[1:10,],80,1)   #怎么去寻找Eps 和 Minpts




