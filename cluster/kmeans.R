
mydist<-function(x,y)
  return  (sqrt(sum((x-y)^2)))

findcluster<-function(x,s1){
  whichk<-apply(s1,1,mydist,x)
  return (which.min(whichk))   
}

calcenter<-function(x,data,res){
  newdata<-data[which(res==x),]
  return (apply(newdata,2,mean))  #return class numeric
}

isequal<-function(s1,s2){
  for(i in nrow(s1)){
    if(sum((s1[i,]==s2[i,]))!=ncol(s1)){
      return (FALSE)
    }
  }
  return (TRUE)
}

calSSE<-function(x,data,res,s2){
  newdata<-data[which(res==x),]
  return (sum(apply(newdata,1,mydist,s2[x,])))  #注意s2[x,]和s2[x]的区别
}
kmeans<-function(data,k=5,center=NA){   #
    if(nrow(data)<k){
      print("data's row is too less")
      return 
    }
    if(is.na(center)) idx<-sample(1:nrow(data),k) else idx<-center
    iflag=TRUE
    # i<-1
    s1<-data[idx,]  #s1 存放center的值
    while(iflag){
      res<-apply(data,1,findcluster,s1) #class(res) integer
      s2<-t(sapply(1:k,calcenter,data,res)) #return matrix   行名没有学号
      if(isequal(s1,s2)) iflag=FALSE else iflag=TRUE
      SSE<-sapply(1:k,calSSE,data,res,s2)
      # print(SSE)
      # i<-i+1
      # print(i)
      s1<-s2
    }
    return (list(cluster=res,center=s1,SSE=SSE))  #sse for every cluster, not for all
}
kmeans_result<-kmeans(data0,k=5)   #iteration 4-21

#use the scale data
#calculate the center grade
