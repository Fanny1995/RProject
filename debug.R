gen_candiset<-function(fk,k){#fk is current frequent set,   k is the candinate set scale
  if(length(fk)<=1) return (list()) #如果频繁集元素个数小于1，没有合并的必要
  for(i in 1:length(fk)){
    fk[[i]]<-fk[[i]][order(fk[[i]])]
  }
  fir<-sapply(fk,"[",1)
  fk<-fk[order(fir)]
  fir<-fir[order(fir)]
  ref<-unique(fir)
  names(ref)<-ref
  ref<-sapply(ref,function(x){
    return (which(fir==x)[1])
  })
  # show(fk)
  # show(ref)
  kk<-k-2
  ck<-list()
  #候选集的产生
  if(kk==0){
    ck<-sapply(1:(length(fk)-1),function(x){
      return (sapply((x+1):length(fk),function(y) return (list(c(fk[[x]],fk[[y]])))))
    })
    ck<-unlist(ck)
    ck<-sapply(1:(length(ck)/2),function(x){
      idx<-(x-1)*2+1
      return (list(ck[idx:(idx+1)]))
    })
  }
  else{
    for(i in 1:(length(fk)-1))
      for(j in (i+1):length(fk)){
        if(sum(fk[[i]][1:kk]==fk[[j]][1:kk])==kk & fk[[i]][kk+1]!=fk[[j]][kk+1]){
          show(fk[[i]])
          show(fk[[j]])
          add<-list(c(fk[[i]][1:(kk+1)],fk[[j]][kk+1]))
          for(s in add[[1]][1:kk]){
            flag<-0
            set<-setdiff(add[[1]],s)
            set<-set[order(set)]
            temp<-as.character(set[1])
            idx<-ref[temp]
            if(is.na(idx)) break
            while(fk[[idx]][1]==set[1]){
              if(length(setdiff(set,fk[[idx]]))==0){
                # print("****")
                flag=1
                break
              }
              idx<-idx+1    #效果很好了
              if(idx>length(fk)) break
            }
            if(flag==0){
              break
            }
          }
          if(flag)
            ck<-c(ck,add)
        }
      }
  }
  return (ck)
}
ck<-gen_candiset(fk,3)
