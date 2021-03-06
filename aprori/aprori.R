
#向list中增加元素，newvalue<-c(list(),x=list(c(...)))
#############################
#gen_candinateset 函数说明
#传进的参数fk-1是当前要处理的频繁项集，k是要产生的项的长度
#对fk-1中的每个频繁项集进行排序
#候选集产生并进行剪枝
#返回候选k项集
###########################
gen_candiset<-function(fk,k){#fk is current frequent set,   k is the candinate set scale
  if(length(fk)<=1) return (list()) #如果频繁集元素个数小于1，没有合并的必要
  for(i in 1:length(fk)){
    fk[[i]]<-fk[[i]][order(fk[[i]])]
  }
  # print(fk)
  kk<-k-2
  # print(kk)
  ck<-list()
  #候选集的产生
  if(kk==0){
    for(i in 1:(length(fk)-1))
      for(j in (i+1):length(fk)){
        ck<-c(ck,list(c(fk[[i]],fk[[j]])))  #要加list(,,,)
      }
  }
  else{
    for(i in 1:(length(fk)-1))
      for(j in (i+1):length(fk)){
        if(sum(fk[[i]][1:kk]==fk[[j]][1:kk])==kk & fk[[i]][kk+1]!=fk[[j]][kk+1]){
          add<-list(c(fk[[i]][1:(kk+1)],fk[[j]][kk+1]))
          for(s in add[[1]]){
            flag<-0
            set<-setdiff(add[[1]],s)
            # print(set)
            for(t in 1:length(fk)){
              if(length(setdiff(set,fk[[t]]))==0){
                flag<-1
                break
              }
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
  # print(ck)
  return (ck)
}

cal_supportnum<-function(tt,ck){
  num<-rep(0,length(ck))
  for(i in 1:length(tt))
    for(j in 1:length(ck)){
      if(sum(ck[[j]] %in% tt[[i]])==length(ck[[j]])){
        num[j]<-num[j]+1
      }
    }
  return (num)
}
isequalset<-function(s1,s2){
  if(length(s1)!=length(s2))
    return (-1)
  else if(length(s1)==length(union(s1,s2))){
    return (1)
  }
  return (-1)
}

gen_allrule<-function(ff,sup,Minconf){
  rule<-list()
  for(i in 1:length(ff)){
    if(length(ff[[i]])<2) next
    f<-ff[[i]]
    s0<-sup[i]
    hm<-list()
    m=1
    k=length(f)
    for(j in 1:k){
      s1<-NA
      for(z in 1:length(ff)){
        if(isequalset(setdiff(f,f[j]),ff[[z]])==1){
          s1<-sup[z]
          break
        }
      }
      if(!is.na(s1)){  #这个is.na判断不是很必要
        if(s0/s1>=Minconf){
          hm<-c(hm,f[j])
          rule<-c(rule,list(list(front=setdiff(ff[[i]],f[j]),last=f[j],totalsup=s0,xsup=s1,conf=s0/s1)))
        }
      }
    }
    while(k>m+1){
      m<-m+1
      h2<-gen_candiset(hm,m)
      if(length(h2)==0) break
      hm<-list()
      for(j in 1:length(h2)){
        s1<-NA
        for(z in 1:length(ff)){
          if(isequalset(ff[[z]],h2[[j]])==1){
            s1<-sup[z]
            break
          }
        }
        if(!is.na(s1)){
          if(s0/s1>=Minconf){
            hm<-c(hm,list(h2[[j]]))
            rule<-c(rule,list(list(front=setdiff(ff[[i]],h2[[j]]),last=h2[[j]],totalsup=s0,xsup=s1,conf=s0/s1)))
          }
        }
      }
    }
  }
  return (rule)
}

aprori<-function(tt,Minsup,Minconf){  #Minsup给的是个数，如果要给一个比例，在程序中改变一下
  if(length(tt)==0){
    print("tt is null")
    return ()
  }
  
  f0<-c()
  for(i in 1:length(tt)){
    f0<-c(f0,tt[[i]])
  }
  f0<-unique(f0)
  
  fk<-list()
  for(i in 1:length(f0)){    #得到所有事务的1项集
    fk<-c(fk,list(f0[i]))
  }
  if(length(fk)==0){
    print("一项集是null")
    return ()
  }
  # print(fk)
  sup<-cal_supportnum(tt,fk)   #用来存储所有频繁项集的支持度
  # print(sup)
  idx<-which(sup>Minsup)    
  # print(idx)
  fk<-fk[idx]
  sup<-sup[idx]              #找到所有的频繁1项集
  if(length(fk)==0){
    print("频繁一项集是null")
    return ()
  }
  ff<-fk    #用来存储所有的频繁项集
  # print(fk)
  k<-1
  while(1){
    # print("************")
    k<-k+1
    ck<-gen_candiset(fk,k)      #由k-1项频繁集产生的k项候选集
    if(length(ck)==0) break
    tempsup<-cal_supportnum(tt,ck)
    idx<-which(tempsup>Minsup*length(tt))
    fk<-ck[idx]
    # print(fk)
    if(length(fk)==0) break
    ff<-c(ff,fk)  
    sup<-c(sup,tempsup[idx])
  }
  sup<-sup/length(tt)  
  # print("************")
  rule<-gen_allrule(ff,sup,Minconf)
  return (rule)
}
showonerule<-function(x,cour_name,term){
  frontterm<-max(as.numeric(term[x[[1]]]))
  rearterm<-min(as.numeric(term[x[[2]]]))
  allsup<-x[[3]]
  frontsup<-x[[4]]
  conf<-x[[5]]
  if(frontterm>rearterm){
    return () 
  }
  else{
    frontname<-cour_name[x[[1]]]
    rearname<-cour_name[x[[2]]]
    str<-c()
    for(i in 1:length(frontname)){
      str<-paste(str,frontname[i],"   ",sep="")
    }
    str<-paste(str,"-->  ",sep="")
    for(i in 1:length(rearname)){
      str<-paste(str,rearname[i],"    ",sep="")
    }
    str<-paste(str,"allsup=",round(allsup,digit=2)," frontsup=",round(frontsup,digit=2),"   conf=",round(conf,digits=2),sep="")
    return (str)
  }
}
showallrule<-function(rule,cour_name,term,sequence="decline"){
  conf<-sapply(rule,function(x) return (x[[5]]))
  rule<-rule[order(conf,decreasing=TRUE)]
  res<-sapply(rule,showonerule,cour_name,term)
  temp<-list()
  for(i in 1:length(res)){
    if(!is.null(res[[i]])){
      temp<-c(temp,res[i])
    }
  }
  return (temp)
}
tt<-pre_aprori_res$transac
cour_name<-pre_aprori_res$tt_num_meaning
term<-pre_aprori_res$tt_term
rule<-aprori(tt,0,0.8)
allrule<-showallrule(rule,cour_name,term)
####################
#补充加一些关联规则的评价指数进去。