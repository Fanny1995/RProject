
#nm represent cluster neighbor matrix
# | represent or
#matrix the same as vector
#which.min return a value not index

mydist<-function(x,y){
  return (sqrt(sum( (x-y)^2 )))
}
vectordist<-function(x,data){
  return (apply(data,1,mydist,x))
}
getnm<-function(data){
  nm<-as.matrix(apply(data,1,vectordist,data),nrow=nrow(data),ncol=nrow(data))
  for(i in 1:nrow(data))
    nm[i,i]<-NA
  return (nm)
      #check the format of the result
}

updatenm<-function(nm,idx,method,cluster){   #idx  two cluster
  df<-as.data.frame(nm)     #Ǳ������
  m1<-df[idx,]
  m1<-m1[-idx]
  df<-df[-idx,-idx]
  switch(method,
         singlelink = {
           m<-1/2*m1[1,]+1/2*m1[2,]-1/2*abs(m1[1,]-m1[2,])
         },
         completelink = {
           m<-1/2*m1[1,]+1/2*m1[2,]-1/2*abs(m1[1,]-m1[2,])
         },
         groupaverge = {
           s1<-sum(cluster==idx[1])
           s2<-sum(cluster==idx[2])
           s<-s1+s2
           m<-s1/s*m1[1,]+s2/s*m1[2,]
         },
         ward = { },
         center = {}
  )
  m<-as.numeric(m)
  df<-rbind(df,m)
  m<-c(m,NA)
  df<-cbind(df,m)
  return (as.matrix(df))
}
getmergeidx<-function(nm){
  idx<-which.min(nm)  #which.min ֻ����һ��ֵ������ж����Сֵ��ô?
#  print("********")
  # print(nm[which.min(nm)])
  for( k in 1:nrow(nm)){
    if(idx %in% seq((k-1)*nrow(nm)+1,(k-1)*nrow(nm)+nrow(nm))){
      nrow<-k  #�ֲ������ⲿ����
      break
    }
  }
  ncol<-idx-nrow(nm)*(nrow-1)
  idx<-c(nrow,ncol)
  return (idx)
}
ghc<-function(data,k,method){
  nm<-getnm(data)
  # write.csv(nm,file="nm1.csv");
  cluster<-seq(1,nrow(data),by=1)
  num<-nrow(nm)
  # i<-1;
  while(num>k)
  { 
    idx<-getmergeidx(nm) 
    # print(idx)
    nm<-updatenm(nm,idx,method,cluster)
    # i<-i+1
    # filename<-paste("nm",i,".csv",sep="")
    # write.csv(nm,file=filename)
    # wrong cluster[cluster==idx[1]|cluster==idx[2]]<-ncol(nm)
    if(idx[1]<idx[2]){
      idx<-c(idx[2],idx[1])
    }
    cluster[cluster==idx[1]| cluster==idx[2]]<--1
    cluster[cluster>idx[2] & cluster<idx[1]]<-cluster[cluster>idx[2] & cluster<idx[1]]-1
    cluster[cluster>idx[1]]<-cluster[cluster>idx[1]]-2
    cluster[cluster==-1]<-ncol(nm)
    # show(table(cluster))
    # show(cluster)
    num<-num-1
    # print("***************")
  }
  return (cluster)  #��ʱû������غû��Ļ��ƣ�û�д����ġ�
}
res<-ghc(data0[1:50,],5,"groupaverge")
table(res)
#������ȫ������Ч�����ã������һ���ػ�ܴ󣬰������سԵ�
#��ƽ��������,Ҳ���Ǻܺ�
  # res ��������Щ��ӽ�ȥЧ�����ã�������ɾ����Щ�㣬�����ܻ��и���صĳ�Աֻ��һ����
  # 1   2   3   4   5 
  # 1   1   2   8 417 
  #�������������㲻����һ���������󣿣���������������������ڲ��õ�Ĵ��ڣ������㷨���������ʣ�
