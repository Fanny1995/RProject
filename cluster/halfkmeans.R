
setwd("C:/Users/fresh guoguo/Desktop/cluster/")
source("k-means.R")

#ddcluster<-function(clusterset,totalres,totalsse,totalcenter){  #function can't change parameter value
# ifelse(length(clusterset)==1,label=1,label=which.max(totalsse)) #if label has two value
addresult<-function(oldres,newres,i){
  if(is.na(oldres)[1]){  #����1����warning����
    oldres<-newres
    oldres[[2]]<-list(oldres[[2]])
    return (oldres)
  }
  oldres[[1]]<-rbind(oldres[[1]],newres[[1]])  #cluster
  oldres[[2]][[i]]<-newres[[2]]   #center
  oldres[[3]]<-rbind(oldres[[3]],newres[[3]])  #sse
  return (oldres)
}
caltotalsse<-function(x,sse){
  return (sum(sse[x,]))
}

halfkmeans<-function(data,k=5,experimentnum=10){
  if(k>nrow(data)){
    print("half.kmeans'data is too less")
    return 
  }
  clusterset=1
  totalres<-rep(1,nrow(data))
  totalsse<-NA
  totalcenter<-data.frame()
  knum<-2
  k=k-1 #����k-1��ѭ�����ѵõ�k���� 
  while(k>0){
    k<-k-1  
    if(length(clusterset)==1) label<-1 else label<-which.max(totalsse)  #label ָ������Сsse������
    if(length(clusterset)==1) usedata<-data 
     else usedata<-data[which(totalres==clusterset[label]),] #��Сsse��Ӧ������clusterset[label]
    
    totalres[totalres==clusterset[label]]<-NA
    totalsse<-totalsse[-label]
    totalcenter<-totalcenter[-label,]
    clusterset<-clusterset[-label]    #delete the old cluster  #�����ı��
    oldres<-NA
    for (i in 1:experimentnum){
      oldres<-addresult(oldres,kmeans(usedata,2),i)
    }
    sse<-sapply(1:experimentnum,caltotalsse,oldres[[3]])
    label<-which.min(sse)   #���ʵ����SSE��С������    #oldres��������
    cluster<-oldres[[1]][label,]
    center<-oldres[[2]][[label]]
    sse<-oldres[[3]][label,]
    
    #�����صĴظ����µı��
    newcluster<-cluster
    newcluster[cluster==1]<-knum
    newcluster[cluster==2]<-knum+1
    cluster<-newcluster
    
    clusterset<-c(clusterset,knum,knum+1)   
    knum<-knum+2
    totalres[which(rownames(data) %in% rownames(usedata))]<-cluster
    totalcenter<-rbind(totalcenter,center)
    totalsse<-c(totalsse,sse)
    # print(list(clusterset,table(totalres),totalcenter,totalsse))
  }
  newtotalres<-totalres
  newclusterset<-clusterset
  for(i in 1:length(clusterset)){
    newtotalres[totalres==clusterset[i]]<-i
    newclusterset[i]<-i
  }
  totalres<-newtotalres
  clusterset<-newclusterset
  return (list(cluster=totalres,center=totalcenter,SSE=totalsse))
}
halfkmeans_result<-halfkmeans(data0)   #���ֹ���һ���صĸ���Ϊ1�����Ǵص�sse��Ϊ0����


