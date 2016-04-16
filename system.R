
#this is the whole system for cluster and aprori 

directory<-"C:/Users/fresh guoguo/OneDrive/R/RProject/cj"
filename<-"2010.csv"
grade<-"5310"
system<-function(directory,filename,grade){
  print("preprocess data for aprori")
  aproridir<-"C:/Users/fresh guoguo/OneDrive/R/RProject/aprori"
  setwd(aproridir)
  source("pro_aprori.R")
  source("aprori.R")
  cat("whether you just want to get just ÓÅÐã course aprori anylysis: yes or no\n")
  while(1){
    x<-scan()
    if(x=="yes"){
      selecres="partial"
      break
    }
    else if(x=="no"){
      selectres="all"
      break
    }
    else{
      print("error answer! please input yes or no!")
    }
  }
  pre_aprori_res<-pre_aprori(directory,filename,grade,selectres="all")
  tt<-pre_aprori_res$transac
  term<-pre_aprori_res$tt_term
  cour_name<-pre_aprori_res$tt_num_meaning
  print("perform the aprori algorithm")
  while(1){
    cat("aprori Minsup and Minconf you need to give such as 0.1 0.5:  ")
    x<-scan()
    if(length(x)==2 & is.numeric(x) & x[1]>=0 & x[1]<=1 & x[2]>=0 & x[2]<=1)
      break
    else{
      print("error data")
      next
    }
  }
  Minsup=x[1]
  Minconf=x[2]
  Minsup=0.1
  Minconf=0.5
  rule<-aprori(tt,Minsup,Minconf)
  print("show the result for aprori")
  showallrule(rule,cour_name,term)
  print("aprori analysis end!")
  
  print("Now begin the cluster analysis!")
  clusterdir<-"C:/Users/fresh guoguo/OneDrive/Documents/R/RProject/cluster"
  setwd(clusterdir)
  source("pro_cluster.R")
  source("kmeans.R")
  source("halfkmeans.R")
  source("ghc.R")
  source("dbscan.R")
  print("preprocess data for cluster")
  pre_cluster_res<-pre_cluster(directory,filename,grade)
  longdata<-pre_cluster_res$longdata
  longcredit<-pre_cluster_res$longcredit
  longterm<-pre_cluster_res$longterm
  longtype<-pre_cluster_res$longtype
  
  print("perform the cluster algorithm")
  kmeans_result<-kmeans(longdata)
  halfkmeans_result<-halfkmeans(longdata)
  
  print("show the cluster result")
  ##########
}