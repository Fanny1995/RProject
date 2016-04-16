
tempdata<-read.csv("2010.csv")
tempdata<-tempdata[,-c(1,2,3,8,9)]
tempdata<-unique(tempdata)
totaltype<-unique(data.frame(cour=tempdata$EXT_COURSE_NO,type=tempdata$TYPE5))
t<-table(totaltype$cour)
n<-names(t)[t>1]
tempdata<-unique(tempdata[which(tempdata$EXT_COURSE_NO %in% n),])
unique(tempdata$COUR_NAME)
head(tempdata)
unique(data.frame(tempdata$EXT_COURSE_NO,tempdata$TYPE5))

myfunc<-function(x,data){
  return (data[,1])
}
res<-apply(data0,1,myfunc,data0)
class(res)
#************************************************************

temp=names(table(res)[table(res)<5])
temp
idx<-which(res %in% temp)
idx
data0<-data0[-idx,]

myfunc<-function(x,y){
  return (apply(as.matrix(1:2),1,function(x){  #变量的值是最近定义的
    func(y)     #输出形参的值
    return (c(1))
  }))
}
func<-function(y){
  print(y)
  return ()
}

#############################################
t1<-calcorepoint(res,80,30)
t2<-caledgepoint(res,80,30)
t3<-calnoisepoint(res,80,30)
summary(t1)   
table(t2[1,])
summary(t3)    #sum is 429


###########################################

# 
# rownames(distmatrix)<-seq(1,nrow(distmatrix),by=1)
# colnames(distmatrix)<-seq(1,nrow(distmatrix),by=1)
# 
# coreidx<-calcorepoint(distmatrix,Eps,Minpts)
# edgeidx<-which(caledgepoint(distmatrix,Eps,Minpts)==1)
# edgefor<-sapply(caledgepoint(distmatrix,Eps,Minpts),"[",2)
# noiseidx<-calnoisepoint(distmatrix,Eps,Minpts)
# corenum<-rownames(distmatrix)[coreidx]
# edgenum<-rownames(dismatrix)[edgeidx]
# distmatrix<-distmatrix[-noiseidx,-noiseidx]
# matrix10<-distmatrix[corenum,corenum]  #corematrix
# matrix10[matrix10<Eps]<-1
# matrix10[matrix10>=Eps]<-0
# 
# k<-rep(NA,length(corenum))
# names(k)<-corenum
# kk<-1
# while(sum(is.na(k))){
#   c2<-names(k)[is.na(k)][1]
#   c1<-NULL
#   while(length(c2)){    #if there are two sapply, what's the results like?
#     c1<-c(c1,c2)
#     c3<-NULL
#     for(i in c2){
#       cc<-rownames(matrix10[i,]==1)
#       c3<-c(c3,cc[!(cc %in% c1)])
#     }
#     c2<-c3
#   }
#   k[c1]<-kk
#   kk<-kk+1
# }
# cluster<-rep(NA,nrow(distmatrix))   
# names(cluster)<-rownames(distmatrix)   # use the name skill
# cluster[names(k)]<-k
# cluster[edgenum]<-k[edgefor]

####################################################################
# tt<-list()
# for(i in 1:nrow(newdata)){
#   t1<-c()
#   temp<-newdata[i,]
#   for(j in 1:length(temp)){
#     if(!is.na(temp[j])){
#       if(temp[j]=="优秀")
#         t1<-c(t1,(j-1)*5+1)
#       else if(temp[j]=="良好")
#         t1<-c(t1,(j-1)*5+2)
#       else if(temp[j]=="中等")
#         t1<-c(t1,(j-1)*5+3)
#       else if(temp[j]=="及格")
#         t1<-c(t1,(j-1)*5+4)
#       else if(temp[j]=="不及格")
#         t1<-c(t1,(j-1)*5+5)
#     }
#   }
#   t1<-list(t1)
#   names(t1)<-rownames(temp)
#   tt<-c(tt,t1)
# }


# cour_id_name<-unique(data.frame(cour_no=hdata$EXT_COURSE_NO,cour_name=hdata$COUR_NAME))
# expcourid<-table(cour_id_name$cour_name)  #课程id有问题
# expcourid<-expcourid[expcourid>=2]
# for()
#   findexp<-function(x,data,num){ #要比较的值，num指的是data那一列应该和x比较
#     idx<-which(data[,num]==x)
#     return (data[,-num][idx])
#   }
# if(length(expcourid)!=0){
#   cat("有些课程id相同，但是课程名字不同: \n")
#   res<-sapply(names(expcourid),findexp,cour_id_name,2)  #class(res) is matrix
# }
# if(length(res)!=0){
#   for(i in 1:ncol(res)){
#     idx<-which(hdata$EXT_COURSE_NO %in% res[,i])
#     temp<-hdata[idx,c("EXT_COURSE_NO","COUR_NAME")]
#     t<-table(temp$EXT_COURSE_NO)
#     t<-names(t)[which.max(t)]
#     hdata[idx,"EXT_COURSE_NO"]<-t
#   }
# }
# 
# expcourname<-table(cour_id_name$cour_no)   # 查找课程id相同，但是课程名字不同的,将课程名字改成最多的课程
# expcourname<-expcourname[expcourname>=2]
# if(length(expcourname)!=0){
#   cat("有些课程名字相同，但是课程id不同：")
#   res<-sapply(names(expcourname),findexp,cour_id_name,1)
# }
# if(length(res)!=0){
#   for(i in 1:ncol(res)){
#     idx<-which(hdata$COUR_NAME %in% res[,i])
#     temp<-hdata[idx,c("EXT_COURSE_NO","COUR_NAME")]
#     t<-table(temp$COUR_NAME)
#     t<-names(t)[which.max(t)]
#     hdata[idx,"COUR_NAME"]<-t
#   }
# }
# 
# xx<-c("EXT_COURSE_NO","FIRST_TERM_ID")
# cour_no_term<-unique(data.frame(cour_no=hdata[,"EXT_COURSE_NO"],term=hdata[,"FIRST_TERM_ID"]))
# expcourterm<-table(cour_no_term$cour_no)   #有相当一部分的选修课的term_id是不一样的
# 



