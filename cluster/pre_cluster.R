
#53100907	M	1991/6/21	531009	���������	ab07153053	123	80	3	4160	3
#################grammer error 
# func<-function(x){
#   newdata<-data[which(data$EXT_COURSE_NO==x),]
#   t<-table(newdata$CREDIT)
#   return (names(t)[which.max(t)])
# }
# creditmany<-apply(as.matrix(names(n1)),1,func(x),data) 


#######################
#�õ�ʲô���͵����ݣ�
#1.���޿Σ�ѡ�޿Σ���ѡ�Σ�Уѡ�޿Σ�������
#2.�������ݣ����ֵ���ּ����רҵ��������רҵ
#3.���տ�ֵ����������Ϊ�����רҵ��������רҵ
########################

#TYPE5 5�����͵Ŀ�
#"reshape" package   melt and cast
# [1] "STUD_NO"                            "GENDER"                            
# [3] "TO_CHAR.SI.BIRTHDATE..YYYY.MM.DD.." "CLASS_NO"                          
# [5] "COUR_NAME"                          "EXT_COURSE_NO"                     
# [7] "FIRST_TERM_ID"                      "FIRST_SCORE"                       
# [9] "FIRST_GPOINT"                       "TYPE5"                             
# [11] "CREDIT"   

#class(data$STUD_NO)   integer
#class(data[,1])   interger
#class(data[1])   data.frame


pre_cluster<-function(directory="C:/Users/fresh guoguo/OneDrive/Documents/R/RProject/cj",filename="2010.csv",grade="5310"){
  olddir<-setwd(directory)
  data<-read.csv(filename)  #������������ͬ
  data<-data[grep(paste("^",grade,sep=""),data$STUD_NO),] #delete not 2010 grade
  t<-as.matrix(table(data$STUD_NO)) #rownames(t)
  t<-rownames(t)[(t<0.75*mean(t))]  #delete those drop the grade
  data<-data[-which(data$STUD_NO %in% t),]
  with(data,{
    data[which(data$FIRST_SCORE=="������"),"FIRST_SCORE"]<<-30
    data[which(data$FIRST_SCORE=="����"),"FIRST_SCORE"]<<-60
    data[which(data$FIRST_SCORE=="�е�"),"FIRST_SCORE"]<<-70
    data[which(data$FIRST_SCORE=="����"),"FIRST_SCORE"]<<-80
    data[which(data$FIRST_SCORE=="����"),"FIRST_SCORE"]<<-90  #how to update the factor's value
  })
  data$FIRST_SCORE<-as.numeric(as.character(data$FIRST_SCORE)) #wrong
  data$EXT_COURSE_NO<-as.character(data$EXT_COURSE_NO)
  ##### ��Щ�γ�id�в�ͬ��ѧ�֣�ѧ�ڣ�����Ҫͳһѧ�֣�ѧ��
  
  consist<-function(hdata,content,option){#
    tempdata<-unique(hdata[content])
    t<-table(tempdata[,-option])
    name<-names(t)[t>=2]  #��sapply�������޸��ⲿ����ֵû����
    if(length(name)!=0){
      cat(content[option],"��ͬ",content[-option],"��ͬ\n")
      for(i in name){
        idx<-which(hdata[content[-option]]==i)
        temp<-hdata[idx,]
        t<-table(temp[,content[option]])
        hdata[idx,][,content[option]]<-names(t)[which.max(t)]
      }
    }
    tempdata<-unique(hdata[content])
    if(nrow(tempdata)!=nrow(unique(tempdata[content[-option]]))){
      print("error consist function")
    }
    return (hdata)
  }
  content<-matrix(c("EXT_COURSE_NO","COUR_NAME",
                    "EXT_COURSE_NO","COUR_NAME",
                    "EXT_COURSE_NO","FIRST_TERM_ID",
                    "EXT_COURSE_NO","CREDIT",
                    "EXT_COURSE_NO","TYPE5"),nrow=5,ncol=2,byrow=TRUE)
  option<-c(1,2,2,2,2)
  for(i in 1:length(option)){
    data<-consist(data,content[i,],option[i])
  }
  
  ##########data$FIRST_SCORE no NA value
  #����ʽ���� �� ��Ӧ�γ̵�ѧ��
  #4160 4161 4162 4163 4164 
  library("reshape")
  data$EXT_COURSE_NO<-as.character(data$EXT_COURSE_NO)
  #�����uniqueҪ��һһӳ��
  totalcredit<-unique(data.frame(cour=data$EXT_COURSE_NO,credit=data$CREDIT))
  totalterm<-unique(data.frame(cour=data$EXT_COURSE_NO,term=data$FIRST_TERM_ID)) #term problem ??
  totaltype<-unique(data.frame(cour=data$EXT_COURSE_NO,type=data$TYPE5))
  cc<-c("STUD_NO","EXT_COURSE_NO","FIRST_SCORE")
  func<-function(x) return (x[1])
  with(data,{
    longdata<<-data[cc]
    longdata<<-cast(data,STUD_NO~EXT_COURSE_NO,func,value="FIRST_SCORE")
  })
  rownames(longdata)<-longdata$STUD_NO
  longdata<-longdata[,-1]   #credit,term,type�ǿγ̵�����
  
  funccredit<-function(x,totalcredit){
    return (totalcredit[which(totalcredit$cour==x),]$credit)
  }
  longcredit<-sapply(colnames(longdata),funccredit,totalcredit)
  
  functerm<-function(x,totalterm){
    return (totalterm[which(totalterm$cour==x),]$term)
  }
  longterm<-sapply(colnames(longdata),functerm,totalterm)
  
  functype<-function(x,totaltype){
    return (totaltype[which(totaltype$cour==x),]$type)
  }
  longtype<-sapply(colnames(longdata),functype,totaltype)
  
  # test<-c()
  # for(i in colnames(longdata)){
  #   test<-c(test,data[which(data$EXT_COURSE_NO==i),]$TYPE5[1])
  # }
  write.csv(longdata,"havena10.csv")
  #���������еĿ�ֵ
  nareplacebymean<-function(data){
    cname<-colnames(data)
    rname<-rownames(data)
    data<-as.data.frame(apply(data,2,function(x){
      x[is.na(x)]<-mean(x,na.rm=TRUE)
      return (x)
    }))
    colnames(data)<-cname
    rownames(data)<-rname
    return (data)
  }
  nareplacebyzero<-function(data){
    cname<-colnames(data)
    rname<-rownames(data)  
    data<-as.data.frame(apply(data,2,function(x){  #apply return a matrix
      x[is.na(x)]<-0
      return (x)
    }))
    colnames(data)<-cname
    rownames(data)<-rname
    return (data)
  }
  #newlongdata<-nareplacebyzero(longdata)
  longdata<-nareplacebymean(longdata)
  write.csv(longdata,"nona10.csv")
  
  data0<-longdata[,which(longtype=="4160")]   #dim(data0) 429 65
  credit0<-longcredit[which(longtype=="4160")]
  
  data1<-longdata[,which(longtype=="4161")] #dim(data1) 429 30
  credit1<-longcredit[which(longtype=="4161")]
  
  data2<-longdata[,which(longtype=="4162")]  #dim(data2) 429 14
  credit2<-longcredit[which(longtype=="4162")]
  
  data3<-longdata[,which(longtype=="4163")]   #dim(data3)  429 16
  credit3<-longcredit[which(longtype=="4163")]
  
  data4<-longdata[,which(longtype=="4164")]   #dim(data4) 429 16
  credit4<-longcredit[which(longtype=="4164")]
  
  setwd(olddir)
  return (list(longdata=longdata,longcredit=longcredit,longtype=longtype,longterm=longterm))
}

directory="C:/Users/fresh guoguo/OneDrive/Documents/R/RProject/cj"
filename="2009.csv"
grade="5309"
res<-pre_cluster(directory,filename,grade)  














# 
# ##########################
# newdata<-data[cc]
# newdata<-cast(newdata,STUD_NO~EXT_COURSE_NO,func,value="FIRST_SCORE")
# write.csv(newdata,file="10.csv")    #�������ݽ����ȷ��
# dim(newdata)    #429 265-1 
# nanum<-apply(newdata,2,function(x){
#   return (sum(is.na(x)))
# })
# nanum<-sort(nanum)     #find an evident bundary
# plot(1:length(nanum),nanum)#רҵ�κ�ѡ�޿Σ��������ͼ����ѧ���Ĳ����ͼ�п�������
# plot(density(nanum))
# nat<-table(nanum) #�ҵ����ԵĿ�ֵ�ֽ��,����Ϊ��������
# lessnacour<-names(nanum)[nanum<=17]  #17 ��Ϊ���Ƶ����֣���������
# #�Ժ����������޸��ں����˳��Ժ�û���κ�����
# #�Խ��ٵĿ�ֵ�γ��ÿ�ֵ���
# newdata[,lessnacour]<-apply(newdata[,lessnacour],2,function(x){
#   x[!complete.cases(x)]<-mean(x,na.rm=TRUE) #na.rm=TRUE
#   return (x)
# })
# #���Կ�ֵ�Ƿ�ȡ���� newdata[,lessnacour][!complete.cases(newdata[,lessnacour]),]
# 
# 
# 
# 


# t<-unique(data.frame(cour=as.character(data$EXT_COURSE_NO),credit=as.numeric(data$CREDIT)))
# dim(t)   # 266 2 ��������
# tt<-unique(as.character(data$EXT_COURSE_NO))  #length 264  
# t1<-table(t[1])
# n1<-t1[t1>=2]     #���t[t$cour==names(n1),]
# creditmany<-c()   
# summary(data)
# for(i in names(n1)){
#   newdata<-data[data$EXT_COURSE_NO==i,]
#   t<-table(newdata$CREDIT)
#   data[data$EXT_COURSE_NO==i,]$CREDIT<-as.numeric(names(t)[which.max(t)])
#   creditmany<-c(creditmany,names(t)[which.max(t)])
# }      ##2010 right 264
# #�γ�һ�£�ѧ�ڲ�һ�£�������ѧ�����
# t<-unique(data.frame(cour=data$EXT_COURSE_NO,term=data$FIRST_TERM_ID))
# tt<-table(t$cour)
# n1<-names(tt)[tt>1]
# termmany<-c()
# for(i in names(n1)){
#   newdata<-data[data$EXT_COURSE_NO==i,]
#   t<-table(newdata$FIRST_TERM_ID)
#   data[data$EXT_COURSE_NO==i,]$FIRST_TERM_ID<-as.numeric(names(t)[which.max(t)])
#   termany<-c(termany,names(t)[which.max(t)])
# }
# #�γ�һ�£����಻һ�£��������������
# t<-unique(data.frame(cour=data$EXT_COURSE_NO,type=data$TYPE5))
# tt<-table(t$cour)
# n1<-names(tt)[tt>1]
# typemany<-c()
# for(i in n1){
#   newdata<-data[data$EXT_COURSE_NO==i,]
#   t<-table(newdata$TYPE5)
#   data[data$EXT_COURSE_NO==i,]$TYPE5<-as.numeric(names(t)[which.max(t)])
#   typemany<-c(typemany,names(t)[which.max(t)])
# }