

###################################
#dir C:/Users/fresh guoguo/OneDrive/Documents/R/RProject
#C:/Users/fresh guoguo/OneDrive/Documents/R/RProject/cj
#filename "导出2010.csv"
#grade "5310"
#return tt,tt_term,tt_nummeaning
#if the parameter is wrong, we cant't guarantee it right
################################
pre_aprori<-function(dir="C:/Users/fresh guoguo/OneDrive/Documents/R/RProject/cj",filename="2006.csv",grade="5306"){
  setwd(dir)
  hdata<-read.csv(filename)
  hdata$COUR_NAME<-as.character(hdata$COUR_NAME)
  hdata$EXT_COURSE_NO<-as.character(hdata$EXT_COURSE_NO)
  idx<-grep(paste("^",grade,sep=""),hdata$STUD_NO)  #首先把不是10级的删掉
  hdata<-hdata[idx,]
  
  temp<-table(hdata$STUD_NO)   #delete those may drop out 10 grade
  idxname<-names(temp)[temp<0.75*mean(temp)]
  hdata<-hdata[-which(hdata$STUD_NO %in% idxname),]
  
  temp<-table(hdata$EXT_COURSE_NO) #对于课程来讲，用character最合适，不会出现某些水平为0
  temp<-temp[order(temp)]          #可以看到很多课程只有很少的学生上，但是对于关联分析来讲，没有必要删掉
  quantile(temp)
  
  #consist 函数返回为空，对content[option]进行一致性处理。
  #比如content<-c("EXT_COURSE_NO","COUR_NAME") option=1，检查课程名字相同时，id有没有不同，如果有不同的，一致性处理
  consist<-function(hdata,content,option){
    tempdata<-unique(hdata[content])
    t<-table(tempdata[,content[-option]])
    t<-t[t>=2]
    if(length(t)!=0){
      cat(content[option],"不同, ",content[-option],"相同\n")
      name<-names(t)
      for(i in name){
        idx<-which(hdata[content[-option]]==i)
        newdata<-hdata[idx,][content]
        t<-table(newdata[content[option]])
        # print(t)
        # show(names(t)[which.max(t)])
        hdata[idx,][content[option]]<-names(t)[which.max(t)]
      }
    }
    tempdata<-unique(hdata[content])
    return (hdata)
  }
  content<-matrix(c("EXT_COURSE_NO","COUR_NAME",
                    "EXT_COURSE_NO","COUR_NAME",
                    "EXT_COURSE_NO","FIRST_TERM_ID",
                    "EXT_COURSE_NO","CREDIT"),nrow=4,ncol=2,byrow=TRUE)
  option<-c(1,2,2,2) #看要处理的是那一列不一致
  for(i in 1:length(option)){
    hdata<-consist(hdata,content[i,],option[i])
    tempdata<-unique(data.frame(hdata[content[i,]]))
    if(nrow(tempdata)!=nrow(unique(tempdata[content[i,-option[i]]])))   #由于都是data.frame，用nrow
      print("error")
  }
  
  library("reshape")
  func<-function(x) return (x[1])
  hdata$FIRST_SCORE<-as.character(hdata$FIRST_SCORE)
  newdata<-cast(hdata,STUD_NO~EXT_COURSE_NO,func,value="FIRST_SCORE")  #newdata 都是factor型的
  rownames(newdata)<-newdata[,1]  #名字那一列去掉
  newdata<-newdata[,-1] 
  write.csv(newdata,file="10.csv")  #可以发现有些课程成绩（大学生就业指导）是分数和类别型都有的
  #课程的成绩有三类，NA，数值，类别，如何处理既含有数值型又含有类别型的数据是个难点
  #x<-factor(c(23,"良好"))  x==23 | x=="良好"
  #我想的是对每一门课程，将数值型的成绩抽取出来，然后按照排名转换成类别变量
  numtocategory<-function(x){ #length(dataframe) equals how many columns it has
    #传进来的x要求是vector类型
    idxna<-which(is.na(x))
    idxcha<-which(x %in% c("优秀","良好","中等","及格","不及格"))
    idxnum<-setdiff(seq(1,length(x),by=1),c(idxna,idxcha))
    #x[c()]<-1 is ok, nothing to be changed
    if(length(idxnum)==0){#没有数值型成绩，直接原封不动还回去
      return (x)
    }
    else{
      temp<-as.numeric(x[idxnum])
      len<-length(temp)
      if(len<5){
        temp[temp>=60]<-"优秀"
        temp[temp<60]<-"不及格"
        x[idxnum]<-temp
        return (x)
      }
      idx<-order(temp,decreasing = TRUE)
      temp<-temp[idx]
      temp[1:round(0.2*len)]<-"优秀"
      temp[(round(0.2*len)+1):round(0.4*len)]<-"良好"  #比例可以调
      temp[(round(0.4*len)+1):round(0.6*len)]<-"中等"
      temp[(round(0.6*len)+1):round(0.8*len)]<-"及格"
      temp[(round(0.8*len)+1):len]<-"不及格"
      x[idxnum][idx]<-temp
      return (x)
    }
  }
  for(i in 1:length(newdata)){
    newdata[,i]<-as.character(newdata[,i])   #FIRST_SCORE 以字符串处理，不适合因子
    newdata[,i]<-numtocategory(newdata[,i])
  }
  write.csv(newdata,"11.csv")
  
  myfunc<-function(x){  #用apply传进来的参数x不带name属性，x只是一个向量
    label<-c("优秀","良好","中等","及格","不及格")
    t1<-c()
    for(j in 1:length(x)){
      if(!is.na(x[j])){
        for(i in 1:5){
          if(x[j]==label[i]){
            y<-(j-1)*5+i
            # names(y)<-paste(name[j],label[i])
            t1<-c(t1,y)
          }
        }
      }
    }
    return (t1)
  }
  temp<-unique(data.frame(cour_no=hdata$EXT_COURSE_NO,cour_name=hdata$COUR_NAME))
  cour_no_name<-as.character(temp[,"cour_name"])
  names(cour_no_name)<-temp[,"cour_no"]     #cour_no_name可以直接根据课程编号查找课程名字
  
  tt<-apply(newdata,1,myfunc)    #还是用apply函数快一些
  names(tt)<-rownames(newdata)
  
  temp<-unique(data.frame(cour_no=hdata$EXT_COURSE_NO,term=hdata$FIRST_TERM_ID))
  cour_no_term<-as.character(temp[,"term"])
  names(cour_no_term)<-temp[,"cour_no"]
  
  tt_term<-as.vector(sapply(colnames(newdata),function(x){
    return (rep(cour_no_term[x],5))
  }))
  
  tt_num_meaning<-as.vector(sapply(cour_no_name[colnames(newdata)],function(x){
    label<-c("优秀","良好","中等","及格","不及格")
    return (paste(x,label))
  }))
  return (list(transac=tt,tt_term=tt_term,tt_num_meaning=tt_num_meaning))
}
res<-pre_aprori()