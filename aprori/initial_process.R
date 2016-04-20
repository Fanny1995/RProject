
#�ַ������Ͷ�����csv��Ĭ����factor���ͣ�ע�����ˮƽ
#x<-c(2,3,NA) x==NA return NA,NA,NA error
# [1] "STUD_NO"                            "GENDER"                            
# [3] "TO_CHAR.SI.BIRTHDATE..YYYY.MM.DD.." "CLASS_NO"                          
# [5] "COUR_NAME"                          "EXT_COURSE_NO"                     
# [7] "FIRST_TERM_ID"                      "FIRST_SCORE"                       
# [9] "FIRST_GPOINT"                       "TYPE5"                             
# [11] "CREDIT
setwd("C:/Users/fresh guoguo/Desktop/aprori")
hdata<-read.csv("2010.csv")
hdata$COUR_NAME<-as.character(hdata$COUR_NAME)
hdata$EXT_COURSE_NO<-as.character(hdata$EXT_COURSE_NO)
idx<-grep("^5310",hdata$STUD_NO)  #���ȰѲ���10����ɾ��
hdata<-hdata[idx,]

temp<-table(hdata$STUD_NO)   #delete those may drop out 10 grade
idxname<-names(temp)[temp<0.75*mean(temp)]
hdata<-hdata[-which(hdata$STUD_NO %in% idxname),]

temp<-table(hdata$EXT_COURSE_NO) #���ڿγ���������character����ʣ��������ĳЩˮƽΪ0
temp<-temp[order(temp)]          #���Կ����ܶ�γ�ֻ�к��ٵ�ѧ���ϣ����Ƕ��ڹ�������������û�б�Ҫɾ��
quantile(temp)

#consist ��������Ϊ�գ���content[option]����һ���Դ�����
#����content<-c("EXT_COURSE_NO","COUR_NAME") option=1�����γ�������ͬʱ��id��û�в�ͬ������в�ͬ�ģ�һ���Դ���
consist<-function(hdata,content,option){
  tempdata<-unique(hdata[content])
  t<-table(tempdata[,content[-option]])
  t<-t[t>=2]
  if(length(t)!=0){
    cat(content[option],"��ͬ, ",content[-option],"��ͬ\n")
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
option<-c(1,2,2,2) #��Ҫ����������һ�в�һ��
for(i in 1:length(option)){
  hdata<-consist(hdata,content[i,],option[i])
  tempdata<-unique(data.frame(hdata[content[i,]]))
  if(nrow(tempdata)!=nrow(unique(tempdata[content[i,-option[i]]])))   #���ڶ���data.frame����nrow
    print("error")
}

library("reshape")
func<-function(x) return (x[1])
hdata$FIRST_SCORE<-as.character(hdata$FIRST_SCORE)
newdata<-cast(hdata,STUD_NO~EXT_COURSE_NO,func,value="FIRST_SCORE")  #newdata ����factor�͵�
rownames(newdata)<-newdata[,1]  #������һ��ȥ��
newdata<-newdata[,-1] 
write.csv(newdata,file="10.csv")  #���Է�����Щ�γ̳ɼ�����ѧ����ҵָ�����Ƿ���������Ͷ��е�
#�γ̵ĳɼ������࣬NA����ֵ�������δ����Ⱥ�����ֵ���ֺ�������͵������Ǹ��ѵ�
#x<-factor(c(23,"����"))  x==23 | x=="����"
#������Ƕ�ÿһ�ſγ̣�����ֵ�͵ĳɼ���ȡ������Ȼ��������ת����������
numtocategory<-function(x){ #length(dataframe) equals how many columns it has
  #��������xҪ����vector����
  idxna<-which(is.na(x))
  idxcha<-which(x %in% c("����","����","�е�","����","������"))
  idxnum<-setdiff(seq(1,length(x),by=1),c(idxna,idxcha))
  #x[c()]<-1 is ok, nothing to be changed
  if(length(idxnum)==0){#û����ֵ�ͳɼ���ֱ��ԭ�ⲻ������ȥ
    return (x)
  }
  else{
    temp<-as.numeric(x[idxnum])
    len<-length(temp)
    if(len<5){
      temp[temp>=60]<-"����"
      temp[temp<60]<-"������"
      x[idxnum]<-temp
      return (x)
    }
    idx<-order(temp,decreasing = TRUE)
    temp<-temp[idx]
    temp[1:round(0.2*len)]<-"����"
    temp[(round(0.2*len)+1):round(0.4*len)]<-"����"  #�������Ե�
    temp[(round(0.4*len)+1):round(0.6*len)]<-"�е�"
    temp[(round(0.6*len)+1):round(0.8*len)]<-"����"
    temp[(round(0.8*len)+1):len]<-"������"
    x[idxnum][idx]<-temp
    return (x)
  }
}
for(i in 1:length(newdata)){
  newdata[,i]<-as.character(newdata[,i])   #FIRST_SCORE ���ַ������������ʺ�����
  newdata[,i]<-numtocategory(newdata[,i])
}
write.csv(newdata,"11.csv")

myfunc<-function(x){  #��apply�������Ĳ���x����name���ԣ�xֻ��һ������
  label<-c("����","����","�е�","����","������")
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
names(cour_no_name)<-temp[,"cour_no"]     #cour_no_name����ֱ�Ӹ��ݿγ̱�Ų��ҿγ�����

tt<-apply(newdata,1,myfunc)    #������apply������һЩ
names(tt)<-rownames(newdata)

temp<-unique(data.frame(cour_no=hdata$EXT_COURSE_NO,term=hdata$FIRST_TERM_ID))
cour_no_term<-as.character(temp[,"term"])
names(cour_no_term)<-temp[,"cour_no"]

tt_term<-as.vector(sapply(colnames(newdata),function(x){
  return (rep(cour_no_term[x],5))
}))

tt_num_meaning<-as.vector(sapply(cour_no_name[colnames(newdata)],function(x){
  label<-c("����","����","�е�","����","������")
  return (paste(x,label))
}))


