data <- read.csv("G:\\iris.csv", header = FALSE)
nc<-ncol(data)
nr<-nrow(data)
k<-length(unique(sort(data[,nc]))) 

testData<-read.csv("G:\\testData.csv", header = FALSE)
ncc<-ncol(testData)
nrr<-nrow(testData)
mt<-matrix(nrow = nrr,ncol = 101)
merr<-matrix(nrow = 100,ncol = 1)
mtk=0;
# 
for (i in 1:33) 
{
  
  tempTrain<-data
  t<-sample(1: nr,nr, replace = TRUE, prob = NULL)
  for (i in 1:nr) {
    tempTrain[1,]<-data[t[i],]
  }
  tempTrain[,nc]<-as.factor(tempTrain[,nc]) #cover factor
  
  t2<-sort(unique(t))
  
  t3<-(1:nr)
  for (i in 1:nr) {
    t3[t2[i]]<-0
  }
  t3<-sort(unique(t3))
  t3[1]<-t3[2]
  t3<-sort(unique(t3))
  
  tempTest<-data[1:length(t3),]
  for (i in 1:length(t3)) {
    tempTest[1,]<-data[t3[i],]
  }
  #knn
  #install.packages(class)
  library(class)
  mKnn <- knn(tempTrain[,1:(nc-1)],tempTest[,1:(nc-1)],tempTrain[,nc],k)
  #mKnnt <- knn(tempTrain[,1:(nc-1)],testData[,1:(ncc)],tempTrain[,nc],k)
  mtk<-(mtk+1)
  mt[,mtk] <- knn(tempTrain[,1:(nc-1)],testData[,1:(ncc-1)],tempTrain[,nc],k)
  #
  #mKnnErr<-sum((as.numeric(mNbp)-as.numeric(tempTest[,nc])))/length(mNbp)
  merr[mtk]<-sum((as.numeric(mNbp)-as.numeric(tempTest[,nc])))/length(mNbp)
}


for (i in 1:33) 
{
  tempTrain<-data
  t<-sample(1: nr,nr, replace = TRUE, prob = NULL)
  for (i in 1:nr) {
    tempTrain[1,]<-data[t[i],]
  }
  tempTrain[,nc]<-as.factor(tempTrain[,nc]) #cover factor
  
  t2<-sort(unique(t))
  
  t3<-(1:nr)
  for (i in 1:nr) {
    t3[t2[i]]<-0
  }
  t3<-sort(unique(t3))
  t3[1]<-t3[2]
  t3<-sort(unique(t3))
  
  tempTest<-data[1:length(t3),]
  for (i in 1:length(t3)) {
    tempTest[1,]<-data[t3[i],]
  }
  
  #c5.0
  #install.packages(C50)
  library(C50)
  mC50 <- C5.0(tempTrain[,1:(nc-1)],tempTrain[,nc],trials=1,costs=NULL)
  pC50 <- predict(mC50, tempTest[1:(nc-1)], type = "class")
  #pC50t <- predict(mC50, testData[1:(ncc)], type = "class")
  mtk<-(mtk+1)
  mt[,mtk]<- predict(mC50, testData[1:(ncc-1)], type = "class")
  #pC50Err <- sum((as.numeric(pC50)-as.numeric(tempTest[,nc])))/length(pC50)
  merr[mtk]<-sum((as.numeric(pC50)-as.numeric(tempTest[,nc])))/length(pC50)
}


for (i in 1:34) 
{
  tempTrain<-data
  t<-sample(1: nr,nr, replace = TRUE, prob = NULL)
  for (i in 1:nr) {
    tempTrain[1,]<-data[t[i],]
  }
  tempTrain[,nc]<-as.factor(tempTrain[,nc]) #cover factor
  
  t2<-sort(unique(t))
  
  t3<-(1:nr)
  for (i in 1:nr) {
    t3[t2[i]]<-0
  }
  t3<-sort(unique(t3))
  t3[1]<-t3[2]
  t3<-sort(unique(t3))
  
  tempTest<-data[1:length(t3),]
  for (i in 1:length(t3)) {
    tempTest[1,]<-data[t3[i],]
  }
  
  #naiveBayes
  #install.packages(e1071)
  library(e1071)
  mNb <- naiveBayes(tempTrain[,1:(nc-1)],tempTrain[,nc],laplace=0)
  mNbp<-predict(mNb, tempTest[,1:(nc-1)])
  #mNbp<-predict(mNb, testData[,1:(ncc)])
  mtk<-(mtk+1)
  mt[,mtk]<-predict(mNb, testData[,1:(ncc-1)])
  #
  #mNbpErr<-sum((as.numeric(mNbp)-as.numeric(tempTest[,nc])))/length(mNbp)
  merr[mtk]<-sum((as.numeric(mNbp)-as.numeric(tempTest[,nc])))/length(mNbp)
}

for (i in 1:nrr) {
  mt[i,101]<-as.numeric(names(table(mt[i,1:100])))[which.max(table(mt[i,1:100]))]
}

variety<-matrix(nrow = 100,ncol = 100)
#variety
for (i in 1:100) {
  for (j in i:100) {
    a<-0
    b<-0
    c<-0
    d<-0
    for (k in 1:nrr) {
      if ((mt[k,i]==testData[k,ncc])&&(mt[k,j]==testData[k,ncc])) {
        a<-a+1
      }else if((mt[k,i]==testData[k,ncc])&&(mt[k,j]!=testData[k,ncc])){
        b<-b+1
      }else if((mt[k,i]!=testData[k,ncc])&&(mt[k,j]==testData[k,ncc])){
        c<-c+1
      }else
      {
        d<-d+1
      }
    }
    variety[i,j]<-((b+c)/(a+d+b+c))
  }
}
