library(dplyr)
load("coe.Rda")
riskscore<-function(x){
  mm<-mean(x)
  vv<-var(x)*(9/10)
  x1<-(x-mm)/sqrt(vv+1e-5)*w1+b1
  x1<-x1%*%W1+B1
  
  mm<-mean(x1)
  vv<-var(as.numeric(x1))*(31/32)
  x2<-(x1-mm)/sqrt(vv+1e-5)*w2+b2
  x2<-ifelse(1e-2*x2>x2,1e-2*x2,x2)
  x2<-x2%*%W2+B2
  x2<-x2+x1
  
  mm<-mean(x2)
  vv<-var(as.numeric(x2))*(31/32)
  x3<-(x2-mm)/sqrt(vv+1e-5)*w3+b3
  x3<-ifelse(1e-2*x3>x3,1e-2*x3,x3)
  x3<-x3%*%W3+B3
  x3<-x3+x2
  
  mm<-mean(x3)
  vv<-var(as.numeric(x3))*(31/32)
  x4<-(x3-mm)/sqrt(vv+1e-5)*w4+b4
  x4<-ifelse(1e-2*x4>x4,1e-2*x4,x4)
  x4<-x4%*%W4+B4
  y<--x4
  return(y)
}
genes<-c("CYBB" , "FURIN" , "DPP4" , "ETV4" , "RRM2" , "NR4A1", "EPAS1" , "GCLC" ,"TNFAIP3", "AKR1C1" )
#input the expression data of the sample which you want to predict.
E<-c()
rs<-riskscore(E)

