pvaluematrix<-function(x){
    if(nrow(x)<30){
    return("Enter a matrix of column length more than 30")
  }
  
  rowlen<-nrow(x)
  columnlen<-ncol(x)
  matnew<-matrix(NA,ncol = columnlen,nrow = columnlen)
  for (i in 1: (columnlen-1)) {
    for (j in (i+1):columnlen) {
      matnew[i,j]<-t.test(x[,i],x[,j])$p.value
      matnew[j,i]<-matnew[i,j]
    }
   
  }
  print(matnew)
}
##To test 
x<-as.matrix(iris[1:4])
pvaluematrix(x)
