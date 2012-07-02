`johnson.graybill.test` <-
function(Y,alpha=0.05,critical.value=NA,Nsim=1000)
{

  if (nrow(Y)>ncol(Y)) Y<-t(Y)
  if (is.na(critical.value)) critical.value<-critical.values(nrow(Y),ncol(Y),Nsim,alpha)$t1

  a<-nrow(Y)
  b<-ncol(Y)
  p<-a-1
  q<-b-1

  R<-Y-rep(apply(Y,1,mean),b)-rep(apply(Y,2,mean),each=a)+rep(mean(Y),a*b)
  S<-R %*% t(R)
  vl.cisla<-eigen(S / sum(diag(S)),only.values = TRUE)$values

  if (vl.cisla[1]>critical.value) out<-list(result=TRUE,stat=vl.cisla[1],critical.value=critical.value,alpha=alpha,name="Johnson and Graybill test") # zamitame aditivitu
                             else out<-list(result=FALSE,stat=vl.cisla[1],critical.value=critical.value,alpha=alpha,name="Johnson and Graybill test")

  class(out)<-"aTest"
  return(out)

}

