`lbi.test` <-
function(Y,alpha=0.05,critical.value=NA,Nsim=1000)
{

  if (nrow(Y)>ncol(Y)) Y<-t(Y)
  if (is.na(critical.value)) critical.value<-critical.values(nrow(Y),ncol(Y),Nsim,alpha)$t2

  a<-nrow(Y)
  b<-ncol(Y)
  p<-a-1
  q<-b-1

  R<-Y-rep(apply(Y,1,mean),b)-rep(apply(Y,2,mean),each=a)+rep(mean(Y),a*b)
  S<-R %*% t(R)
  vl.cisla<-eigen(S / sum(diag(S)),only.values = TRUE)$values

  if (sum(vl.cisla^2)>critical.value)  out<-list(result=TRUE,stat=sum(vl.cisla^2),critical.value=critical.value,alpha=alpha,name="Locally Best Invariant test") # zamitame aditivitu
                             else out<-list(result=FALSE,stat=sum(vl.cisla^2),critical.value=critical.value,alpha=alpha,name="Locally Best Invariant test") # nezamitame aditivitu

  class(out)<-"aTest"
  return(out)
}

