mtukey.test<-function(Y,alpha=0.05,correction=0,Nboot=1000)
{

p.test<-function(data)
{
  #dimension of data
  n.a<-nrow(data)
  n.b<-ncol(data)  

  #effects in submodel
  mu<-mean(data)
  r.effect<-apply(data,1,mean)-mu
  c.effect<-apply(data,2,mean)-mu
  
  #first estimate of k
  exp<-matrix(mu+rep(r.effect,n.b)+rep(c.effect,each=n.a),n.a,n.b)
  err<-data-exp
  k<-sum(cbind(r.effect)%*%rbind(c.effect)*err)/sum(cbind(r.effect)^2%*%rbind(c.effect)^2)

  #reestimation
  y<-data-mu-rep(c.effect,each=n.a)
  x<-matrix(1+k*rep(c.effect,each=n.a),n.a,n.b)
  a.new<-apply(x*y,1,sum)/apply(x^2,1,sum)

  y<-data-mu-rep(r.effect,n.b)
  x<-matrix(1+k*rep(r.effect,n.b),n.a,n.b)
  b.new<-apply(x*y,2,sum)/apply(x^2,2,sum)

  exp2<-matrix(mu+rep(a.new,n.b)+rep(b.new,each=n.a),n.a,n.b) 
  err2<-data-exp2
  k.new<-sum(cbind(a.new)%*%rbind(b.new)*err2)/sum(cbind(a.new)^2%*%rbind(b.new)^2)

  #compute statistics
  exp3<-matrix(mu+rep(a.new,n.b)+rep(b.new,each=n.a),n.a,n.b) + k.new*cbind(a.new)%*%rbind(b.new)
  err3<-data-exp3

  rss0<-sum(err^2)
  rss<-sum(err3^2)
  s<-rss/(n.a*n.b-n.a-n.b)
  loglik.ratio<-rss0/rss
  stat<-(loglik.ratio-1)*(n.a*n.b-n.a-n.b)/qf(1-alpha,1,n.a*n.b-n.a-n.b)

  return(c(stat,rss,s,loglik.ratio,abs(k.new)))
}    

  #dimension of data
  n.a<-nrow(Y)
  n.b<-ncol(Y)  

  #effects in submodel
  mu<-mean(Y)
  r.effect<-apply(Y,1,mean)-mu
  c.effect<-apply(Y,2,mean)-mu
  
  #first estimate of k
  exp<-matrix(mu+rep(r.effect,n.b)+rep(c.effect,each=n.a),n.a,n.b)
  err<-Y-exp
  
  ppp<-p.test(Y)

  if (correction==0) out<-list(result=ppp[1]>1,stat=ppp[1],critical.value=1,alpha=alpha,name="Modified Tukey test")

  if (correction==1)
  {

     boot<-replicate(Nboot,p.test(exp+sample(err,n.a*n.b,rep=FALSE)))
     B<-apply(boot,1,quantile,probs=1-alpha)

     out<-list(result=ppp[1]>B[1],stat=ppp[1],critical.value=B[1],alpha=alpha,name="Modified Tukey test (small sample size correction, type 1)")
  }

  if (correction==2)
  {
    boot<-replicate(Nboot,p.test(exp+rnorm(n.a*n.b,sd=sqrt(ppp[3]))))
    B<-apply(boot,1,quantile,probs=1-alpha)
    out<-list(result=ppp[1]>B[1],stat=ppp[1],critical.value=B[1],alpha=alpha,name="Modified Tukey test (small sample size correction, type 2)")  
  }
 
  class(out)<-"aTest"
  return(out)
}
