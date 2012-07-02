`tukey.test` <-
function(data,alpha=0.05,critical.value=NA) {
# Mandel test of additivity in two-way ANOVA
# In rows is factor A (fixed factor), in columns factor B (random)

  a<-nrow(data) # number of levels of factor A
  b<-ncol(data) # number of levels of factor B	

  d.f1=1 # df for SS.mandel
  d.f2=(a-1)*(b-1)-1 #df for SS.resid
  if (is.na(critical.value)) critical.value=qf(1-alpha,df1=d.f1,df2=d.f2)

  yMEAN<-mean(data) # grand mean
  A.hat<-apply(data,1,mean)-yMEAN # deviations of the row means from the grand mean
  B.hat<-apply(data,2,mean)-yMEAN # deviations of the row means from the grand mean
  ss.col<-a*sum(B.hat^2) # SS of columns	
  ss.row<-b*sum(A.hat^2) # SS of rows
  ss.tukey<-((sum(A.hat*(data%*%B.hat)/(ss.col/a)))^2)/(ss.row/b)*(ss.col/a) 

  test.stat=ss.tukey*((a-1)*(b-1)-1)/(sum(apply(data^2,1,sum))-a*b*yMEAN^2-ss.row-ss.col-ss.tukey) # The F statistic
 out<-list(result=test.stat>critical.value,stat=test.stat,critical.value=critical.value,alpha=alpha,name="Tukey test") # nezamitame aditivitu

  class(out)<-"aTest"
  return(out)
}

