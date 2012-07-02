print.aTest<-function(x,...)
{
cat(paste('\n',x$name,' on ',x$alpha*100,'% alpha-level:\n\n',sep=""));
cat(paste('Test statistic:',x$stat,"\n"));
cat(paste('Critival value:',x$critical.value,"\n"));
if (x$result) cat("The additivity hypothesis was rejected.\n\n")
  else cat("The additivity hypothesis cannot be rejected.\n\n")
}
