intersection<-function(a,b)
{
  n<-length(a)
  count<-0
  for(i in 1:n)
  {
    if(a[i]==1 && b[i]==1)
      count<-count+1
  }
  return(count)
}