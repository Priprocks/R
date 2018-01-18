dicefor2<-function(t1,t2)
{
  t1<-as.vector(as.numeric(t1))
  t2<-as.vector(as.numeric(t2))
  n<-length(t1)
  sum1<-0
  s1<-0
  s2<-0
  for (i in 1:n) {
    sum1<-sum1+t1[i]*t2[i]
    s1<-s1+t1[i]*t1[i]
    s2<-s2+t2[i]*t2[i]
  }

  ans<-2*sum1/(s1+s2)
  return(ans)
}
