simmatrix<-function(s,max)
{
  union<-function(a,b)
  {
    n<-length(a)
    count<-0
    for(i in 1:n)
    {
      if(a[i]==1 || b[i]==1)
        count<-count+1
    }
    return(count)
  }
  subset<-function(a,b)
  {
    n<-length(a)
    res<-TRUE
    for(i in 1:n)
    {
      if(a[i]==0 && b[i]==1)
      {
        res<-FALSE
        return(res)
      }
    }
    return(res)
  }
  n<-nrow(s)
  m<-ncol(s)
  max1<-max["row"]
  max2<-max["col"]
  merge<-vector(mode="numeric",length = m)
  for(i in 1:n)
  {
    if(s[max1,i]==1 || s[max2,i]==1)
      merge[i]<-1
    else
      merge[i]<-0
  }
  s<-s[-c(max1,max2),]
  n<-nrow(s)
  m<-ncol(s)
  vec<-vector(mode="numeric",length = 0L)
  for(i in 1:n)
  {
    if(subset(merge,s[i,]))
      vec<-c(vec,i)
  }
  s<-s[-vec,]
  s<-rbind(s,unname(merge))
  return(s)
}