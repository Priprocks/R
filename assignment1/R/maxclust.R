maxclust<-function(s)
{
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
  n<-nrow(s)
  cluster<-matrix(data = 0,nrow = n,ncol = n)
  for(i in 1:n)
  {
    for(j in 1:i)
    {
      if(j<i)
        cluster[i,j]=intersection(s[i,],s[j,])/union(s[i,],s[j,])
    }
  }  
  ans<-which(cluster == max(cluster), arr.ind = TRUE)
  return(ans[1,])
}