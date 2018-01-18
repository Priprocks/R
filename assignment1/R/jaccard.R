jaccard<-function(data)
{
  normalize<-function(data)
  {
    return ((data-min(data))/(max(data)-min(data)))
  }
  
  jaccardfor2<-function(t1,t2)
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
    
    ans<-sum1/(s1+s2-sum1)
    return(ans)
  }
  
  jaccard_function<-function(data)
  {
    n<-nrow(data)
    ans<-matrix(data=NA,nrow=n,ncol=n)
    for (i in 1:n) {
      for(j in 1:n)
      {
        ans[i,j]<-jaccardfor2(data[i,],data[j,])
      }
    }
    return(ans)
  }
  
  avgjaccard<-function(data,table)
  {
    n<-nrow(data)
    Average<-vector(mode = "numeric",length = n)
    for(i in 1:n)
      Average[i]=sum(data[i])/n
    table<-cbind(table,Average)
    return(table)
  }
  
  class_jaccard<-function(data)
  {
    avg<-data[,"Average"]
    n<-nrow(data)
    avg_sum<-sum(avg)/n
    Class<-vector(mode="numeric",length=n)
    for(i in 1:n)
    {
      if(avg[i]>=avg_sum)
        Class[i]=1
      else
        Class[i]=0
    }
    data<-cbind(data,Class)
    return(data)
  }
  
  data<-normalize(data)
  matrix<-jaccard_function(data)
  data<-avgjaccard(matrix,data)
  data<-class_jaccard(data)
  return(data)
}