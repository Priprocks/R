sim_matrixd<-function(data)
{
  normalize<-function(data)
  {
    return ((data-min(data))/(max(data)-min(data)))
  }
  
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
  
  dice_function<-function(data)
  {
    n<-nrow(data)
    ans<-matrix(data=NA,nrow=n,ncol=n)
    for (i in 1:n) {
      for(j in 1:n)
      {
        ans[i,j]<-dicefor2(data[i,],data[j,])
      }
    }
    return(ans)
  }
  
  avgdice<-function(data,table)
  {
    n<-nrow(data)
    Average<-vector(mode = "numeric",length = n)
    for(i in 1:n)
      Average[i]=sum(data[i])/n
    table<-cbind(table,Average)
    return(table)
  }
  data<-normalize(data)
  matrix<-dice_function(data)
  n<-nrow(data)
  for(i in 1:n)
  {
    avg<-sum(matrix[i,])/n
    for(j in 1:n)
    {
      if(matrix[i,j]>=avg)
        matrix[i,j]<-1
      else
        matrix[i,j]<-0
    }
  }
  return(matrix)
}