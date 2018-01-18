class_dice<-function(data)
{
  avg<-data[,"Average"]
  avg_sum<-sum(avg)/n
  n<-nrow(data)
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