avgdice<-function(data,table)
{
  n<-nrow(data)
  Average<-vector(mode = "numeric",length = n)
  for(i in 1:n)
    Average[i]=sum(data[i])/n
  table<-cbind(table,Average)
  return(table)
}
