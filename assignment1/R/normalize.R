normalize<-function(data)
{
  return ((data-min(data))/(max(data)-min(data)))
}
