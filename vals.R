vals<-function(x, fp, anno){
  fp<-fp
  fn<-x
  tp<-anno-fn
  out<-c(fp, fn, tp)
  out
}