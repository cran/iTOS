gconv<-function(g1,g2){
  #convolution of probability generating functions g1 and g2
  o<-stats::convolve(g1,rev(g2),type="open")
  o<-pmin(1,pmax(0,o))
  names(o)<-(0:(length(o)-1))
  o
}
