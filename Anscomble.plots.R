#install.packages("quartets")
#require("quartets")
x.all=anscombe_quartet$x; x.max = max(x.all); x.min = min(x.all)
y.all=anscombe_quartet$y; y.max = max(y.all); y.min = min(y.all)
N = 4; n = 11
these.models = vector(mode='list', length=N)
for (ii in 1:N){
  these.pts = n*(ii-1)+1:n
  x = x.all[these.pts]; y = y.all[these.pts]
  plot(x,y,xlim=c(x.min,x.max),ylim=c(y.min,y.max),
       main = paste("Quartet",ii))
  these.models[[ii]] = lm(y~x)
  abline(these.models[[ii]], col=2,lwd=2)
}