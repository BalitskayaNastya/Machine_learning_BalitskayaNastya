xl = data[c(3,4,5)]
n = dim(xl)[2]-1
l = dim(xl)[1]
colors = c("red", "green", "blue")

# drop a classifier, dataset and other parameters to build
# a nice classification map, an example of a call would be like that:
# classificationmap(function(x) a(x, selected, 0.394, square, dist),
#       dataset, "A map", -1, 1, 50, -1, 1, 50)
classificationmap = function(classifier, xl, title,
                             xfrom=1, xto=7, xticks=50,
                             yfrom=0, yto=3, yticks=50,
                             colors=c("red", "green", "blue", "gray")) {
  plot(xl[,1], xl[,2], col=colors[xl[,3]], pch=19, main=title, xlab = "Feature 3", ylab = "Feature 4",
       xlim=c(xfrom, xto), ylim=c(yfrom,yto))
  xx = seq(from=xfrom, to=xto, (xto - xfrom) / xticks)
  yy = seq(from=yfrom, to=yto, (yto - yfrom) / yticks)
  n1 = length(xx)
  n2 = length(yy)
  mat = matrix(0, nrow=length(yy), ncol=3)
  for (i in 1:n1) {
    for (j in 1:n2) {
      class = classifier(c(xx[i], yy[j]))
      if (class == -1) {
        class = 4
      }
      mat[j,] = c(xx[i], yy[j], class)
    }
    points(mat[,1], mat[,2], col=colors[mat[,3]], pch='+')
  }
}

L = function(y, x, w) {
  l = dim(x)[1]
  n = dim(x)[2]
  ret = 0
  for (i in 1:l) {
    s = sum(x[i,] * w)
    ret = ret + log(1+exp(-y[i] * s))
  }
  ret
}

dL = function(y, x, w,i=F) {
  l = dim(x)[1]
  n = dim(x)[2]
  ret = rep(0,n)
  if (i == F) {
    range = 1:l
  } else {
    range = i:i
  }
  for (i in range) {
    s = sum(x[i,] * w)
    val = exp(-y[i]*s) / (1 + exp(-y[i]*s))
    for (k in 1:n) {
      ret[k] = ret[k] - val * y[i]*x[i,k]
    }
  }
  ret
}

ndL = function(f,y,x,w) {
  n = dim(x)[2]
  ret = rep(0,n)
  h = rep(0,n)
  eps=1e-7
  for (k in 1:n) {
    h[k] = eps
    ret[k] = (f(y,x,w + h) - f(y,x,w)) / eps
    h[k] = 0
  }
  ret
}

savelines = function(w) { # save lines during learning 
  from = -100
  to = 100
  p1 = (-w[3] - w[1] * from) / w[2]
  p2 = (-w[3] - w[1] * to) / w[2]
  pts <<- rbind(pts,c(from,to,p1,p2))
}

minimize = function(f,g,y,x,w,maxiter=1000,nu=0.1,callback=savelines) {
  l = dim(x)[1]
  for (iter in 1:maxiter) {
    w = w - nu * g(y,x,w,sample(1:l,1))
    print(sprintf("%d %f", iter, f(y,x,w)))
    callback(w)
  }
  w
}

a = function(w, x) {
  n = length(x)
  x[n+1]=1
  if (sum(w*x)>=0) {
    return(1)
  } else {
    return(2)
  }
}

# build training data
x = xl[,1:n]
x = cbind(x, rep(1,l))
y = rep(0,l)
y[xl[,n+1]=="setosa"] = 1
y[xl[,n+1]!="setosa"] = -1

# check gradient implementation
# w = runif(n+1,-0.5/l,0.5/l)
# sqrt(sum(abs(dL(y,x,w) - ndL(L,y,x,w))^2))
# stop()

# uncomment to train
# pts = matrix(NA,nrow=1,ncol=4)
# w = runif(n+1,-0.5/l,0.5/l)
# w = minimize(L,dL,y,x,w,nu=0.1,maxiter=500)

classificationmap(function(x) a(w, x), xl, "map", xfrom=-2,xto=10,yfrom=-1,yto=4,xticks=100,yticks=100)
nlines = dim(pts)[1]
nlines
for (i in seq(2, nlines, 20)) {
  lines(pts[i,1:2],pts[i,3:4],col=rgb(0,i/nlines,i/nlines))
}

# count errors
errs = 0
for (i in 1:l) {
  if (y[i] != sign(sum(w*x[i,]))) {
    errs = errs + 1
  }
}
errs
