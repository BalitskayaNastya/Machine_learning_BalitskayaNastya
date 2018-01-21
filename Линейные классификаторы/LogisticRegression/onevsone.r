xl = data[c(3,4,5)]
# xl = data
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

phi = function(x) {
  l = dim(x)[1]
  n = dim(x)[2]-1
  ret = matrix(NA, nrow=l,ncol=2*n+1+1)
  pos = 1
  for (j in 1:n) {
    ret[,pos+1] = x[,j]
    ret[,pos+2] = x[,j] * x[,j]
    pos = pos + 2
  }
  ret[,pos] = 1
  ret[,pos+1] = x[,n+1]
  ret
}

L = function(y, x, w) {
  # x = phi(x)
  l = dim(x)[1]
  n = dim(x)[2]
  ret = 0
  for (i in 1:l) {
    s = sum(x[i,] * w)
    ret = ret + log(1+exp(-y[i] * s))
  }
  ret
}

dL = function(y, x, w,range) {
  # x = phi(x)
  l = dim(x)[1]
  n = dim(x)[2]
  ret = rep(0,n)
  for (i in range) {
    s = sum(x[i,] * w)
    val = exp(-y[i]*s) / (1 + exp(-y[i]*s))
    for (k in 1:n) {
      ret[k] = ret[k] - val * y[i]*x[i,k]
    }
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

minimize = function(f,g,y,x,w,maxiter=500,nu=0.1,callback=savelines) {
  l = dim(x)[1]
  for (iter in 1:maxiter) {
    w = w - nu * g(y,x,w,sample(1:l,6))
    print(sprintf("%d %f", iter, f(y,x,w)))
    callback(w)
  }
  w
}

rmsprop = function(f,g,y,x,w,maxiter=500,nu=0.05,decay_rate=0.9,callback=savelines) {
  l = dim(x)[1]
  cache = rep(0, length(w))
  for (iter in 1:maxiter) {
    dx = g(y,x,w,sample(1:l,25))
    cache = decay_rate * cache + (1 - decay_rate) * (dx*dx)
    w = w - nu * dx / sqrt(cache + 1e-8)
    print(sprintf("%d %f", iter, f(y,x,w)))
    callback(w)
  }
  w
}

a = function(w,x) {
  # x = phi(x)
  n = length(x)
  x[n+1] = 1
  classes = levels(xl[,n+1])
  Y = length(classes)
  
  res = rep(0,Y)
  row = 1
  for (i in 1:Y) {
    for (j in (i+1):Y) {
      if (j > Y) break
      if (sign(sum(w[row,]*x)) == 1) {
        res[i] = res[i] + 1
      } else {
        res[j] = res[j] + 1
      }
      row = row + 1
    }
  }
  which.max(res)
}

get_vs = function(xl, c1, c2) {
  l = dim(xl)[1]
  n = dim(xl)[2]-1
  
  idx1 = as.integer(xl[,n+1])==c1
  idx2 = as.integer(xl[,n+1])==c2
  x1 = xl[idx1,1:n]
  x2 = xl[idx2,1:n]
  l1 = dim(x1)[1]
  l2 = dim(x2)[1]
  x1 = cbind(x1, rep(1,l1))
  x2 = cbind(x2, rep(1,l2))
  names(x2) = names(x1)
  
  y1 = rep(1,l1)
  y2 = rep(-1,l2)
  rbind(x1,x2)
  list(rbind(x1,x2),c(y1,y2))
}

all_w = function(xl) {
  classes = levels(xl[,n+1])
  Y = length(classes)
  # xl = phi(xl)
  l = dim(xl)[1]
  n = dim(xl)[2]-1
  w = matrix(NA,nrow=Y*(Y-1)/2,ncol=n+1)
  print(w)
  
  row = 1
  for (i in 1:Y) {
    for (j in (i+1):Y) {
      if (j > Y) break
      
      tdata = get_vs(xl,classes[i],classes[j])
      x = tdata[[1]]
      y = tdata[[2]]
      pts <<- matrix(NA, nrow=1, ncol=4)
      w0 = runif(n+1,-0.5/l,0.5/l)
      wres = rmsprop(L,dL,y,x,w0)
      w[row,] = wres
      row = row + 1
    }
  }
  w
}

# train
# w = all_w(xl)
# stop()
classificationmap(function(x) a(w, matrix(x,nrow=1)), xl, "map", xfrom=-2,xto=10,yfrom=-1,yto=4,xticks=100,yticks=100)

# a(w,xl[1,1:n])
# stop()

# nlines = dim(pts)[1]
# nlines
# for (i in seq(2, nlines, 20)) {
#   lines(pts[i,1:2],pts[i,3:4],col=rgb(0,i/nlines,i/nlines))
# }

# count errors
errs = 0
for (i in 1:l) {
  if (as.integer(xl[i,n+1]) != a(w,xl[i,1:n])) {
    errs = errs + 1
  }
}
print(sprintf("%.2f%% errors. %d errors from %d", errs/l*100, errs, l))