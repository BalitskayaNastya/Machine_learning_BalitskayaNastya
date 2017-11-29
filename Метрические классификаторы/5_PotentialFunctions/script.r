xl = data[c(3,4,5)]
n = dim(xl)[2]-1
l = dim(xl)[1]
colors = c("red", "green", "blue")

library(snow)
library(doSNOW)
library(foreach)
cl = makeCluster(4)
registerDoSNOW(cl)

# multithread classifier
classificationmap = function(classifier, xl, title,
                             xfrom=1, xto=7, xticks=50,
                             yfrom=0, yto=3, yticks=50,
                             colors=c("red", "green", "blue", "gray"), cex=2) {
  plot(xl[,1], xl[,2], col=colors[xl[,3]], pch=19, main=title, xlab = "Feature 3", ylab = "Feature 4", cex=cex)
  xx = seq(from=xfrom, to=xto, (xto - xfrom) / xticks)
  yy = seq(from=yfrom, to=yto, (yto - yfrom) / yticks)
  n1 = length(xx)
  n2 = length(yy)
  cl = makeCluster(4)
  registerDoSNOW(cl)
  for (i in 1:n1) {
    mat = foreach(j=1:n2,.combine = rbind,
                  .export = ls(globalenv())) %dopar% {
                    class = classifier(c(xx[i], yy[j]))
                    if (class == -1) {
                      class = 4
                    }
                    c(xx[i], yy[j], class)
                  }
    points(mat[,1], mat[,2], col=colors[mat[,3]], pch='+', cex=0.75)
  }
  stopCluster(cl)
}
# drop 2 objects to calculate their distances
dist = function(u, v) {
  sqrt(sum((u-v)^2))
}

distances = function(u, xl, ro) {
  l = dim(xl)[1]
  n = dim(xl)[2]-1
  dists = matrix(0, l, 2)
  for (i in 1:l) {
    cost = ro(u, xl[i,1:n])
    dists[i,] = c(cost, i)
  }
  idx = order(dists[,1])
  xl[dists[idx,2],]
}

# kernels start
square = function(x) {
  if (-1 <= x && x <= 1) {
    return(0.5)
  } else {
    return(0)
  }
}

triangle = function(x) {
  if (x < -1) {
    return(0)
  } else if (-1 <= x && x <= 0) {
    return(2 * x + 2)
  } else if (0 <= x && x <= 1) {
    return(-2 * x + 2)
  } else {
    return(0)
  }
}

gaussian = function(x) {
  exp(-(x * x))
}
# kernels end



# kernel and metric and get an estimated class
# from parzen window algorithm
pfunc = function(u, xl, g, h, K, ro) {
  l = dim(xl)[1]
  n = dim(xl)[2]-1
  facts = levels(xl[,n+1])
  s = rep(0, times=length(facts))
  # ret=foreach(i=1:l,.export=ls(globalenv()),.combine=rbind) %dopar% {
  ret = matrix(NA, nrow=l, ncol=2)
  for (i in 1:l) {
    fact = xl[i,n+1]
    score = g[i] * K(ro(u, xl[i,1:n])/h[i])
    ret[i,] = c(fact,score)
  }
  for (f in 1:length(facts)) {
    curr = factor(facts[f], levels=facts)
    s[f] = sum(ret[ret[,1]==f,2])
  }
  if (sum(s) == 0) {
    -1
  } else {
    factor(facts[which.max(s)], levels=facts)
  }
}

 #drop a (n,2) matrix of points to have a nice connected point plot
plotlines = function(x, xlab, ylab, title) {
  plot(x[,1], x[,2], col="red", xlab=xlab, ylab=ylab, main=title)
  lines(x)
}


# cls function should recieve 3 params: object, dataset without that object and searched loo parameter
getLoo = function(cls, xl, from, to, by) {
  l = dim(xl)[1]
  n = dim(xl)[2] - 1
  vals = seq(from, to, by)
  res = rep(0, length(vals))
  for (i in 1:length(res)) {
    for (t in 1:l) {
      class = cls(xl[t,1:n], xl[-t,], vals[i])
      if (class != xl[t,n+1]) {
        res[i] = res[i] + 1
      }
      print(c(i,t))
      print(res)
    }
  }
  res = res / l
  cbind(vals,res)
}

kernel = triangle

# # pick h
# k = 7
h = rep(0.509, times=l)
# for (i in 1:l) {
#   dists = distances(xl[i,1:n], xl[-i,], dist)
#   h[i] = max(h[i], dist(xl[i,1:n], dists[k+1,1:n]))
# }

# pick gammas
g = rep(0, times=l)
cnt = 0
while (!(g[which.max(g)] >= 7 || cnt >= 20)) {
  ## drop your function here, export global environment
  for (i in 1:l) {
    print(i)
    class = pfunc(xl[i,1:n], xl, g, h, kernel, dist)
    if (class != xl[i,n+1]) {
      g[i] = g[i] + 1
    }
  }
  cnt = cnt + 1
  print(cnt)
  print(g)
}

# loo = getLoo(function(a, b, c) pfunc(a, b, g, h, gaussian, dist), xl, 1, 1, 1)
errs = 0
for (i in 1:l) {
  if (pfunc(xl[i,1:n], xl, g, h, kernel, dist) != xl[i,n+1]) {
    errs = errs + 1
  }
}
errs

#plot
par(mfrow=c(1,1))
classificationmap(function(x) pfunc(x, xl, g, h, kernel, dist), xl, "", cex=g)

stopCluster(cl)

