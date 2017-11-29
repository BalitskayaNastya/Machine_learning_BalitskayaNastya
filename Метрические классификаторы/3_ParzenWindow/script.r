setwd(dirname(sys.frame(1)$ofile))
selected = data[c(3,4,5)]
n = dim(selected)[2]-1
l = dim(selected)[1]
colors = c("red", "green", "blue")

# drop 2 objects to calculate their distances
dist = function(u, v) {
  features = length(u)-1
  sqrt(sum((u-v)^2))
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


# fetch an object, dataset, window size,
# kernel and metric and get an estimated class
# from parzen window algorithm
a = function(u, xl, h, K, ro) {
  l = dim(xl)[1]
  n = dim(xl)[2]-1
  facts = levels(xl[,n+1])
  s = rep(0, times=length(facts))
  for (i in 1:l) {
    fact = xl[i,n+1]
    score = K(ro(u, xl[i,1:n])/h)
    s[fact] = s[fact] + score
  }
  if (sum(s) == 0) {
    -1
  } else {
    factor(facts[which.max(s)], levels=facts)
  }
}

#  drop a dataset and range to compute a loo
# for a given kernel to find best window size
looforh = function(xl, from, to, by, kernel, ro) {
  l = dim(xl)[1]
  n = dim(xl)[2]-1
  hs = seq(from=from, to=to, by=by)
  res = rep(0, length(hs))
  for (i in 1:length(hs)) {
    for (t in 1:l) {
      print(sprintf("iter: %d, h: %f, case: %d", i, hs[i], t))
      class = a(xl[t,1:n], xl[-t,], hs[i], kernel, ro)
      if (class != xl[t,n+1]) {
        res[i] = res[i] + 1
      }
    }
  }
  res = res / l
  cbind(hs,res)
}

# commented computation
#s = 1:l
#smpl = sample(s, 30)
#tmp = selected[smpl,]
#tmp = selected

plotlines = function(x, name) {
  plot(x[,1], x[,2], col="red", xlab="h", ylab="error", main=c("Loo for ", name))
  lines(x)
}

#  drop a classifier, dataset and other parameters to build
# a nice classification map, an example of a call would be like that:
# classificationmap(function(x) a(x, selected, 0.394, square, dist),
#       dataset, "A map", -1, 1, 50, -1, 1, 50)
classificationmap = function(classifier, xl, title,
                             xfrom=1, xto=7, xticks=50,
                             yfrom=0, yto=3, yticks=50,
                             colors=c("red", "green", "blue", "gray")) {
  plot(xl[,1], xl[,2], col=colors[xl[,3]], pch=19, main=title, xlab = "Feature 3", ylab = "Feature 4")
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

xfrom = 1
xto = 7
xticks = 50
yfrom = 0
yto = 3
yticks = 20

# plot loos for kernels
from = 0.01
to = 5
step = (to - from) / 20
# resforsquare = looforh(selected, from, to, step, square, dist)
# resfortriangle = looforh(selected, from, to, step, triangle, dist)
# resforgauss = looforh(selected, from, to, step, gaussian, dist)
sarg = which.min(resforsquare[,2])
targ = which.min(resfortriangle[,2])
garg = which.min(resforgauss[,2])
par(mfrow=c(3,1))
plotlines(resforsquare, "Square")
plotlines(resfortriangle, "Square")
plotlines(resforgauss, "Square")
print(sprintf("Best for %s is %f val is %f", 
              c("sq", "tri", "gauss"), 
              c(resforsquare[sarg,1], resfortriangle[targ,1], resforgauss[garg,1]),
              c(resforsquare[sarg,2], resfortriangle[targ,2], resforgauss[garg,2])
                ))

classificationmap(function(x) a(x, selected, resforsquare[sarg,1], square, dist),
                  selected, "Map for square", xfrom, xto, xticks, yfrom, yto, yticks)

classificationmap(function(x) a(x, selected, resfortriangle[targ,1], triangle, dist),
                  selected, "Map for triange", xfrom, xto, xticks, yfrom, yto, yticks)

classificationmap(function(x) a(x, selected, resforgauss[garg,1], gaussian, dist),
                  selected, "Map for gauss", xfrom, xto, xticks, yfrom, yto, yticks)
