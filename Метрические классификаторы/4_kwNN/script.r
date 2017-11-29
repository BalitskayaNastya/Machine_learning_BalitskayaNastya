library(doSNOW)
library(foreach)

selected = data[c(3,4,5)]
n = dim(selected)[2]-1
l = dim(selected)[1]
colors = c("red", "green", "blue")

# drop 2 objects to calculate their distances
dist = function(u, v) {
  features = length(u)-1
  sqrt(sum((u-v)^2))
}
# drop an object, dataset and desired metric
# and get a sorted dataset by this metric! wow!
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

kwNN = function(u, xl, k, q, ro) {
  l = dim(xl)[1]
  n = dim(xl)[2]-1
  sorted = distances(u, xl, ro)
  facts = levels(xl[,n+1])
  scores = rep(0, times=length(facts))
  curr = 1
  for (i in 1:k) {
    currclass = sorted[i,n+1]
    scores[currclass] = scores[currclass] + curr
    curr = curr * q
  }
  which.max(scores)
}

#  a universal loo function, drop classifier, dataset and range :)
# cls function should recieve 3 params: object, dataset without that object and searched loo parameter
getLoo = function(cls, xl, from, to, by) {
  l = dim(xl)[1]
  n = dim(xl)[2] - 1
  vals = seq(from, to, by)
  res = rep(0, length(vals))
  for (i in 1:length(res)) {
    for (t in 1:l) {
      class = cls(xl[t,1:n], xl[-t,], vals[i])
      if (class != as.integer(xl[t,n+1])) {
        res[i] = res[i] + 1
      }
      print(c(i,t))
      print(res)
    }
  }
  res = res / l
  cbind(vals,res)
}

#  drop a classifier, dataset and other parameters to build
# a nice classification map, an example of a call would be like that:
# classificationmap(function(x) a(x, selected, 0.394, square, dist),
#       dataset, "A map", -1, 1, 50, -1, 1, 50)
classificationmap = function(classifier, xl, title,
                             xfrom=1, xto=7, xticks=50,
                             yfrom=0, yto=3, yticks=50,
                             colors=c("red", "green", "blue", "gray")) {
  plot(xl[,1], xl[,2], col=colors[xl[,3]], pch=19, main=title, xlab = "Feature 3", ylab = "Feature 4", cex=2)
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

loos = getLoo(function(a, b, c) kwNN(a,b,6,c,dist), selected, 1, 1, 0.05)
par(mfrow=c(1,1))
best = which.min(loos[,2])
print(sprintf("Best q is %f with val %f", loos[best,1], loos[best,2]))
plotlines(loos, "k", "Loo(k)", "Loo for kwNN")

xfrom = 1
xto = 7
xticks = 50
yfrom = 0
yto = 3
yticks = 20

# classificationmap(function(x) kwNN(x, selected, 6, loos[best,1], dist), selected, "Map for kwNN")
