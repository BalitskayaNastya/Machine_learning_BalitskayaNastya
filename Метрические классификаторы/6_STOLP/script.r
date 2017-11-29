library(snow)
xl = data[c(3,4,5)]
n = dim(xl)[2]-1
l = dim(xl)[1]
colors = c("red", "green", "blue")

dist = function(u, v) {
  sqrt(sum((u-v)^2))
}

distances = function(obj, data, metric) {
  l = dim(data)[1]
  n = dim(data)[2]-1
  dists = matrix(0, l, 2)
  for (i in 1:l) {
    cost = metric(obj, data[i,1:n])
    dists[i,] = c(cost, i)
  }
  idx = order(dists[,1])
  data[dists[idx,2],]
}

# xfrom = 1
# xto = 7
# xticks = 50
# yfrom = 0
# yto = 3
# yticks = 20

M = function(x, y, xl, k) {
  l = dim(xl)[1]
  n = dim(xl)[2]-1
  dists = distances(x, xl, dist)
  dists = dists[1:k,]
  t = table(dists[,n+1])
  wyi = t[y]
  ret = wyi
  
  others = dists[dists[,n+1]!=y,]
  if (dim(others)[1] > 0) {
    t = table(others[,n+1])
    wm = t[which.max(t)]
    ret = ret - wm
  }
  
  names(ret) = NULL
  ret
}

STOLP = function(xl, delta, l0, k) {
  # remove error cases
  l = dim(xl)[1]
  n = dim(xl)[2]-1
  good = rep(T, times=l)
  for (i in 1:l) {
    print(sprintf("removing bad: %d", i))
    if (M(xl[i,1:n], xl[i,n+1], xl[-i,], k) < delta) {
      good[i] = F
    }
  }
  xl = xl[good,]
  l = dim(xl)[1]
  
  #  build initial omega,
  # idxval contains pairs (index, M-value)
  inomega = rep(F, times=l)
  classes = length(levels(xl[,n+1]))
  idxval = matrix(0, nrow=classes, ncol=2)
  for (i in 1:l) {
    print(sprintf("inital omega: %d", i))
    curr = M(xl[i,1:n], xl[i,n+1], xl[-i,], k)
    cls = xl[i,n+1]
    if (idxval[cls,2] < curr) {
      idxval[cls,1] = i
      idxval[cls,2] = curr
    }
  }
  inomega[idxval[,1]] = T
  
  while (length(inomega[inomega==T]) < 30) {    
    #  build E
    omega = xl[inomega,]
    xl_omega = xl[!inomega,]
    worst = matrix(1e9, nrow=l, ncol=2)
    for (i in 1:l) {
      print(sprintf("omega length: %d, current i: %d", length(inomega[inomega==T]), i))
      if (!inomega[i]) {
        val = M(xl[i,1:n], xl[i,n+1], omega, k)
        if (val < delta) {
          worst[i,] = c(i, val)
        }
      }
    }
    E = xl[!inomega && worst[worst[,2]<delta,1],]
    print(E)
    if (dim(E)[1] < l0) { # exit condition
      return(omega)
    }
    
    # add mistaken
    idx = worst[order(worst[,2])[1],1]
    if (idx == 1e9) {
      break
    } else {
      inomega[idx] = T
    }
  }
  
  xl[inomega,]
}


kNN = function(u, xl, k, metric) {
  l = dim(xl)[1]
  n = dim(xl)[2]-1
  sorted = distances(u, xl, metric)
  t = table(sorted[1:k,n+1])
  res = names(which.max(t))
  factor(res, levels=levels(xl[,n+1]))
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


# display picked cases
par(mfrow=c(1,1))
plot(xl[,1], xl[,2], col="gray", pch=19, cex=2)
points(res[,1], res[,2], col=colors[res[,3]], pch=19, cex=2)

# # build classification map
# par(mfrow=c(2,1))
# classificationmap(function(x) kNN(x, res, 5, dist), res, "Selected by STOLP")
# classificationmap(function(x) kNN(x, res, 5, dist), xl, "Classification map for all")