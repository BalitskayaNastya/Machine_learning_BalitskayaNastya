data=iris
par(mfrow=c(2,1))
selected = data[c(3,4,5)]
features = dim(selected)[2]-1
cases = dim(selected)[1]
colors = c("red", "green", "blue")


dist = function(u, v) { 
  sqrt(sum((u-v)^2))
}

distances = function(obj, data, metric) { 
  dists = matrix(0, cases, 2)
  for (i in 1:cases) {
    cost = metric(obj, data[i,1:features])
    dists[i,] = c(cost, i)
  }
  idx = order(dists[,1])
  data[dists[idx,2],]
}

NN = function(obj, data, metric=dist) { 
  sorted = distances(obj, data, metric)
  sorted[1,features+1]
}

kNN = function(obj, data, k, metric=dist) { 
  sorted = distances(obj, data, metric)
  
  n = 10 
  counts = rep(0, times=n)
  for (i in 1:k) {
    cls = sorted[i,features+1]
    counts[cls] = counts[cls] + 1
  }
  argmax = 1
  for (i in n) {
    if (counts[argmax] < counts[i]) {
      argmax = i
    }
  }
  
  cls[argmax]
}



points = rbind( 
  c(5.5, 2),
  c(2.5, 1),
  c(3.5, 1.5),
  c(1, 0.7),
  c(5.4, 2)
)


#  1NN
plot(selected[,1], selected[,2], col=colors[selected[,features+1]], xlab="1NN", ylab="")
for (i in 1:dim(points)[1]) {
  pt = points[i,]
  points(pt[1], pt[2], col=colors[NN(pt, selected)], pch=19) 
}

#  kNN
plot(selected[,1], selected[,2], col=colors[selected[,features+1]], xlab="kNN", ylab="")
for (i in 1:dim(points)[1]) {
  pt = points[i,]
  points(pt[1], pt[2], col=colors[kNN(pt, selected, 7)], pch=19) 
}

