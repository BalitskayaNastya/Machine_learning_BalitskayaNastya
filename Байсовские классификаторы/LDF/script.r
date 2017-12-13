xl = data[,c(3,4,5)]

# xl = data
n = dim(xl)[2]-1
l = dim(xl)[1]
classes = levels(xl[,n+1])
colors = c("red", "green", "blue")

classes = levels(xl[,n+1])
Y = length(classes)

N = function(x, mu, covinv, covdet) {
  n = length(x)
  m1 = matrix(x-mu, nrow=1, ncol=n)
  m2 = matrix(x-mu, nrow=n, ncol=1)
  1/sqrt((2*pi)^n*covdet)*exp(-1/2*m1 %*% covinv %*% m2)
}

ldf = function(x, classes, probs, mus, covinv, covdet) {
  Y = length(classes)
  n = dim(mus)[2]
  scores = rep(0, Y)
  for (i in 1:Y) {
    mu = matrix(mus[i,], nrow=n, ncol=1)
    scores[i] = log(probs[i]) - 1/2*t(mu)%*%covinv%*%mu + x%*%covinv%*%mu
  }
  which.max(scores)
}

mus = matrix(0, nrow=Y, ncol=n)
stds = matrix(0, nrow=Y, ncol=n)
covs = matrix(0, nrow=n, ncol=n)
covinv = matrix(0, nrow=n, ncol=n)
covdet = rep(0, times=Y)
probs = rep(0, times=Y)
for (i in 1:Y) {
  xll = xl[xl[,n+1]==classes[i],]
  probs[i] = length(xll[,1]) / length(xl[,1])
  for (j in 1:n) {
    mus[i,j] = mean(xll[,j])
    stds[i,j] = sqrt(var(xll[,j]))
  }
}
for (k in 1:l) {
  for (i in 1:n) {
    for (j in 1:n) {
      cls = xl[k,n+1]
      covs[i,j] = covs[i,j] + (xl[k,i] - mus[cls,i]) * (xl[k,j] - mus[cls,j])
    }
  }
}
covs = covs / l
covinv = solve(covs)
covdet = det(covs)

classificationmap = function(classifier, xl, title,
                             xfrom=1, xto=7, xticks=50,
                             yfrom=0, yto=3, yticks=50,
                             colors=c("red", "green", "blue", "gray")) {
  plot(xl[,1], xl[,2], col=colors[xl[,3]], pch=19, main=title, xlab = "Feature 3", ylab = "Feature 4", cex=2)
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
    points(mat[,1], mat[,2], col=colors[mat[,3]], pch=22, cex=1.5)
  }
}

classificationmap(function(x) ldf(x, classes, probs, mus, covinv, covdet), xl, "")

# plot splitting lines
plot(xl[,1], xl[,2], col=colors[xl[,3]], pch=19, main="", xlab = "Feature 3", ylab = "Feature 4", cex=2)
as = matrix(0, ncol=2, nrow=Y)
bs = rep(0, times=Y)
for (i in 1:Y) {
  bs[i] = log(probs[i]) - 1/2*t(mus[i,])%*%covinv%*%mus[i,]
  as[i,] = covinv %*% mus[i,]
}
from = 1
to = 7
for (i in 1:Y) {
  for (j in (i+1):Y) {
    if (j > Y) break
    p1 = ((bs[i] - bs[j]) - from * (as[j,1] - as[i,1])) / (as[j,2] - as[i,2])
    p2 = ((bs[i] - bs[j]) - to * (as[j,1] - as[i,1])) / (as[j,2] - as[i,2])
    lines(c(from, to), c(p1, p2))
  }
}

errs = 0
for (i in 1:l) {
  cls = ldf(as.matrix(xl[i,1:n]), classes, probs, mus, covinv, covdet)
  if (cls != as.integer(xl[i,n+1])) {
    errs = errs + 1
  }
}
print(errs / l)