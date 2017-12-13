xl = data[,c(3,4,5)]

# xl = data
n = dim(xl)[2]-1
l = dim(xl)[1]
classes = levels(xl[,n+1])
colors = c("red", "green", "blue")

classes = levels(xl[,n+1])
Y = length(classes)


N = function(x, mu, cov) {
  n = length(x)
  x = as.matrix(x)
  arg = matrix(x-mu, nrow=n,ncol=1)
  1/sqrt(2*pi)/det(cov)*exp(-0.5 * t(arg)%*%solve(cov)%*%arg)
}

a = function(x, classes, probs, mus, covs) {
  Y = length(classes)
  n = dim(mus)[2]
  scores = rep(0, Y)
  for (i in 1:Y) {
    scores[i] = probs[i] * N(x, mus[i,], covs[[i]])
  }
  res = which.max(scores)
  factor(classes[res], levels=classes)
}

# build mean and std
mus = matrix(0, nrow=Y, ncol=n)
stds = matrix(0, nrow=Y, ncol=n)
probs = rep(0, times=Y)
for (i in 1:Y) {
  curr = factor(classes[i], levels=classes)
  xll = xl[xl[,n+1]==curr,]
  probs[i] = length(xll[,1]) / length(xl[,1])
  for (j in 1:n) {
    mus[i,j] = mean(xll[,j])
    stds[i,j] = sqrt(var(xll[,j]))
  }
}

# build covariances
covar = list()
for (c in 1:length(classes)) {
  xll = xl[xl[,n+1]==classes[c],1:n]
  covar[[c]] = cov(xll)
}

classificationmap = function(classifier, xl, title,
                             xfrom=-2, xto=7, xticks=150,
                             yfrom=-1, yto=3, yticks=150,
                             colors=c("red", "green", "blue", "gray")) {
  plot(xl[,1], xl[,2], col=colors[xl[,3]], pch=19, main=title, xlab = "Feature 3", ylab = "Feature 4", cex=2,
       xlim=c(xfrom,xto),ylim=c(yfrom,yto))
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

classificationmap(function(x) a(x, classes, probs, mus, covar), xl, "Map")

# plot curves
# x = seq(-2,7,length.out=100)
# y = seq(-1,3,length.out=100)
# for (i in 1:Y) {
#   for (j in (i+1):Y) {
#     if (j > Y) break
#     
#     form = function(x,y,mu,mat) {
#       a = mat[1,1]
#       b = mat[1,2]
#       c = mat[2,1]
#       d = mat[2,2]
#       a*x*x + (b+c)*x*y + d*y*y + 
#         (-2*a*mu[1]-b*mu[2]-c*mu[2])*x +
#         (-b*mu[1]-c*mu[1]-2*d*mu[2])*y +
#         (a*mu[1]*mu[1]+(b+c)*mu[1]*mu[2]+d*mu[2]*mu[2])
#     }
#     
#     z = outer(x,y,function(x,y) 
#       form(x,y,mus[i,],solve(covar[[i]])) -
#       form(x,y,mus[j,],solve(covar[[j]]))
#     )
#     contour(x,y,z,levels=0,add=T)
#   }
# }

errs = 0
for (i in 1:l) {
  cls = a(xl[i,1:n], classes, probs, mus, covar)
  if (cls != xl[i,n+1]) {
    errs = errs + 1
  }
}
print(errs / l)

