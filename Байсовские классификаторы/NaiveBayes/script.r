xl = data[,c(3,4,5)]
# xl = data

n = dim(xl)[2]-1
l = dim(xl)[1]
classes = levels(xl[,n+1])
colors = c("red", "green", "blue")

classes = levels(xl[,n+1])
Y = length(classes)

N = function(x, mu, sig) {
  1/sqrt(2*pi)/sig*exp(-1/2*(x-mu)^2/sig^2)
}

a = function(x, classes, probs, mus, stds) {
  Y = length(classes)
  n = dim(mus)[2]
  scores = rep(0, Y)
  for (i in 1:Y) {
    scores[i] = probs[i]
    for (j in 1:n) {
      scores[i] = scores[i] * N(x[j], mus[i,j], stds[i,j])
    }
  }
  which.max(scores)
}

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

classificationmap = function(classifier, xl, title,
                             xfrom=-2, xto=7, xticks=50,
                             yfrom=-2, yto=3, yticks=50,
                             colors=c("red", "green", "blue", "gray")) {
  plot(xl[,1], xl[,2], col=colors[xl[,3]], pch=19, main=title, xlab = "Feature 3", ylab = "Feature 4", cex=2,
    xlim = (c(xfrom,xto)),ylim = c(yfrom,yto))
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

classificationmap(function(x) a(x, classes, probs, mus, stds), xl, "Hello")

errs = 0
for (i in 1:l) {
  cls = a(xl[i,1:n], classes, probs, mus, stds)
  if (cls != as.integer(xl[i,n+1])) {
    errs = errs + 1
  }
}
print(errs / l)

