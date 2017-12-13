xl = data[c(3,4,5)]
n = dim(xl)[2]-1
l = dim(xl)[1]
classes = levels(xl[,n+1])
colors = c("red", "green", "blue")


Nu = function(x, mu, cov) {
  1/sqrt(2*pi*det(cov))*exp(-1/2*((x-mu) %*% solve(cov) %*% (x-mu)))
}

super = function(mu, cov, what=1) {  
  from = -3
  to = 3
  ticks = 200
  det = det(cov)
  a = cov[1,1]
  b = cov[1,2]
  c = cov[2,1]
  d = cov[2,2]
  mu1 = mu[1]
  mu2 = mu[2]
  A = d/det
  B = (-b-c)/det
  C = a/det
  D = (-2*d*mu1 + b*mu2 + c*mu2)/det
  E = (b*mu1 + c*mu1 - 2*a*mu2)/det
  f = (d*mu1*mu1-b*mu1*mu2-c*mu1*mu2+a*mu2*mu2)/det
    
  x = seq(from, to, (to - from) / ticks)
  y = seq(from, to, (to - from) / ticks)
  
  z = outer(x, y, function(x, y) 1/sqrt(2*pi*d) * exp(-1/2 * (A*x*x+B*y*x+C*y*y+D*x+E*y+f)))
  
  if (what == 1) {
    contour(x, y, z, levels=seq(0,1,0.05), drawlabels=T, asp=1)
  } else {
    mx = max(z)
    mn = min(z)
    l1 = length(x)
    l2 = length(y)
    pos = 1
    mat = matrix(0, nrow=l1*l2, ncol=3)
    for (i in 1:l1) {
      for (j in 1:l2) {
        col = rgb(z[i,j] / mx, 0, 0)
        mat[pos,] = c(x[i], x[j], col)
        pos = pos + 1
      }
      print(sprintf("%d", i))
    }
    print(mat)
    plot(mat[,1], mat[,2], col=mat[,3], pch=22, asp=1)
  } 
}

par(mfrow=c(2,2))
what = 1
super(c(0,0), matrix(c(1,0,0,1), nrow=2, ncol=2), what=what)
super(c(0,0), matrix(c(1,1,0,1), nrow=2, ncol=2), what=what)
super(c(0,0), matrix(c(3,0,0,1), nrow=2, ncol=2), what=what)
super(c(0,0), matrix(c(1,0,0,3), nrow=2, ncol=2), what=what)

