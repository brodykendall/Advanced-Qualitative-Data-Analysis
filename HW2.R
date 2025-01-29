lambda_r = function(p1, p2, n) {
  return((9*(n*p1+n*p2-2*n/3)^2)/(2*n))
}

lambda_u = function(p1, p2, n) {
  return((6*(n*p1)^2)/(n) + (6*(n*p2)^2)/(n) + (6*(n*p1)*(n*p2))/(n) - 6*(n*p1) - 6*(n*p2) + 2*n)
}

aPs = function(p1, p2, n) {
  yr = round(lambda_r(p1, p2, n), 12)
  yu = round(lambda_u(p1, p2, n), 12)
  aPr = round(1-pchisq(3.8415, 1, ncp = yr), 2)
  aPu = round(1-pchisq(5.9915, 2, ncp = yu), 2)
  return(c(aPr, aPu))
}

Ps = function(p1, p2, n) {
  N=10000
  x = rmultinom(N, n, c(p1, p2, 1-p1-p2))
  sr = (9*(x[1,] + x[2,] - 2*n/3)^2)/(2*n)
  Pr = round(sum(sr > 3.8415)/N, 2)
  su = (6*x[1,]^2)/n + (6*x[2,]^2)/n + (6*x[1,]*x[2,])/n - 6*x[1,] - 6*x[2,] + 2*n
  Pu = round(sum(su > 5.9915)/N, 2)
  return(c(Pr, Pu))
}

ns = function(p1, p2) {
  nr = 1
  yr = lambda_r(p1, p2, nr)
  aPr = 1-pchisq(3.8415, 1, ncp = yr)
  while(aPr < 0.8 & nr < 10000) {
    nr = nr+1
    yr = lambda_r(p1, p2, nr)
    aPr = 1-pchisq(3.8415, 1, ncp = yr)
  }
  
  nu = 1
  yu = lambda_u(p1, p2, nu)
  aPu = 1-pchisq(5.9915, 2, ncp = yu)
  while(aPu < 0.8 & nu < 10000) {
    nu = nu+1
    yu = round(lambda_u(p1, p2, nu), 11)
    aPu = 1-pchisq(5.9915, 2, ncp = yu)
  }
  
  return(c(nr, nu))
}