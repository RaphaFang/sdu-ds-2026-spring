# Demos for DSK805 Lecture 5 March 5 2026

# Demo 1:
# The Mahalanobis distances and chi squared distribution
###
# Setup: 
# Sample N observations from the 2d-normal distribution with mean mu
# and variance sigma.
# Compute the Squared Mahalanobis distances for the N points
# Compare their distribution against the chi_squared distribution with 2 degrees of freedom

library(mvtnorm) # This library lets us directly sample from the MVN

# The sigma and the mu don't matter, this works regardless

N=100*1000
mu=c(1,2) #
sigma=matrix(c(2,1,1,2), ncol=2,nrow=2)

sample=rmvnorm(N,mu,sigma) # Sample from MVN

dists=mahalanobis(sample,mu,sigma)

par(mfrow=c(2,2))

plot(sample)
grid()

hist(dists, freq = F) # Histogram of the distances


chi2sample=rchisq(N,2)
qqplot(dists,chi2sample, main='Quantile-Quantile plot')
grid()
abline(a=0,b=1, lty=2)

xs=dists[order(dists)]
ys=dchisq(xs,2)

hist(dists, freq = F, main='Histogram of Squared Mahalanobis distances agaisnt Chi squared (red)') # Histogram of the distances
lines(xs,ys, col='red', lwd=4)



# Demo 2 - Follow up: Drawing ellipses

par(mfrow=c(1,1))
plot(sample)
grid()

# The following function let's us draw ellipses based on mean, covariance and distance
draw_ellipse=function(mean, cov, distance, n_points=100){
  results=NULL
  mu=mean
  S=cov
  tmp=eigen(S)
  v1=sqrt(tmp$values[1])*tmp$vectors[,1]
  v2=sqrt(tmp$values[2])*tmp$vectors[,2]
  t=seq(0,2*pi, length.out=n_points)
  res=matrix(rep(0,2*n_points),ncol=2,nrow=n_points)
  for (i in 1:n_points) {
    ti=t[i]
    vector=sqrt(distance)*(cos(ti)*v1+sin(ti)*v2)+mu
    res[i,]=vector
  }
  results$res=res
  results$v1=v1
  results$v2=v2
  return(results)
}

q=0.01 # Pick a quantile q, 0 < q< 1

cutoff=qchisq(q,2) # The theoretical cutoff
cutoff2=quantile(dists,probs=q) # The empirical cutoff
res=draw_ellipse(mu,sigma,cutoff)
lines(res$res, col='red')

res2=draw_ellipse(mu,sigma,cutoff2)
lines(res2$res, col='blue')



