
# Linear Algebra
df=read.table('T1-9R.dat')
X=as.matrix(df[,-1]) # An n by p matrix
In=rep(1,(dim(X)[1])) # A vector of all ones, I_n=(1,1,1,...,1) (one 1 per one row of X)
colnames(X)=c('100m','200m','400m','800m','1500m','3000m','42.19km')
ybar=In%*%(X)/(length(In)) # sample means # The column sums are given by 1_n %*% X, divided by n this is the row means
Xtmp=matrix(rep(ybar,dim(X)[1]),ncol=length(ybar),nrow=dim(X)[1], byrow=TRUE) # Make a matrix Xtmp of same dimensions of X that contains the column means 
Z=X-Xtmp # Z is X minus the sample mean for each variable
t(Z)%*%Z/(dim(X)[1]-1) # Z^T Z is the square sum matrix, divided by 1/(n-1) this gives the sample variance-covariance matrix
var(X) # Verify the above gives the same result as what we have here

# Correlation plot
library(corrplot)
corrplot(
  cor_mat,
  method = "circle",     # circles (default style)
  addCoef.col = "white", # add correlation numbers
  number.cex = 0.9       # size of numbers
)

#Mahalanobis distances

names=df[,1]
mu=colMeans(X)
cov=var(X)
dists=mahalanobis(X,mu,cov)
DF1=data.frame(cbind(names,dists))

# Show the data in the increasing order of Mahalanobis distance
df[order(dists),]

# Compare this with the 7-dimensional distribution:
plot(df)

# Not so easy to see what is going on

