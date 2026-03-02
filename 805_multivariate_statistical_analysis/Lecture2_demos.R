# Demonstrating positive definite symmetric matrices in 2d


A=matrix(c(2,1,1,2),ncol=2,nrow=2,byrow=TRUE)

scale=max(eigen(A)$values)

theta=c(1:1000)/1000*2*pi # 1000 points on a unit circle

x=cos(theta) # the x-coordinates
y=sin(theta) # the y- coordinates


plot(x,y, xlim=c(-scale,scale), ylim=c(-scale, scale)) # plots a circle
grid()


z=A%*%rbind(x,y)
points(t(z), col='red')


tmp=eigen(A)
v1=tmp$vectors[,1]
v2=tmp$vectors[,2]

V1=rbind(c(0,0),v1)
V2=rbind(c(0,0),v2)


lines(V1, col='blue', lty=1, lwd=4)
lines(V2, col='blue', lty=1, lwd=4)

LV1=rbind(c(0,0),tmp$values[1]*v1)
LV2=rbind(c(0,0),tmp$values[2]*v2)

lines(LV1, col='blue',lty =2 )
lines(LV2, col='blue', lry =2 )



tmp=eigen(A)

A=matrix(c(2,1,1,2),ncol=2,nrow=2,byrow=TRUE)
tmp=eigen(A)
Q=tmp$vectors
D=diag(tmp$values)
Q%*%D%*%t(Q)
