one_sample_test =function(data, mu0){
  # Compute summaries
  n=dim(data)[1] # number of observations
  p=dim(data)[2] # number of rows
  ybar=colMeans(data) #compute sample mean
  S=cov(data) #compute sample variance
  
  # Compute the Hotelling T^2 statistic:
  T2=n*t(ybar-mu0)%*%solve(S)%*%(ybar-mu0)
  
  # Compute the degrees of freedom for the T^2-distribution
  nu=n-1
  tcoef = (nu-p+1)/(nu * p) # The transformation coefficient
  Fstat= tcoef*T2 # The F-statistic
  df1=p # the degrees of freedom for the F-distribution
  df2=nu-p+1
  
  # Inference:
  1-pf(Fstat,df1,df2) # The p-value
  # Reject if p-value smaller than the specified level (alpha)
  
  # Alternatively, we can reject based on the critical values:
  # Critical values (Reject if Fstat is higher than these)
  # We need to fix a level at something, below is the typical 0.05 level
  alpha=0.05 
  qf(1-alpha,df1,df2) # the critical value
  
  res=NULL
  
  res$pvalue=1-pf(Fstat,df1,df2)
  res$T2=T2
  res$Fstat=Fstat
  res$p=p
  res$nu=nu
  res$df1=df1
  res$df2=df2
  
  return(res)
}

two_sample_test =function(data1,data2){
  
  n1=dim(data1)[1];n2=dim(data2)[1] #sample sizes
  p=dim(data1)[2] # dimension
  xbar=colMeans(data1) #sample mean
  ybar=colMeans(data2) #sample mean
  Sx=cov(data1);Sy=cov(data2)# sample variances
  S= ((n1-1)*Sx+(n2-1)*Sy)/(n1+n2-2) #pooled variance S_pl
  coef=n1*n2/(n1+n2) # An auxiliary computation
  
  # Compute the test statistic
  T2=coef*t(xbar-ybar)%*%solve(S)%*%(xbar-ybar) # The test statistic
  
  ## Inference: Convert the T2 to an F-statistic
  # Compute the parameters
  nu= n1+n2-2
  tcoef = (nu-p+1)/(nu * p) # The transformation coefficient
  Fstat= tcoef*T2 # The F-statistic
  df1=p # the degrees of freedom for the F-distribution
  df2=nu-p+1
  
  # Inference:
  1-pf(Fstat,df1,df2) # The p-value
  # Reject if p-value smaller than the specified level (alpha)
  
  # Alternatively, we can reject based on the critical values:
  # Critical values (Reject if Fstat is higher than these)
  # We need to fix a level at something, below is the typical 0.05 level
  alpha=0.05 
  qf(1-alpha,df1,df2) # the critical value
  
  res=NULL
  
  res$pvalue=1-pf(Fstat,df1,df2)
  res$T2=T2
  res$Fstat=Fstat
  res$p=p
  res$nu=nu
  res$df1=df1
  res$df2=df2
  
  return(res)
}