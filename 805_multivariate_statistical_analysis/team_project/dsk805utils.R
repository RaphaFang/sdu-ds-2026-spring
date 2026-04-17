library(car)
yj_transform <- function(x) {
  pt <- powerTransform(x)
  return(yjPower(x, coef(pt)))
}
