int_f2 <- function(x, s11,s12,s21,s22) {
  f1 <- dbeta(x,shape1 = s11,shape2 = s12)
  f2 <- dbeta(x,shape1 = s21,shape2 = s22)
  pmin(pmin(f1, f2))
}