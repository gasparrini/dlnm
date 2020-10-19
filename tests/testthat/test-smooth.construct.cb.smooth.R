library(mgcv)
data(nested)
Q <- as.matrix(nested[, 5:14]) 
L <- matrix(1:ncol(Q), nrow = nrow(Q), ncol = ncol(Q), byrow = TRUE)

test_that("penalty matrix is formed correctly for bs = 'cs'", {
  xt <-
    list(
      bs = c("cr", "cs"),
      arglag = list(int = F)
    )
  m_cs <- gam(case ~ age + s(Q, L, bs = "cb", xt = xt), family = binomial, data = nested)
  
  expect_length(m_cs$sp, 2)
  expect_named(m_cs$sp, c("s(Q,L)Svar", "s(Q,L)Slag"))
})

test_that("penalty matrix is formed correctly for bs = 'cr'", {
  xt <-
    list(
      bs = c("cr", "cr"),
      arglag = list(int = F)
    )
  m_cs <- gam(case ~ age + s(Q, L, bs = "cb", xt = xt), family = binomial, data = nested)
  
  expect_length(m_cs$sp, 2)
  expect_named(m_cs$sp, c("s(Q,L)Svar", "s(Q,L)Slag"))
})