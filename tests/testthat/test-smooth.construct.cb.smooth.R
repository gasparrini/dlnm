library(mgcv)
data(nested)
Q <- as.matrix(nested[, 5:14]) 
L <- matrix(1:ncol(Q), nrow = nrow(Q), ncol = ncol(Q), byrow = TRUE)
xt1 <- list(bs = c("cr", "cr"))
xt2 <- list(bs = c("cr", "cs"))
m_cr <- gam(case ~ age + s(Q, L, bs = "cb", xt = xt1), family = binomial, data = nested)
m_cs <- gam(case ~ age + s(Q, L, bs = "cb", xt = xt2), family = binomial, data = nested)


test_that("penalty matrix is formed correctly for bs = 'cs'", {
  expect_length(m_cs$sp, 2)
  expect_named(m_cs$sp, c("s(Q,L)Svar", "s(Q,L)Slag"))
})

test_that("penalty matrix is formed correctly for bs = 'cr'", {
  expect_length(m_cr$sp, 2)
  expect_named(m_cr$sp, c("s(Q,L)Svar", "s(Q,L)Slag"))
})

test_that("penalty matrixes differ", {
  expect_false(all.equal(m_cs$sp, m_cr$sp, check.attributes = FALSE))
})

test_that("defining marginal basis equivalant between bs = and arglag/argvar", {
  xt3 <- list(argvar = list(fun = "cr"),
              arglag = list(fun = "cr"))
  m_cr2 <- gam(case ~ age + s(Q, L, bs = "cb", xt = xt3), family = binomial, data = nested)
  expect_equivalent(m_cr, m_cr2)
})