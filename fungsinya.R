usethis::use_r("bernoulli")
usethis::use_r("binomial")

devtools::document()
devtools::build()
devtools::install()


library(diskrit1)

# Bernoulli
simulasi_bernoulli(10, 0.7)
pmf_bernoulli(1, 0.7)
cdf_bernoulli(0.5, 0.7)

# Binomial
simulasi_binomial(10, size = 5, p = 0.5)
pmf_binomial(2, size = 5, p = 0.5)
cdf_binomial(2, size = 5, p = 0.5)


devtools::check()
