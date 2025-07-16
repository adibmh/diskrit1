#simulasi

library(diskrit1)

# Simulasi Bernoulli dengan plot
simulasi_bernoulli(n = 100, p = 0.6, plot = TRUE)

# Simulasi Binomial dengan plot
simulasi_binomial(n = 200, size = 10, p = 0.3, plot = TRUE)


##studi kasus
data_bernoulli <- c(1, 1, 1, 0, 1, 1, 0, 1, 1, 1,
                    0, 1, 1, 1, 1, 0, 1, 0, 1, 1,
                    1, 1, 1, 0, 0, 1, 1, 0, 1, 1)

# Estimasi probabilitas p̂
estimasi_p_bernoulli(data_bernoulli)

# Peluang puas (x = 1)
pmf_bernoulli(x = 1, p = 0.77)

# Peluang tidak puas (x = 0)
pmf_bernoulli(x = 0, p = 0.77)

# Peluang kumulatif hingga x = 0 (artinya: P(X ≤ 0))
cdf_bernoulli(x = 0, p = 0.77)

# Peluang kumulatif hingga x = 1 (artinya: P(X ≤ 1) = 1)
cdf_bernoulli(x = 1, p = 0.77)

# Plot distribusi
plot_bernoulli(data_bernoulli)


#binomial
data_binomial <- c(5, 3, 4, 2, 3, 4, 5, 5, 3, 2,
                   4, 4, 5, 3, 2, 5, 4, 4, 3, 5)
size <- 5

# PMF: Peluang tepat 3 benar
pmf_binomial(x = 3, size = 5, p = 0.74)

# CDF: Peluang ≤ 2 benar (P(X ≤ 2))
cdf_binomial(x = 2, size = 5, p = 0.74)

# Estimasi probabilitas keberhasilan per soal
estimasi_p_binomial(data_binomial, size = 5)

# Visualisasi jumlah sukses
plot_binomial(data_binomial, size = 5)


#plot teoritis
plot_pmf_bernoulli(p = 0.7)
plot_cdf_bernoulli(p = 0.7)

plot_pmf_binomial(size = 10, p = 0.5)
plot_cdf_binomial(size = 10, p = 0.5)
