#' Simulasi Binomial
#'
#' @param n Jumlah observasi
#' @param size Jumlah percobaan
#' @param p Probabilitas sukses
#' @param plot Logika, apakah hasil akan diplot?
#' @return Vektor data Binomial
#' @export
simulasi_binomial <- function(n, size, p, plot = FALSE) {
  data <- rbinom(n, size, p)

  if (plot) {
    barplot(table(data),
            col = "skyblue",
            main = paste("Simulasi Binomial (n =", n, ", size =", size, ", p =", p, ")"),
            xlab = "x", ylab = "Frekuensi")
  }

  return(data)
}


#' PMF Binomial (dengan rumus eksplisit)
#'
#' @param x Nilai x (jumlah sukses)
#' @param size Jumlah percobaan (n)
#' @param p Probabilitas sukses
#' @return Probabilitas f(x)
#' @export
pmf_binomial <- function(x, size, p) {
  if (x < 0 || x > size || p < 0 || p > 1) {
    return(0)
  }
  choose(size, x) * p^x * (1 - p)^(size - x)
}

#' CDF Binomial (dengan penjumlahan manual PMF)
#'
#' @param x Nilai x
#' @param size Jumlah percobaan (n)
#' @param p Probabilitas sukses
#' @return Probabilitas kumulatif hingga x
#' @export
cdf_binomial <- function(x, size, p) {
  if (x < 0) {
    return(0)
  }
  sum(sapply(0:floor(x), function(k) {
    choose(size, k) * p^k * (1 - p)^(size - k)
  }))
}


#' Estimasi Parameter p untuk Distribusi Binomial
#'
#' @param data Vektor jumlah sukses per observasi
#' @param size Jumlah percobaan per observasi
#' @return Estimasi p̂ sebagai proporsi keberhasilan
#' @export
estimasi_p_binomial <- function(data, size) {
  if (any(data < 0) || any(data > size)) {
    stop("Nilai data harus di antara 0 dan size.")
  }
  p_hat <- mean(data) / size
  cat("Estimasi p (p̂) Binomial:", round(p_hat, 4), "\n")
  return(p_hat)
}

#' Plot PMF Distribusi Binomial
#'
#' @param size Jumlah percobaan
#' @param p Probabilitas sukses
#' @export
plot_pmf_binomial <- function(size, p) {
  x <- 0:size
  y <- dbinom(x, size, p)

  barplot(y,
          names.arg = x,
          col = "skyblue",
          ylim = c(0, max(y) + 0.05),
          main = paste("PMF Binomial (n =", size, ", p =", p, ")"),
          xlab = "x", ylab = "P(X = x)")
}


#' Plot CDF Distribusi Binomial
#'
#' @param size Jumlah percobaan
#' @param p Probabilitas sukses
#' @export
plot_cdf_binomial <- function(size, p) {
  x <- 0:size
  y <- pbinom(x, size, p)

  plot(x, y, type = "s", col = "darkgreen", lwd = 2,
       main = paste("CDF Binomial (n =", size, ", p =", p, ")"),
       xlab = "x", ylab = "P(X ≤ x)", ylim = c(0, 1.1))
  abline(h = 1, col = "gray", lty = 2)
}


#' Analisis dan Visualisasi Data Binomial
#'
#' @param data Vektor jumlah sukses per pelanggan (misalnya dari 10 pelanggan)
#' @param size Jumlah percobaan per pelanggan (harus diketahui)
#' @return Estimasi p̂ dan barplot jumlah sukses
#' @export
plot_binomial <- function(data, size) {
  if (any(data < 0) || any(data > size)) {
    stop("Nilai data harus berada antara 0 dan size.")
  }
  n <- length(data)
  p_hat <- mean(data) / size

  cat("Jumlah Observasi:", n, "\n")
  cat("Percobaan per Observasi:", size, "\n")
  cat("Estimasi Probabilitas Sukses p̂:", round(p_hat, 4), "\n")

  barplot(table(factor(data, levels = 0:size)),
          col = "skyblue",
          main = paste("Distribusi Jumlah Sukses (Binomial, size =", size, ")"),
          xlab = "Jumlah Sukses", ylab = "Frekuensi")
}


