#' Simulasi Bernoulli
#'
#' @param n Jumlah observasi
#' @param p Probabilitas sukses
#' @param plot Logika, apakah hasil akan diplot?
#' @return Vektor data Bernoulli
#' @export
simulasi_bernoulli <- function(n, p, plot = FALSE) {
  data <- rbinom(n, 1, p)

  if (plot) {
    barplot(table(data),
            col = c("tomato", "lightgreen"),
            names.arg = c("0", "1"),
            main = paste("Simulasi Bernoulli (n =", n, ", p =", p, ")"),
            xlab = "x", ylab = "Frekuensi")
  }

  return(data)
}


#' PMF Bernoulli
#'
#' @param x Nilai 0 atau 1
#' @param p Probabilitas sukses
#' @return Probabilitas f(x)
#' @export
pmf_bernoulli <- function(x, p) {
  ifelse(x == 1, p, 1 - p)
}

#' CDF Bernoulli
#'
#' @param x Nilai x (real)
#' @param p Probabilitas sukses
#' @return Probabilitas kumulatif hingga x
#' @export
cdf_bernoulli <- function(x, p) {
  ifelse(x < 0, 0, ifelse(x < 1, 1 - p, 1))
}

#' Estimasi Parameter p untuk Distribusi Bernoulli
#'
#' @param data Vektor data bernoulli (0 atau 1)
#' @return Estimasi p̂ sebagai rata-rata dari data
#' @export
estimasi_p_bernoulli <- function(data) {
  if (!all(data %in% c(0, 1))) {
    stop("Data hanya boleh berisi 0 dan 1 untuk distribusi Bernoulli.")
  }
  p_hat <- mean(data)
  cat("Estimasi p (p̂) Bernoulli:", round(p_hat, 4), "\n")
  return(p_hat)
}


#' Plot PMF Distribusi Bernoulli
#'
#' @param p Probabilitas sukses
#' @export
plot_pmf_bernoulli <- function(p) {
  x <- c(0, 1)
  y <- c(1 - p, p)

  barplot(y,
          names.arg = x,
          col = c("tomato", "lightgreen"),
          ylim = c(0, 1),
          main = paste("PMF Bernoulli (p =", p, ")"),
          xlab = "x", ylab = "P(X = x)")
}

#' Plot CDF Distribusi Bernoulli
#'
#' @param p Probabilitas sukses
#' @export
plot_cdf_bernoulli <- function(p) {
  x <- c(-1, 0, 1, 2)
  y <- c(0, 1 - p, 1, 1)

  plot(x, y, type = "s", col = "blue", lwd = 2,
       main = paste("CDF Bernoulli (p =", p, ")"),
       xlab = "x", ylab = "P(X ≤ x)", ylim = c(0, 1.1))
  abline(h = 1, col = "gray", lty = 2)
}


#' Plot Distribusi Frekuensi Data Bernoulli
#'
#' @param data Vektor data Bernoulli (berisi 0 dan 1)
#' @return Barplot frekuensi 0 dan 1
#' @export
plot_bernoulli <- function(data) {
  if (!all(data %in% c(0, 1))) {
    stop("Data harus berupa 0 dan 1 (Bernoulli).")
  }

  barplot(table(factor(data, levels = c(0, 1))),
          col = c("tomato", "lightgreen"),
          names.arg = c("Gagal (0)", "Sukses (1)"),
          main = "Distribusi Frekuensi Bernoulli",
          ylab = "Frekuensi",
          xlab = "Kategori")
}


