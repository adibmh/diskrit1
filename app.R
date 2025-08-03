library(shiny)

# ===== Fungsi Distribusi Bernoulli =====
pmf_bernoulli <- function(x, p) {
  ifelse(x == 1, p, 1 - p)
}

cdf_bernoulli <- function(x, p) {
  ifelse(x < 0, 0, ifelse(x < 1, 1 - p, 1))
}

estimasi_p_bernoulli <- function(data) {
  if (!all(data %in% c(0, 1))) {
    stop("Data hanya boleh berisi 0 dan 1 untuk distribusi Bernoulli.")
  }
  round(mean(data), 4)
}

# ===== Fungsi Distribusi Binomial =====
pmf_binomial <- function(x, size, p) {
  if (x < 0 || x > size || p < 0 || p > 1) {
    return(0)
  }
  choose(size, x) * p^x * (1 - p)^(size - x)
}

cdf_binomial <- function(x, size, p) {
  if (x < 0) return(0)
  sum(sapply(0:floor(x), function(k) {
    choose(size, k) * p^k * (1 - p)^(size - k)
  }))
}

estimasi_p_binomial <- function(data, size) {
  if (any(data < 0) || any(data > size)) {
    stop("Nilai data harus di antara 0 dan size.")
  }
  round(mean(data) / size, 4)
}

# ===== UI =====
ui <- fluidPage(
  titlePanel("Distribusi Bernoulli dan Binomial"),
  sidebarLayout(
    sidebarPanel(
      h4("Bernoulli"),
      numericInput("bx", "x (0 atau 1):", value = NULL),
      helpText("x adalah hasil dari satu percobaan (0 = gagal, 1 = sukses)"),

      numericInput("bp", "Probabilitas sukses (p):", value = NULL),
      helpText("p adalah peluang terjadinya sukses dalam percobaan"),

      textInput("b_data", "Data Bernoulli:", value = "", placeholder = "contoh: 1,0,1,1,0"),
      helpText("Masukkan data hasil percobaan Bernoulli (hanya 0 dan 1)"),

      hr(),
      h4("Binomial"),
      numericInput("binx", "x (jumlah sukses):", value = NULL),
      helpText("x adalah jumlah sukses yang diamati dari n percobaan"),

      numericInput("binsize", "Jumlah percobaan (n):", value = NULL),
      helpText("n adalah total percobaan yang dilakukan"),

      numericInput("binp", "Probabilitas sukses (p):", value = NULL),
      helpText("p adalah peluang sukses dalam satu percobaan"),

      textInput("bin_data", "Data jumlah sukses:", value = "", placeholder = "contoh: 2,3,1,4"),
      helpText("Masukkan jumlah sukses dari beberapa pengamatan binomial"),

      actionButton("hitung", "Hitung")
    ),

    mainPanel(
      h4("Output Bernoulli"),
      tags$h5("PMF f(x):"),
      verbatimTextOutput("bernoulli_pmf"),

      tags$h5("CDF F(x):"),
      verbatimTextOutput("bernoulli_cdf"),

      tags$h5("Estimasi p̂:"),
      verbatimTextOutput("bernoulli_estimasi"),

      hr(),

      h4("Output Binomial"),
      tags$h5("PMF f(x):"),
      verbatimTextOutput("binomial_pmf"),

      tags$h5("CDF F(x):"),
      verbatimTextOutput("binomial_cdf"),

      tags$h5("Estimasi p̂:"),
      verbatimTextOutput("binomial_estimasi")
    )
  )
)

# ===== SERVER =====
server <- function(input, output, session) {
  observeEvent(input$hitung, {
    # ==== Bernoulli ====
    bx <- as.numeric(input$bx)
    bp <- as.numeric(input$bp)
    bern_data <- suppressWarnings(as.numeric(unlist(strsplit(input$b_data, ","))))

    output$bernoulli_pmf <- renderPrint({
      if (!is.na(bx) && !is.na(bp)) pmf_bernoulli(bx, bp)
    })

    output$bernoulli_cdf <- renderPrint({
      if (!is.na(bx) && !is.na(bp)) cdf_bernoulli(bx, bp)
    })

    output$bernoulli_estimasi <- renderPrint({
      if (!any(is.na(bern_data)) && all(bern_data %in% c(0, 1))) {
        p_hat <- estimasi_p_bernoulli(bern_data)
        paste("Estimasi p (p̂) Bernoulli dari data adalah:", p_hat)
      } else if (input$b_data != "") {
        "Data harus berupa angka 0 atau 1 saja."
      }
    })

    # ==== Binomial ====
    binx <- as.numeric(input$binx)
    binsize <- as.numeric(input$binsize)
    binp <- as.numeric(input$binp)
    bin_data <- suppressWarnings(as.numeric(unlist(strsplit(input$bin_data, ","))))

    output$binomial_pmf <- renderPrint({
      if (!is.na(binx) && !is.na(binsize) && !is.na(binp)) {
        pmf_binomial(binx, binsize, binp)
      }
    })

    output$binomial_cdf <- renderPrint({
      if (!is.na(binx) && !is.na(binsize) && !is.na(binp)) {
        cdf_binomial(binx, binsize, binp)
      }
    })

    output$binomial_estimasi <- renderPrint({
      if (!any(is.na(bin_data)) && all(bin_data >= 0 & bin_data <= binsize)) {
        p_hat <- estimasi_p_binomial(bin_data, binsize)
        paste("Estimasi p (p̂) Binomial dari data adalah:", p_hat)
      } else if (input$bin_data != "") {
        paste("Data harus antara 0 dan", binsize)
      }
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
