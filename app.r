library(shiny)
library(tidyverse)
library(MASS)
library(quantmod)
library(PerformanceAnalytics)

# -----------------------------
# Helper: simulate correlated returns
# -----------------------------
sim_returns <- function(years, vt_mean, vt_sd, bnd_mean, bnd_sd, corr) {
  cov_mat <- matrix(c(vt_sd^2, corr * vt_sd * bnd_sd,
                      corr * vt_sd * bnd_sd, bnd_sd^2), nrow = 2)
  draws <- mvrnorm(n = years, mu = c(vt_mean, bnd_mean), Sigma = cov_mat)
  colnames(draws) <- c("vt", "bnd")
  as.data.frame(draws)
}

# -----------------------------
# Helper: simulate one path
# -----------------------------
simulate_path <- function(start_value, contrib, glidepath,
                          vt_mean, vt_sd, bnd_mean, bnd_sd, corr) {
  
  vals <- numeric(nrow(glidepath) + 1)
  vals[1] <- start_value
  
  vt_value  <- start_value * glidepath$vt[1]
  bnd_value <- start_value * glidepath$bnd[1]
  
  rets <- sim_returns(nrow(glidepath), vt_mean, vt_sd, bnd_mean, bnd_sd, corr)
  
  for (t in 1:nrow(glidepath)) {
    vt_value  <- vt_value  * (1 + rets$vt[t])
    bnd_value <- bnd_value * (1 + rets$bnd[t])
    
    if (contrib != 0) {
      vt_value  <- vt_value  + contrib * glidepath$vt[t]
      bnd_value <- bnd_value + contrib * glidepath$bnd[t]
    }
    
    total <- vt_value + bnd_value
    vt_value  <- total * glidepath$vt[t]
    bnd_value <- total * glidepath$bnd[t]
    
    vals[t + 1] <- total
  }
  
  tibble(year = 0:nrow(glidepath), value = vals)
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("VT/BND Glidepath Monte Carlo Simulator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("start_value", "Starting Balance", 7000),
      numericInput("contrib", "Annual Contribution", 0),
      numericInput("years", "Years to Retirement", 15),
      numericInput("sims", "Number of Simulations", 2000),
      
      h4("Glidepath Customization"),
      sliderInput("start_vt", "Start VT Allocation (%)", min = 50, max = 100, value = 90),
      sliderInput("end_vt",   "End VT Allocation (%)",   min = 20, max = 80,  value = 60),
      
      h4("Return Assumptions"),
      numericInput("vt_mean", "VT Mean Return", 0.07),
      numericInput("vt_sd",   "VT Std Dev",     0.18),
      numericInput("bnd_mean","BND Mean Return",0.03),
      numericInput("bnd_sd",  "BND Std Dev",    0.06),
      numericInput("corr",    "Correlation",    0.1),
      
      actionButton("run", "Run Simulation")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Glidepath", tableOutput("glidepath")),
        tabPanel("Example Path", plotOutput("path_plot")),
        tabPanel("Distribution", plotOutput("dist_plot")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Historical Yields",
                 fluidRow(
                   column(
                     4,
                     selectizeInput(
                       "ticker", "Ticker",
                       choices = NULL,
                       options = list(
                         placeholder = "Type or select a ticker (e.g., VT, BND, SPY)",
                         create = TRUE
                       )
                     ),
                     dateRangeInput("hist_range", "Date Range",
                                    start = Sys.Date() - 365*5,
                                    end   = Sys.Date()),
                     selectInput("hist_freq", "Return Frequency",
                                 choices = c("Daily" = "daily",
                                             "Monthly" = "monthly",
                                             "Yearly" = "yearly"),
                                 selected = "monthly"),
                     actionButton("load_hist", "Load Historical Data")
                   ),
                   column(
                     8,
                     plotOutput("hist_plot"),
                     tableOutput("hist_table")
                   )
                 )
        )
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  
  # Preload some common tickers for autocomplete
  common_tickers <- c("VT","BND","VTI","VXUS","VOO","BNDX","QQQ","SPY","AGG","IWM")
  updateSelectizeInput(session, "ticker", choices = common_tickers, server = TRUE)
  
  # Dynamic glidepath generator
  glidepath <- reactive({
    years <- input$years
    start_vt <- input$start_vt / 100
    end_vt   <- input$end_vt   / 100
    
    vt_vec  <- seq(start_vt, end_vt, length.out = years)
    bnd_vec <- 1 - vt_vec
    
    tibble(
      year = 1:years,
      vt   = vt_vec,
      bnd  = bnd_vec
    )
  })
  
  output$glidepath <- renderTable(glidepath())
  
  # Run simulations
  sims <- eventReactive(input$run, {
    replicate(input$sims,
              tail(simulate_path(
                start_value = input$start_value,
                contrib     = input$contrib,
                glidepath   = glidepath(),
                vt_mean     = input$vt_mean,
                vt_sd       = input$vt_sd,
                bnd_mean    = input$bnd_mean,
                bnd_sd      = input$bnd_sd,
                corr        = input$corr
              )$value, 1))
  })
  
  # Example path
  output$path_plot <- renderPlot({
    path <- simulate_path(
      start_value = input$start_value,
      contrib     = input$contrib,
      glidepath   = glidepath(),
      vt_mean     = input$vt_mean,
      vt_sd       = input$vt_sd,
      bnd_mean    = input$bnd_mean,
      bnd_sd      = input$bnd_sd,
      corr        = input$corr
    )
    
    ggplot(path, aes(year, value)) +
      geom_line(color = "steelblue", size = 1.2) +
      theme_minimal() +
      labs(title = "Example Simulation Path",
           x = "Year", y = "Portfolio Value")
  })
  
  # Distribution plot
  output$dist_plot <- renderPlot({
    df <- tibble(final = sims())
    
    ggplot(df, aes(final)) +
      geom_histogram(bins = 40, fill = "darkgreen", alpha = 0.7) +
      theme_minimal() +
      labs(title = "Distribution of Final Portfolio Values",
           x = "Final Value", y = "Count")
  })
  
  # Summary stats
  output$summary <- renderPrint({
    summary(sims())
    cat("\nQuantiles:\n")
    print(quantile(sims(), probs = c(.1, .25, .5, .75, .9)))
  })
  
  # -----------------------------
  # Historical yields tab
  # -----------------------------
  hist_data <- eventReactive(input$load_hist, {
    req(input$ticker)
    sym <- toupper(input$ticker)
    
    getSymbols(
      sym,
      src = "yahoo",
      from = input$hist_range[1],
      to   = input$hist_range[2],
      auto.assign = FALSE
    )
  })
  
  hist_returns <- reactive({
    xt <- hist_data()
    freq <- input$hist_freq
    
    prices <- Cl(xt)
    
    if (freq == "daily") {
      ret <- dailyReturn(prices)
    } else if (freq == "monthly") {
      ret <- monthlyReturn(prices)
    } else {
      ret <- yearlyReturn(prices)
    }
    
    ret
  })
  
  output$hist_plot <- renderPlot({
    req(hist_returns())
    ret <- hist_returns()
    
    df <- tibble(
      date = index(ret),
      ret  = as.numeric(ret)
    )
    
    ggplot(df, aes(date, ret)) +
      geom_col(fill = "steelblue") +
      theme_minimal() +
      labs(title = paste("Historical", input$hist_freq, "returns for", toupper(input$ticker)),
           x = "Date", y = "Return") +
      scale_y_continuous(labels = scales::percent)
  })
  
  output$hist_table <- renderTable({
    req(hist_returns())
    ret <- hist_returns()
    
    tibble(
      date = index(ret),
      return = as.numeric(ret)
    ) %>%
      mutate(return = scales::percent(return, accuracy = 0.01))
  })
}

shinyApp(ui, server)
