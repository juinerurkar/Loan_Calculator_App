library(shiny)
library(dplyr)
library(DT)
library(ggplot2)
library(reshape2)
library(scales)
options(scipen = 999)

# Define UI for slider demo app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Loan Calculator"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar to demonstrate various slider options ----
    sidebarPanel(
      
      # Input: Simple integer interval ----
      numericInput("principal", "Principal", 500000, min = 0, step = 1000),
      hr(),
      numericInput("interest", "Annual interest rate (%)", 6, min = 0, max = 100, step = 0.01),
      hr(),
      sliderInput("length", "Duration of the loan (in years)",
                  min = 0,
                  max = 30,
                  value = 10,
                  step = 1
      ),
      hr(),
      checkboxInput("pieplot", "Pie plot", TRUE),
      hr(),
      checkboxInput("plot", "Bar plot", TRUE),
      hr(),
      h3("Formula for loan calculator"),
      tags$div("Let P = Principal,", tags$br(),
               "R = Rate of interest per annum,", tags$br(), 
               "r = Rate of interest per month,", tags$br(), 
               "I = Monthly installment,",tags$br(), "B = Balance of the loan amount.", tags$br(),
               "B(0) = P", tags$br(),
               "B(1) = P(1+r) - I", tags$br(),
               "B(2) = (1+r)[P(1+r) - I] - I", tags$br(),"= P(1+r)", tags$sup("2")," - I(1+r) - I",tags$br(),
      "B(3) = (1+r)[P(1+r)",tags$sub("2")," - I(1+r) - I] - I", tags$br(), "= P(1+r)", tags$sup("3")," - I(1+r)", tags$sup("2") , " - I(1+r) - I", tags$br(),"We continue this process till balance equals 0", tags$br(),
      "B(n) = P(1+r)", tags$sup("n"), " - I(1+r)", tags$sup("n-1"), " - I(1+r)", tags$sup("n-2"), " ... - I(1+r) - I" , tags$br(),"= P(1+r)", tags$sup("n"), " - I [(1+r)", tags$sup("n"), " - 1]/[i]",tags$br(), " = 0", tags$br(),
      "Solving the last equation, we get,", tags$br(), "I = (P X r X (1+r)",tags$sup("n"),")/((1+r)",tags$sup("n"), " - 1)"),
      hr()
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Table summarizing the values entered ----
      uiOutput("text"),
      br(),
      plotOutput("piePlot"),
      br(),
      plotOutput("barPlot"),
      br(),
      DT::dataTableOutput("loan_table"),
      br(),
      p(em("Disclaimer: All the information in this app is published in good faith and for general information only. I do not make any warranties about the completeness, reliability and accuracy of this information. Any action taken upon the information you find on this app is strictly at your own risk. I shall not be liable for any losses and/or damages in connection with the use of this app. This R Shiny app is partially based on the R code of Prof. Thomas Girke.")),
      br(),
      br()
    )
  )
)

# Define server logic for slider examples ----
server <- function(input, output) {
  mortgage <- function(P = 500000, I = 6, L = 30, amort = TRUE, plotData = TRUE) {
    J <- I / (12 * 100)
    N <- 12 * L
    M <- (P*J*(1+J)^N)/(((1+J)^N) - 1)
    monthPay <<- M
    if(P == 0){
      print("Principal(loan amount) cannot be 0")
    } 
    else{# Calculate Amortization for each Month
    if (amort == TRUE) {
      Pt <- P # current principal or amount of the loan
      currP <- NULL
      while (Pt >= 0) {
        H <- Pt * J # this is the current monthly interest
        C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
        Q <- Pt - C # this is the new balance of your principal of your loan
        Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
        currP <- c(currP, Pt)
      }
      monthP <- c(P, currP[1:(length(currP) - 1)]) - currP
      aDFmonth <<- data.frame(
        Month = 1:length(currP),
        Year = sort(rep(1:ceiling(N / 12), 12))[1:length(monthP)],
        Balance = c(currP[1:(length(currP))]),
        Payment = monthP + c((monthPay - monthP)[1:(length(monthP))]),
        Principal = monthP,
        Interest = c((monthPay - monthP)[1:(length(monthP))])
      )
      aDFmonth <<- subset(aDFmonth, Year <= L * 12)
      aDFyear <- data.frame(
        Amortization = tapply(aDFmonth$Balance, aDFmonth$Year, max),
        Annual_Payment = tapply(aDFmonth$Payment, aDFmonth$Year, sum),
        Annual_Principal = tapply(aDFmonth$Principal, aDFmonth$Year, sum),
        Annual_Interest = tapply(aDFmonth$Interest, aDFmonth$Year, sum),
        Year = as.factor(na.omit(unique(aDFmonth$Year)))
      )
      aDFyear <<- aDFyear
    }
    if (plotData == TRUE) {
      aDFyear2 <- melt(aDFyear[, c("Annual_Interest", "Annual_Principal", "Year")], id.vars = "Year")
      ggplot(aDFyear2, aes(x = Year, y = value, fill = variable)) +
        geom_bar(position = "fill", stat = "identity") +
        labs(y = "\nAnnual Payment", title = "Proportions of Interest and Principal in Annual Payments") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
        theme_minimal() +
        theme(legend.position = "top", plot.title = element_text(hjust = 0.5, face = "bold", size = 13.5), axis.title = element_text(size = 12), axis.text = element_text(size = 11), axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")), legend.text = element_text(size = 12)) +
        scale_fill_discrete(name = "", labels = c("Annual Interest", "Annual Principal"))
    }
    }
  }
  mortgage2 <- function(P = 500000, I = 6, L = 30, amort = TRUE, plotData = TRUE) {
    J <- I / (12 * 100)
    N <- 12 * L
    M <- (P*J*(1+J)^N)/(((1+J)^N) - 1)
    monthPay <<- M
    if(P == 0){
      print("Principal(loan amount) cannot be 0")
    }
    else{# Calculate Amortization for each Month
    if (amort == TRUE) {
      Pt <- P # current principal or amount of the loan
      currP <- NULL
      while (Pt >= 0) {
        H <- Pt * J # this is the current monthly interest
        C <- M - H # this is your monthly payment minus your monthly interest, so it is the amount of principal you pay for that month
        Q <- Pt - C # this is the new balance of your principal of your loan
        Pt <- Q # sets P equal to Q and goes back to step 1. The loop continues until the value Q (and hence P) goes to zero
        currP <- c(currP, Pt)
      }
      monthP <- c(P, currP[1:(length(currP) - 1)]) - currP
      aDFmonth <<- data.frame(
        Month = 1:length(currP),
        Year = sort(rep(1:ceiling(N / 12), 12))[1:length(monthP)],
        Balance = c(currP[1:(length(currP))]),
        Payment = monthP + c((monthPay - monthP)[1:(length(monthP))]),
        Principal = monthP,
        Interest = c((monthPay - monthP)[1:(length(monthP))])
      )
      aDFmonth <<- subset(aDFmonth, Year <= L * 12)
      aDFyear <- data.frame(
        Amortization = tapply(aDFmonth$Balance, aDFmonth$Year, max),
        Annual_Payment = tapply(aDFmonth$Payment, aDFmonth$Year, sum),
        Annual_Principal = tapply(aDFmonth$Principal, aDFmonth$Year, sum),
        Annual_Interest = tapply(aDFmonth$Interest, aDFmonth$Year, sum),
        Year = as.factor(na.omit(unique(aDFmonth$Year)))
      )
      aDFyear <<- aDFyear
    }
    if (plotData == TRUE) {
      aDFyear2 <- aDFyear %>%
        rename(
          Interest = Annual_Interest,
          Payment = Annual_Payment,
          Principal = Annual_Principal
        )
      aDFyear2$Year <- as.factor(aDFyear2$Year)
      aDFyear2 <- melt(aDFyear2[, c("Interest", "Principal", "Year")], id.vars = "Year")
      
      slices<- c(sum(aDFyear2$value[aDFyear2$variable == "Interest"]), sum(aDFyear2$value[aDFyear2$variable == "Principal"]))
      labels = c("Interest", "Principal")
      pie(slices, labels, main = "Proportions of Interest and Principal in Total Amount Repaid", col = c("#F8766D", "#00BFC4"))
    }
    }
  }
  
  output$text <- renderUI({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
    HTML(paste0("<b>", "Monthly payment: ", format(round(monthPay, digits = 2), big.mark = ","), "</b>",
      "<br>",
      "<b>", "Total cost: ", "</b>", format(round(input$principal, 2), big.mark = ","), " (principal) + ", format(round(monthPay * 12 * input$length - input$principal, 2), big.mark = ","), " (interest) = ", "<b>", format(round(monthPay * 12 * input$length, digits = 2), big.mark = ","), "</b>"
    ))
  })
  
  output$piePlot <- renderPlot({
    mortgage2(P = input$principal, I = input$interest, L = input$length, plotData = input$pieplot)
  })
  
  output$barPlot <- renderPlot({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = input$plot)
  })
  

  # Loan table
  output$loan_table <- DT::renderDataTable({
    mortgage(P = input$principal, I = input$interest, L = input$length, plotData = FALSE)
    df_month <- DT::datatable(data = data.frame(round(aDFmonth, 2)),
                              extensions = "Buttons",
                              options = list(
                                dom = "Blrtip",
                                buttons = list("copy", list(extend = "collection", buttons = c("csv", "pdf", "print"), text = "Download")),lengthMenu = list( c(10, 20, 30, 40, 50, 70, 100, -1) # declare values
                                                   , c(10, 20, 30, 40, 50, 70, 100, "All") # declare titles
                                                   ))
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
