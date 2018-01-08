#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
source("helper.R")
#require(data.table)
#require(plotly)

# Read in data
print("Loading data")
load("../../data/sales_flt1000_months2.RData")
load("../../data/products.RData")
rownames(products) <- products$TOOTEID
load("../../data/sales_mat_bin.RData")
load("../../data/sales_mat.RData")

# Load in models
print("Loading models")
load("../../data/models/IBCF_bin_corr.RData")
load("../../data/models/IBCF_bin_cos.RData")
load("../../data/models/IBCF_corr.RData")
load("../../data/models/IBCF_cos.RData")
load("../../data/models/UBCF_bin_corr.RData")
load("../../data/models/UBCF_bin_cos.RData")
load("../../data/models/UBCF_corr.RData")
load("../../data/models/UBCF_cos.RData")

models = list("IBCF_bin_corr" = IBCF_bin_corr, "IBCF_bin_cos" = IBCF_bin_cos, "IBCF_corr"=IBCF_corr, "IBCF_cos" = IBCF_cos, "UBCF_bin_corr"=UBCF_bin_corr, "UBCF_bin_cos"=UBCF_bin_cos, "UBCF_corr"=UBCF_corr, "UBCF_cos"=UBCF_cos )

print("Done")
# Define server logic required to draw a histogram
shinyServer(
  function(input, output, session) {
    output$purchases <- renderPlot({
      clients_per_day(sales_flt1000_months2, sel_month=input$month)
    })
    output$purchases2 <- renderPlot({
      purchases_per_day(sales_flt1000_months2, sel_month=input$month, arname=input$category)
    })
    output$clientprofile <- renderPlot({
      client_profile(sales_flt1000_months2, input$client)
    })
    output$recommend <- renderTable({recommend_client(models[[input$model]], input$client, input$nr_products, products, sales_mat, sales_mat_bin)}, bordered=TRUE, rownames=TRUE, colnames=TRUE, striped=TRUE)
  })
