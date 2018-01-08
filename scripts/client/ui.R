require(ggplot2)
require(plotly)
require(shiny)
require(data.table)
require(shinythemes)
require(shinydashboard)

# Read in data
#load("../../data/sales_flt1000_months2.RData")
#categories = unique(sales_flt1000_months2$ARNIMI)
#categories = c("All", categories)
#save(categories, file="/dm_project2017/data/categories.RData")
load("../../data/categories.RData") # categories
#clientslist10000 = as.character(unique(sales_flt1000_months2$KLIENDIKAARDIKOOD))
#save(clientslist10000, file="/dm_project2017/data/clientslist10000.RData")
load("../../data/clientslist10000.RData") # top 10000 clients

shinyUI(
  navbarPage(title="CLIENT IS KING",
             theme = shinytheme("flatly"),
             #  shinythemes::themeSelector(),
             tabPanel("About",
                      mainPanel(
                        
                        h3("Recommendation system for COOP-Tartu"),
                        br(),
                        p("The goal of this project is to enhance the experience of COOP Tartu costumers by building a customer recommendation system.  
                          Our data includes 1 year sales data of COOP Tartu stores (> 50 million transactions) as well as metadata of products, e.g. broad categories."),
                        p(HTML("The idea is to identify the groups of clients who have similar shopping patterns by applying <b>collaborative filtering</b> methods. 
Using these approaches we can predict and recommend items to clients based on preference similarities.")), 
                        br(),
                        p(HTML("We implemented 2 different approaches based on tutorial \"Building a recommendation system with R\" by Gorakala and Usuelli. For that we used R package called '<b><it>recommenderlab</b></it>'.")),
                        p(HTML("1. <b>User Based Collaborative Filtering (UBCF)</b> aims to find the most similar users and recommends products based on similar customers.")),
                        p(HTML("Algorithm:<ol><li>Measure how similar each user is to the selected one.</li>
                               <li>Identify k most similar users (k=10 in our case).</li>
<li>Rate the items purchased by the most similar users. The rating is the average amount bought among similar users.</li>
<li>Pick the top-rated items.</li>
                               </ol>")),
                        p(HTML("2. <b>Item Based Collaborative Filtering (IBCF)</b> considers user's purchases and recommends similar products.")),
                        p(HTML("Algorithm:<ol><li>For each two items, measure how similar they are in terms of having received similar ratings by similar users</li>
                               <li>For each item, identify the k-most similar items (k=10 in our case)</li>
<li>For every user, identify the items that are most similar to the user's purchases</li>
                               </ol>")),
                        p(HTML("In both of the approaches we use both, <b>Cosine similarity</b> and <b>Pearson correlation</b> as similarity measures and apply these on <b>count</b> and <b>binary data</b>.")),
                        p(HTML("<b>We built 8 models in total.</b>")),
                        p(HTML("Compared to UBCF, IBCF is suitable for larger datasets because once it is built it doesn’t need access to initial data, however building a model takes a lot more time. 
                               UBCF’s accuracy has shown to be slightly better than IBCF but it needs to keep the entire dataset in memory so it is usually preferred in cases where the dataset is not very big.
                               Several empirical studies has shown that Pearson correlation coefficient outperforms other similarity measures in case of UBCF and cosine similarity is superior in the case of IBCF.")),
                        br(),
                        p(HTML("<b>Limitations:</b><ul>
  <li>If a customer hasn't previously purchased anything, then the models are not able to recommend any products.</li>
                               <li>Similarly, if a product has never been purchased, it cannot be recommended. </li>
                               <li>UBCF and IBCF do not take additional information into account in order to improve the recommendations (such as product descriptions)</li>
                               <li>On binary data, Jaccard index has shown to be a good alternative for Cosine and Pearson similarity measures. In case of two items it is computed as the number of users purchasing both of the items divided by the number of users purchasing at least one of them. However, we didn't test this measure in out project.</li>
<li>Time for testing the models was very limited.</li>
                               </ul>")),
                       
                        br(),
                        p(HTML("<h4>Results:</h4>")),
                        fluidRow(
                          column(5,
                                 img(src="ROC.png")
                                 ),
                          column(5, offset = 3,
                                 img(src="ROC_bin.png"))
                        ),
                        fluidRow(
                          column(5,
                                 img(src="prec.png")
                                ),
                          column(5,offset = 3,
                                 img(src="prec_bin.png"))
                        
                        ),
                        br(),
                        p(HTML("<b>Team:</b> Ahto Salumets, Enn Pokk, Liis Kolberg")),
                        p(HTML("<b>Data:</b> 01.12.2016 - 30.11.2017")),
                        p(HTML("<b>GitHub:</b> <a href='https://github.com/liiskolb/dm_project2017'>https://github.com/liiskolb/dm_project2017</a>")),
                        p(HTML("<i>The system was created during the \"Data mining\" course of the Institute of Computer Science in the University of Tartu.</i>")),
                        br(),
                        fluidRow(
                          column(12, align="left",
                                 img(src="coop_ttk.jpg", width="200", style="padding-right: 20px;"),
                                 img(src="UT_logo.png", width="100"))
                        )
                        )
                      
             ),
             tabPanel("Data overview",
                      sidebarPanel(width=3,
                                   selectInput("month", label = "Select month", 
                                               choices = c("All" = "all",
                                                           "January" = "January", 
                                                           "February" = "February",
                                                           "March" = "March",
                                                           "April" = "April",
                                                           "May" = "May",
                                                           "June" = "June", 
                                                           "July" = "July",
                                                           "August" = "August",
                                                           "September" = "September",
                                                           "October" = "October",
                                                           "November" = "November", 
                                                           "December"="December"), 
                                               selected = "all"),
                                   selectInput("category", label = "Select product", 
                                               choices = categories, selected="All"),
                                   helpText("Select month to visualise the consumer behaviour in COOP Tartu."),
                                   helpText("To browse deeper into product categories, select product."),
                                   helpText("Data is from 01.12.2016 to 30.11.2017.")
                                   
                                   
                      ),
                      mainPanel(
                        h2("Data overview"),
                        fluidRow(
                          shinydashboard::box(title="Clients", plotOutput("purchases",  width = 600))
                          
                        ),
                        
                        fluidRow(
                          shinydashboard::box(title="Products", plotOutput("purchases2", width = 600))
                          
                        ),
                        
                        br()
                        
                        
                      )
                      
             ),
             tabPanel("Recommender",
                      sidebarPanel(width=3,
                                   selectInput("model", label = "Select recommendation model", choices = c("UBCF_cos", "UBCF_bin_cos", "UBCF_bin_corr", "UBCF_corr", "IBCF_bin_corr", "IBCF_corr", "IBCF_bin_cos", "IBCF_cos"), selected = "UBCF_bin_cos"),
                                   selectInput("client", label = "Select client ID", choices = clientslist10000),
                                   sliderInput("nr_products", label = "Select number of products to recommend", min = 1, max = 5, value = 3),
                                   helpText("UBCF = User Based Collaborative Filtering"),
                                   helpText("IBCF = Item Based Collaborative Filtering"),
                                   helpText("bin = purchases considered only in binary form"),
                                   helpText("If 'bin' not given, then the number of times the product was bought is used as product rating"),
                                   helpText("corr = Pearson correlation distance measure"),
                                   helpText("cos = Cosine distance measure")
                                   
                      ),
                      mainPanel(
                        h2("Recommend products to selected client"),
                        fluidRow(
                          shinydashboard::box(title="Profile", plotOutput("clientprofile", width = 600))
                        ),
                        fluidRow(
                          shinydashboard::box(title="Recommendations", tableOutput("recommend"))
                        ),
                        br()
                      )
             )
  )
)
