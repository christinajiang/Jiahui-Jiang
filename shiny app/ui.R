library(leaflet)
library(shiny)

vars2 = c(
  "Bank account or service"    ,                                                 
  "Checking or savings account"  ,                                               
  "Consumer Loan"                ,                                               
  "Credit card or prepaid card" ,                                                
  "Credit reporting, credit repair services, or other personal consumer reports",
  "Debt collection"                                         ,                    
  "Money transfer, virtual currency, or money service"    ,                      
  "Mortgage"                                            ,                        
  "Other financial service"                              ,                       
  "Payday loan, title loan, or personal loan"             ,                      
  "Student loan"                                           ,                     
  "Vehicle loan or lease" 
  
  
)

vars3 = state.abb

navbarPage("Bank Selection", id="nav",
           
  tabPanel("Interactive map",
    div(class="outer",
      
        tags$head(
          # Include our custom CSS
          includeCSS("styles.css")
        ),
        
        # If not using custom CSS, set height of leafletOutput to a number instead of percent
        leafletOutput("map", width="84%", height="100%"),
    
      absolutePanel(id="controls", class="panel panel-default", fixed = TRUE, 
                  draggable = TRUE, top=60, left = "auto", right=20, bottom = "auto",
                  width = 330, height = "auto",
                  
                  
      h2("Select your ZIP and product"),
      
      numericInput("zip","ZIP",0),
      selectInput("state","STATE", vars3,selected = "NY"),
      selectInput("product", "PRODUCT", vars2,selected = "Mortgage"),
      
      plotOutput("histamount", height = 400),
      verbatimTextOutput("reviewtext")
    )
   )       
  )
    
)
