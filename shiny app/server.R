library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(data.table)
library(zipcode)
library(ggplot2)

function(input, output, session){
  
  myfunction <- function(state_name, product_name,data){
    #data = fread("newdata.csv")
    my_data = data %>% filter(State == state_name, Product == product_name ) %>% group_by(Company) %>% count() 
    sort_company = arrange(my_data, nn)
    sort_company$order = 1:nrow(sort_company)
    order_summary = select(sort_company, Company, order)
    
    my_data2 =  data %>% filter(State == state_name, Product == product_name )
    w_index = c()
    issue_kind = unique(my_data2$Issue)
    for (i in 1:length(issue_kind)){
      time = sum(my_data2$Issue == issue_kind[i])
      if (time >= 4){
        w_index = cbind(w_index, as.character(issue_kind[i]))
      }
    }
    
    
    p=""
    for (i in 1:length(w_index)){
      issue_i= my_data2 %>% filter(Issue == w_index[i] )
      sort_company = arrange(issue_i, n)
      sort_company$order = 1:nrow(sort_company)
      order_summary = select(sort_company, Company, order)
      
      indentWidth = .8
      histBarGeom = (geom_bar(colour="black", fill="#DD8888", width=indentWidth,
                              stat="identity"))

      k=paste("We recommend", issue_i$Company[1], "for your concerning issue:", issue_i$Issue[1],
              "associated with your product:" , product_name)
      p=paste(p,k,sep = "\n")
    }
    return(p)
  }
  
  myfunction2 <- function(state_name, product_name,data){
    #data = fread("newdata.csv")
    my_data = data %>% filter(State == state_name, Product == product_name ) %>% group_by(Company) %>% count() 
    sort_company = arrange(my_data, nn)
    sort_company$order = 1:nrow(sort_company)
    order_summary = select(sort_company, Company, order)
    
    my_data2 =  data %>% filter(State == state_name, Product == product_name )
    w_index = c()
    issue_kind = unique(my_data2$Issue)
    for (i in 1:length(issue_kind)){
      time = sum(my_data2$Issue == issue_kind[i])
      if (time >= 4){
        w_index = cbind(w_index, as.character(issue_kind[i]))
      }
    }
    
    
    p=""
    for (i in 1:length(w_index)){
      issue_i= my_data2 %>% filter(Issue == w_index[i] )
      sort_company = arrange(issue_i, n)
      sort_company$order = 1:nrow(sort_company)
      order_summary = select(sort_company, Company, order)
      
      indentWidth = .8
      histBarGeom = (geom_bar(colour="black", fill="#DD8888", width=indentWidth,
                              stat="identity"))
      a=ggplot(data = issue_i, aes(x=reorder(issue_i$Company,-issue_i$n), y=issue_i$n)) +
        histBarGeom + coord_flip() + guides(fill=FALSE) +
        xlab("Company") + ylab("Density") +
        ggtitle(paste("Issue: ", w_index[i]))
    }
    return(a)
  }
  

  
  
  output$histamount = renderPlot({
    zipcode = as.character(input$zip)
    product = input$product
    state = input$state
    
    
    aaa=myfunction2(state,product,newdata)
    aaa
    
  })
  
  output$reviewtext = renderText({
    
    zipcode = as.character(input$zip)
    product = input$product
    state = input$state
    
   
   aaa=myfunction(state,product,newdata)
   print(aaa)
  })
  
  output$map = renderLeaflet({
    leaflet() %>% 
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>% 
      setView(lat=43.07621,lng=-89.38458,zoom=4)
    })
  
  observe({
    
    zipcode = as.character(input$zip)
    product = input$product
    state = input$state
    
    b = which(a$zipcode == zipcode)
    c = which(a[b,]$none == 1)
    
    leafletProxy("map") %>% 
      clearShapes() %>%
      setView(lat = 43.07621,lng=-89.38458,zoom=7) %>% 
      addCircles(lat=a[c,]$lat,lng=a[c,]$lng,radius=50,
                 stroke=TRUE, color="#03F",weight=5,opacity=0.5,
                 fillOpacity = 0.4,layerId = a[c,]$id)
  })

  showZipcodePopup <- function(zipid, lat, lng,zipdata) {
    selectedbranch<- zipdata[zipdata$id == zipid,]
    content <- as.character(tagList(
      tags$h4("Name:", selectedbranch$Bank),
      tags$strong("Address",selectedbranch$Address), tags$br(),
      sprintf(selectedbranch$review)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }
  

  
  a= data.frame(
    "zipcode"=c("53703","53703","53703","53703","53703"),
    "none"=c(1,0,1,0,0),
    "Address"=c("a","d","r","e","s"),
    "Bank"=c("U.S. Bank","Bank of America", "Chase Bank", "Citi bank", "Wells fargo"),
    "lat"=c(43.07537,0,43.07621,0,0),
    "lng"=c(-89.38197,0,-89.38458,0,0),
    "review"=c("a","b","c","d","e"),
    "id"=c("53703-1","53703-2","53703-3","53703-4","53703-5")
  )
  
  observe({
    leafletProxy("map") %>% clearPopups()
    event = input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
    content = as.character(tagList(
      tags$h4("Branchname"),
      tags$strong("Address"),
      tags$br(),
      sprintf("review"),tags$br()
    ))
    
    leafletProxy("map") %>% clearPopups()
    leafletProxy("map") %>% addPopups(lng=event$lng,lat=event$lat,content) %>% 
      setView(lat = 43.07621,lng=-89.38458,zoom=10)
    })
  })
  
}
