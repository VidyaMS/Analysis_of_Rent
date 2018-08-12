#
# This is a Shiny web application for displaying the rent data for apartments around Yelahanka, Bangalore.
# The details such as Apartment name , furnishing can be selected so that the required 
# detail is displayed .
#   
#
##############################################################################################
library(shiny)
library(tidyverse)

## Read in the data .
rent.db <- read.csv("Yelahanka rent.csv" , header = TRUE , stringsAsFactors = FALSE)
rent.db <- rent.db %>% filter(Apt_name != "NA")
rent.db$Availability <- as.factor(rent.db$Availability) 

## Create the list of available furnishings for selection.
apt_furnishing <- levels(as.factor(rent.db$Furnishing))
apt_furnishing <- c("All" , apt_furnishing)

## Create the list of apartment names for selection.
apt.name <- levels(as.factor(rent.db$Apt_name))
apt.name <- c("All", apt.name )


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Rent Analysis for a 3BHK Apartment in Yelahanka, Bangalore "),
   
   inputPanel(
     selectInput("furnishing", label = "Furnishing:",
                 choices = apt_furnishing , selected = "All"),
     
     selectInput("apt_name", label = "Apartment Name:",
                 choices = apt.name , selected = "All")
   ),
      
      # Show a plot of the generated distribution
      mainPanel(
        
        
        tabsetPanel(type = "tabs",
                    tabPanel("Rent data ", br(), h3(textOutput("msg1")), plotOutput("p1" , height = 600 , width = 900 )),
                    tabPanel("Rent Modelling", br(), plotOutput("modelplot"))
                    )
       
        
      
   )
)

# Server logic to select the rent data records based on the input value .
server <- function(input, output) {
   
  ## Adding the reactive component which filters the rent data based on the input 
  
  mydata <- reactive({
    
    
    required.db <- rent.db 
    
  })
  
  ## check for selected Apartment name  
  check_apt <- reactive({
    
    apartment <- input$apt_name
    
    if(apartment != "All") {
      
      required.db <- mydata() %>% dplyr::filter(Apt_name == apartment)
    }
    else required.db <- mydata()
    return(required.db)
    
  })
  
  ## Check for selected furnishing.
  check_furnish <- reactive({
    
    furnish <- input$furnishing
    if(furnish != "All") {
      
      required.db <- check_apt() %>% dplyr::filter(Furnishing == furnish)
    }
    else required.db <- check_apt()
    return(required.db)
    
  })
    
    ## plot the values of rent and SBA for the apartments. 
  
    output$p1 <- renderPlot({
    
      filtered_data <- check_furnish()
      
    if( nrow(filtered_data) > 0 ){
      
    output$msg1 <- renderText("Data available")  
    
    create_title <- paste("SBA Vs Rent for apartment : ", input$apt_name , sep ="")
    create_title <- paste(create_title , "and furnishing ", sep = " ")
    create_title <- paste(create_title , input$furnishing , sep =" ")
    
    
    ggplot(data = filtered_data,  aes(SBA , Rent)) + geom_bar(aes(fill = Apt_name) , stat = "identity" , width = 15) + labs( x= "SBA (sq ft )" , y = "Rent (Rs)" ,title  = create_title) + facet_wrap( ~ Availability , ncol = 1 )
    
    }
    else { output$msg1 <- renderText("No data available for the selected criteria")}
     
  })
  
    ## plot the SBA Vs Rent scatter plot with the regression line .
  output$modelplot <- renderPlot({
    
    filtered_data <- check_furnish()
    
    if( nrow(filtered_data) > 0 ){
      
      create_title <- paste("SBA Vs Rent for apartment : ", input$apt_name , sep ="")
      create_title <- paste(create_title , "and furnishing ", sep = " ")
      create_title <- paste(create_title , input$furnishing , sep =" ")
                            
    ggplot(data = filtered_data,  aes(SBA , Rent)) + geom_point(size = 3, color = "green") + geom_smooth(method = "lm" , se = FALSE) +labs(title = create_title , x = "SBA (sq ft)" , y = "Rent (Rs)")
    
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

