library(shiny)
library(shinythemes)
library(leaflet)
library(dplyr)

ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage("BMI Calculator",
                           tabPanel("About", 
                                    div(includeMarkdown("about.md"), 
                                        align="justify")
                           ),
                           tabPanel("Home",
                                    # Input values
                                    sidebarPanel(
                                      radioButtons("units", label = h4("Units"),
                                                   choices = list("cm / kg" = 1, "inches / lb" = 2), 
                                                   selected = 1),
                                      h5('1 inch = 2.54 cm'),
                                      h5('1 kg = 2.205 lbs'),
                                      numericInput("height", label = "Your Height", value = 165, min = 0),
                                      numericInput("weight", label = "Your Weight", value = 50, min = 0),
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Output')), # Status/Output Text Box
                                      verbatimTextOutput('contents'),
                                      tableOutput('tabledata'), # Results table
                                      div(style="display: inline-block;vertical-align:top; width: 300px;",uiOutput("ok")),
                                      div(style="display: inline-block;vertical-align:top; width: 400px;",uiOutput("gif"))
                                    )
                           ), #tabPanel(), Home
                           
                           tabPanel("BMI World Map",
                                    # Input values
                                    mainPanel(
                                      titlePanel(h2("BMI World Map 2016")),
                                      titlePanel(h4("The map shows the prevalence of obesity among adults in 2016 collected by World Health Organization (WHO).")),
                                      titlePanel(h4("The gradient colour of the dot map represent the the gradient from highest BMI average to the lowest index.")),
                                      leafletOutput("map", width = "1400px", height = "500px")
                                    )
                           ),
                           
                           tabPanel("Underweight", 
                                    titlePanel("Underweight"), 
                                    div(includeMarkdown("underweight.md"), 
                                        align="justify")
                           ),
                           tabPanel("Normal", 
                                    titlePanel("Normal"), 
                                    div(includeMarkdown("normal.md"), 
                                        align="justify")
                           ),
                           tabPanel("Overweight", 
                                    titlePanel("Overweight"), 
                                    div(includeMarkdown("overweight.md"), 
                                        align="justify")
                           )
                ) # navbarPage()
) # fluidPage()

server <- function(input, output, session) {
  # Input Data
  datasetInput <- reactive({  
    
    if(input$units == 1){
      bmi <- input$weight/((input$height/100)* (input$height/100))
    }
    else if(input$units == 2){
      bmi <- ((input$weight / (input$height * input$height)) * 703)
    }
    bmi <- data.frame(bmi)
    names(bmi) <- "BMI"
    print(bmi)
  })
  
  checking <- reactive({  
    
    if(input$units == 1){
      bmi <- input$weight/((input$height/100)* (input$height/100))
    }
    else if(input$units == 2){
      bmi <- ((input$weight / (input$height * input$height)) * 703)
    }
    return(bmi)
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    datasetInput()
    
  })
  
  output$ok <- renderUI({
    if(checking()<18.5)
      h2("Underweight",br(),tags$image(src= 'underweight.png', type = 'image/png', width='200px',height='400px'))
    else if(checking()<25)
      h2("Normal",br(),tags$image(src= 'healthyweight.png', type = 'image/png', width='200px',height='400px'))
    else if(checking()<30)
      h2("Overweight",br(),tags$image(src= 'overweight.png', type = 'image/png', width='200px',height='400px'))
    else if(checking()<35)
      h2("Obese",br(),tags$image(src= 'obese.png', type = 'image/png', width='200px',height='400px'))
    else if(checking()>=35)
      h2("Extreamely Obese",br(),tags$image(src= 'extreamelyobese.png', type = 'image/png', width='200px',height='400px'))
  })
  
  output$gif <- renderUI({
    if(checking()<18.5)
      h2("Quick Tips",br(),tags$image(src= 'underweight.gif', type = 'image/png', width='600px',height='400px'))
    else if(checking()<25)
      h2("Quick Tips",br(),tags$image(src= 'normal.gif', type = 'image/png', width='600px',height='400px'))
    else if(checking()>=25)
      h2("Quick Tips",br(),tags$image(src= 'overweight.gif', type = 'image/png', width='600px',height='400px'))
  })
  
  output$map <- renderLeaflet({
    dataBMI <- read.csv("data 1.csv")
    dataBMI <- dataBMI%>%mutate(popup_info = paste("Prevelence Obesity among adults in",Location,"<br/>",Value))
    colours <- c("pink","red")
    pal <- colorFactor(colours, dataBMI$FactValueNumeric)
    leaflet()%>%addTiles()%>% addCircleMarkers(data = dataBMI, lng = ~longitude, lat = ~latitude, radius = ~3,popup = ~popup_info, color = ~pal(FactValueNumeric))
  }) 

}

shinyApp(ui = ui, server = server)