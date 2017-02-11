require(shiny)
library(deSolve)
library(ggplot2)

source('plots.R')

server <- function(input, output) {
  
    t= 1:100
    
   CO2.emissions <- reactive({
    
    CO2.on = input$CO2.on
    CO2.off = input$CO2.off
    CO2.tk = input$CO2.tk
    CO2.n = input$CO2.n
    CO2.k = input$CO2.k 
    
    function(t){CO2.off + (CO2.on-CO2.off)/(1+(t/CO2.tk)^CO2.n)}
    
  })
   
   CH4.emissions <- reactive({
     
     CH4.on = input$CH4.on
     CH4.off = input$CH4.off
     CH4.tk = input$CH4.tk
     CH4.n = input$CH4.n
     
     function(t){CH4.off + (CH4.on-CH4.off)/(1+(t/CH4.tk)^CH4.n)}
     
     
   })
  
  
  output$plot1 = renderPlot(plot1(t,CO2.emissions()(t)))
  output$plot2 = renderPlot(plot2(t,CH4.emissions()(t)))

  
  CO2.init = 4
  CH4.init = 2
  
  concs <- reactive({
    
    params$CO2.k = input$CO2.k
    params$CH4.k = input$CH4.k
    
    
    odes = function(t,y,params){
  
        dCo2 = CO2.emissions()(t) - params$CO2.k*y[1]
        dCH4 = CH4.emissions()(t) - params$CH4.k*y[2]
        
        
        return(list(c(dCo2,dCH4)))
        
    }
    
    ode(c(CO2.init,CH4.init),t,odes,params)
    
    
  })
  
  output$plot3 <- renderPlot(plot3(concs()))
  

  
  output$plot4 <- renderPlot({
    
    CO2.coef = input$CO2.coef
    CH4.coef = input$CH4.coef
    
    CO2 = concs()[,2]
    CH4 = concs()[,3]
    
    Temp = CO2.coef*CO2 + CH4.coef*CH4
    
    plot5(t,Temp)})
  
  
}

ui <- fluidPage(
  
  fluidRow(
    column(
      width = 4,
      plotOutput('plot1')
          ),
    column(
      width = 8,
      sliderInput('CO2.on','Initial Emission Rate',min=10,max =100,value = 50),
      sliderInput('CO2.off','Final Emission Rate',min=0,max =100,value = 0),
      sliderInput('CO2.tk','Switching Time',min=10,max =100,value = 50),
      sliderInput('CO2.n','Switching Rate',min=10,max =100,value = 50)
      
    )
  ),
  
  
  fluidRow(
    column(
      width = 4,
      plotOutput('plot2')
    ),
    column(
      width = 8,
      sliderInput('CH4.on','Initial Emission Rate',min=10,max =100,value = 50),
      sliderInput('CH4.off','Final Emission Rate',min=0,max =100,value = 0),
      sliderInput('CH4.tk','Switching Time',min=10,max =100,value = 50),
      sliderInput('CH4.n','Switching Rate',min=10,max =100,value = 50)
    )
  ),
  
  fluidRow(
    column(
      width = 4,
      plotOutput('plot3')
    ),
    column(
      width = 8,
      sliderInput('CO2.k','CO2 Decay Rate',min=10,max =100,value = 50),
      sliderInput('CH4.k','CH4 Decay Rate',min=10,max =100,value = 50)
    )
  ),
  
  fluidRow(
    column(
      width = 4,
      plotOutput('plot4')
    ),
    column(
      width = 8,
      sliderInput('CO2.coef','Greenhouse Power',min=10,max =100,value = 50),
      sliderInput('CH4.coef','Greenhouse Power',min=10,max =100,value = 50)
    )
  )
  
)



shinyApp(ui = ui, server = server)


