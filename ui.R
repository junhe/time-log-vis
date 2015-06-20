# ui.R

print('hello')
print(textOutput('text1'))


shinyUI(fluidPage(
  titlePanel("censusVis"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with 
        information from the 2010 US Census."),
    
      #sliderInput("dayrange", 
        #label = "Day Range:",
        #min = 0, max = 366, value = c(0, 366)),
      uiOutput('day.sel'),

      uiOutput('act.sel')
    ),
    mainPanel(plotOutput('myplot'))
  )
))
