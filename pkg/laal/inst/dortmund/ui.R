
library("shiny")



### Shiny client implementation: #####################################

shinyUI(pageWithSidebar(
  headerPanel(
    singleton(tags$head(includeScript("audiosrc.js")))
  ),
  
  sidebarPanel(
    p("Hello ", span(id = "userid", class = "shiny-text-output"), 
      " (", span(id = "iters", class = "shiny-text-output"), ")"),
  
    p("please classify", strong(span(id = "song", class = "shiny-text-output")), ":"),
  
    tags$audio(controls = "controls", tags$source(id = "audiosrc", class = "shiny-audiosrc-output"))
  ),
  
  mainPanel(
    div(style="width: 400px;",
      p(
        sliderInput("g1value", "Genre1", min = 0, max = 100, value = 0, step = 10, ticks = TRUE),
        sliderInput("g2value", "Genre2", min = 0, max = 100, value = 0, step = 10, ticks = TRUE),
        sliderInput("g3value", "Genre3", min = 0, max = 100, value = 0, step = 10, ticks = TRUE),
        sliderInput("g4value", "Genre4", min = 0, max = 100, value = 0, step = 10, ticks = TRUE),
        sliderInput("g5value", "Genre5", min = 0, max = 100, value = 0, step = 10, ticks = TRUE),
      
        submitButton("Submit")
      )
    )
  )
))



# shinyUI(pageWithSidebar(
#     
#   # Application title
#   headerPanel("Miles Per Gallon"),
#   
#   # Sidebar with controls to select the variable to plot against mpg
#   # and to specify whether outliers should be included
#   sidebarPanel(
#     selectInput("variable", "Variable:",
#                 list("Cylinders" = "cyl", 
#                      "Transmission" = "am", 
#                      "Gears" = "gear")),
#     
#     checkboxInput("outliers", "Show outliers", FALSE)
#   ),
#   
#   # Show the caption and plot of the requested variable against mpg
#   mainPanel(
# #     includeScript("js/play-sound.js"),
# 
# #     tags$audio(tags$source(src="___In_the_Dirty_City-Longtooth.mp3", type = "audio/mpeg"), 
# #                controls = "true"),
# #     
# #     h3(getwd()),
# #     h3(list.files()),
#     
#     h3(textOutput("caption")),
# #     div("Countdown", 
# #         id="example3div",
# #         style="border-style:dotted; padding:10px; font-size:24px; width:200px; text-align:center;"),
#     plotOutput("mpgPlot")
#   )
# ))

