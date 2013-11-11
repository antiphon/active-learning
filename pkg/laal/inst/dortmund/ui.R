
library("shiny")



### Shiny client implementation: #####################################

shinyUI(pageWithSidebar(
  headerPanel(
    singleton(tags$head(includeScript("audiosrc.js")))
  ),
  
  sidebarPanel(
    p("Hello ", span(id = "userid", class = "shiny-text-output")),
    
    p("This is question",
      span(id = "iters", class = "shiny-text-output"), "for you:"),
  
   # p("What is the genre of", strong(span(id = "song", class = "shiny-text-output")), "?"),
  
    tags$audio(id = "audiosrc", controls = "controls", autoplay = "autoplay", class = "shiny-audio-output")
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


