
library("shiny")

####
ownSlider <- function(inputId, label, value = 0) {
  tags$div( tagList(
    tags$label(label, "for"=inputId, class="input_label"),
    tags$input(id = inputId,
               class = "slider2",
               type = "range",
               min=0,
               max=100,
               value=value)
  ),
            class="slider-row"
  )
}

K <- 9

### Shiny client implementation: #####################################

shinyUI(
  pageWithSidebar(
  headerPanel(
    singleton(tags$head(includeScript("audiosrc.js"))),
    singleton(tags$head(tags$script(src = "slider.js")))
  ),
  
  sidebarPanel(
    p("Hello ", span(id = "userid", class = "shiny-text-output")),
    
    p("This is question",
      span(id = "iters", class = "shiny-text-output"), "for you:"),
  
    p("What is the genre of", em(span(id = "song", class = "shiny-text-output")), "?"),

    tags$audio(id = "audiosrc", controls = "controls", autoplay = "autoplay", 
               class = "shiny-audio-output")
  ),
  mainPanel(
    div(style="width: 400px;",
      p(
        div(
        ownSlider("g1value", "Alternative", 100/K),
        ownSlider("g2value", "Blues", 100/K),
        ownSlider("g3value", "Electronic", 100/K),
        ownSlider("g4value", "Folk & Country", 100/K),
        ownSlider("g5value", "Funk, soul, R'n'B", 100/K),
        ownSlider("g6value", "Jazz", 100/K),
        ownSlider("g7value", "Pop", 100/K),
        ownSlider("g8value", "Rap, hip-hop", 100/K),
        ownSlider("g9value", "Rock", 100/K), id="div-sliders"),
        submitButton("Submit")
      ),
        tags$script(src="connect_sliders.js")
    )
  )
))


