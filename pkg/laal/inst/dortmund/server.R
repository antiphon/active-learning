
library("shiny")


POOL <- "~/Workspace/active-learning/pkg/laal/inst/dortmund/www"

sound_pool <- readRDS(sprintf("%s/dortmund-features.Rds", POOL))
sound_pool <- subset(sound_pool, genre != "jazz")
genres <- levels(sound_pool$genre)
#w <- lapply(sound_pool$id, agrep, list.files(POOL, pattern = ".mp3"), value = TRUE)

classifier <- function(user_id, question, answer) {
  w <- sample(1:nrow(sound_pool), 1)

  r1 <- as.character(sound_pool$genre[w])
  r4 <- sample(setdiff(genres, r1), 4)
  
  s <- sound_pool$id[w]
  s <- sprintf("%s.mp3", substr(s, 1, nchar(s) - 4))
  
  list(song = s, genres = sample(c(r1, r4)))
}



### Shiny audio source implementation: ###############################

renderAudioSource <- function(expr, env = parent.frame(), quoted = FALSE) {
  installExprFunction(expr, "func", env, quoted)
  return(function(shinysession, name, ...){
    audioinfo <- func()
    contentType <- shiny:::getContentType(sub("^.*\\.", "", basename(audioinfo$src)))
    c(src = "1nothing-walk_away.mp3",
      #src = shinysession$fileUrl(name, file = "1nothing-walk_away.mp3", contentType = contentType), 
      type = audioinfo$type)
  })
}



### Shiny server logic: ##############################################

shinyServer(function(input, output, session) {
  
  user_id <- sprintf("%s%s", 
                     paste(sample(letters, 3), collapse = ""), 
                     as.integer(Sys.time()))

  question <- NULL
  num_questions <- 0
  next_song <- ""
  
  values <- reactiveValues(iters = num_questions,
                           song = next_song)
  
  
  output$userid <- renderText({
    ## Get user judgments:
    answer <- data.frame(g1 = input$g1value, 
                         g2 = input$g2value,
                         g3 = input$g3value,
                         g4 = input$g4value,
                         g5 = input$g5value)
    
    
    ## Call classifier and wait for new music to label:
    question <<- classifier(user_id, question, answer)
    
    
    ## Update website, values, etc:
    updateSliderInput(session, "g1value", value = 0, label = question$genres[1])
    updateSliderInput(session, "g2value", value = 0, label = question$genres[2])
    updateSliderInput(session, "g3value", value = 0, label = question$genres[3])
    updateSliderInput(session, "g4value", value = 0, label = question$genres[4])
    updateSliderInput(session, "g5value", value = 0, label = question$genres[5])
    
    num_questions <<- num_questions + 1

    values$iters <- num_questions
    values$song <- question$song
    
    user_id
  })
  
  
  output$iters <- renderText({
    values$iters
  })
  
  
  output$song <- renderText({
    values$song
  })
  
  
  output$audiosrc <- renderAudioSource({
    list(src = values$song, type = "audio/mpeg")
  })
})

