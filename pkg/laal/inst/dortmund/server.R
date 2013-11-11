
library("shiny")



### Experiment implementation: #######################################

ROOT <- "~/Workspace/active-learning/experiment"

sound_pool <- readRDS("pool.Rds")
sound_pool <- subset(sound_pool, genre != "jazz")
genres <- levels(sound_pool$genre)


init_experiment <- function(id) {
  d <- sprintf("%s/%s", ROOT, id)
  dir.create(d)
  d
}



### Classifier implementation: #######################################

call_classifier <- function(user_id, user_dir, num_question, question, answer) {
  saveRDS(list(user_id = user_id, num_question = num_question, 
               question = question, answer = answer), 
          file = sprintf("%s/%s.Rds", user_dir, num_question))
  
  
  w <- sample(1:nrow(sound_pool), 1)

  r1 <- as.character(sound_pool$genre[w])
  r4 <- sample(setdiff(genres, r1), 4)
  
  s <- sound_pool$id[w]
  f <- sound_pool$file[w]
  
  list(song = s, file = f, genres = sample(c(r1, r4)))
}



### Shiny audio source implementation: ###############################

renderAudioSource <- function(expr, env = parent.frame(), quoted = FALSE) {
  installExprFunction(expr, "func", env, quoted)
  return(function(shinysession, name, ...){
    audioinfo <- func()
    c(src = audioinfo$src, type = audioinfo$type)
  })
}



### Shiny server logic: ##############################################

shinyServer(function(input, output, session) {
  
  user_id <- sprintf("%s%s", 
                     paste(sample(letters, 3), collapse = ""), 
                     as.integer(Sys.time()))
  
  user_dir <- init_experiment(user_id)
  
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
    question <<- call_classifier(user_id, user_dir, num_questions, question, answer)
    
    
    ## Update website, values, etc:
    updateSliderInput(session, "g1value", value = 0, label = question$genres[1])
    updateSliderInput(session, "g2value", value = 0, label = question$genres[2])
    updateSliderInput(session, "g3value", value = 0, label = question$genres[3])
    updateSliderInput(session, "g4value", value = 0, label = question$genres[4])
    updateSliderInput(session, "g5value", value = 0, label = question$genres[5])
    
    num_questions <<- num_questions + 1

    values$iters <- num_questions
    values$song <- question$file
    
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

