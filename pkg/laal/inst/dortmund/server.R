  
library("shiny")


### Experiment definition, etc.: #####################################

ROOT_DIR <- getOption("DORTMUND_EXPERIMENT_DIR")

SOUND_POOL_FILE <- file.path(ROOT_DIR, "musicpool", "description.Rds")
SHINY_MUSIC_DIR <- file.path(ROOT_DIR, "musicpool", "mp3s")
EXPERIMENT_DIR <- file.path(ROOT_DIR, "sessions")

sound_pool <- readRDS(SOUND_POOL_FILE)
genres <- levels(sound_pool$genre)

K <- length(genres)

addResourcePath("music", SHINY_MUSIC_DIR)


init_experiment <- function(id) {
  d <- sprintf("%s/%s", EXPERIMENT_DIR, id)
  dir.create(d)
  d
}

setup_experiment <- function(id) {
  classifier <- classifier_initial(sound_pool)
  # load user here if exists
  labeller <- labeller_initial(parameters_default())
  print(labeller$parameters$minimum_data)
  list(models=list(classifier=classifier, labeller=labeller))
}

### Classifier and user implementation: #######################################

update_models <- function(user_id, user_dir, num_question, question, answer, models) {  
  #w <- sample(1:nrow(sound_pool), 1)
  
  update <- active_learning_step(num_question, question, answer, models, sound_pool)
  w <- update$new_question_idx
  models <- update$models
  
  #r1 <- as.character(sound_pool$genre[w])
  #r4 <- sample(setdiff(genres, r1), 4)
  
  s <- sound_pool$id[w]
  f <- sound_pool$file[w]
  
  saveRDS(list(user_id = user_id, num_question = num_question, 
               question = question, answer = answer, models=models), 
          file = sprintf("%s/%s.Rds", user_dir, num_question))
  
  
  list(question=list(song = s, idx=w, file = f, genres = sample(genres)),#sample(c(r1, r4))), 
       models=models)
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
  initials <- setup_experiment(user_id)
  
  models <- initials$models
  
  question <- NULL
  num_questions <- 0
  
  values <- reactiveValues(iters = num_questions, song = "", file = "")
  
  
  output$userid <- renderText({
    ## Get user judgments:
    answer <- data.frame(g1 = input$g1value, 
                         g2 = input$g2value,
                         g3 = input$g3value,
                         g4 = input$g4value,
                         g5 = input$g5value,
                         g6 = input$g6value,
                         g7 = input$g7value,
                         g8 = input$g8value,
                         g9 = input$g9value)
    
    
    ## Call classifier and wait for new music to label:
    update <- update_models(user_id, user_dir, num_questions, question, answer, models)
    question <<- update$question
    models <<- update$models
    
    ## Update website, values, etc:
    for(i in 1:K){
      updateSliderInput(session, paste0("g",i,"value"), value = 100/K, label = question$genres[i])
    }

    num_questions <<- num_questions + 1

    values$iters <- num_questions
    values$song <- question$song
    values$file <- question$file
    
    user_id
  })
  
  
  output$iters <- renderText({
    values$iters
  })
  
  
  output$song <- renderText({
    values$song
  })
  
  
  output$audiosrc <- renderAudioSource({
    list(src = sprintf("/music/%s", values$file), type = "audio/mpeg")
  })
})

