library(shiny)
library(shinyjs)
library(dplyr)

MIN_HAND <- 6

vars <- reactiveValues(players = list(),
                       deck = NULL,
                       field = NULL,
                       durak = NULL,
                       started = F)

durak <- function() {
  if(is.null(vars$durak)) {
    if(!is.null(vars$deck))
      vars$durak <- unlist(tail(vars$deck, 1), recursive = F)$suit
    else return(NULL)
  }
  
  return(vars$durak)
}

popDeck <- function() {
  if(is.null(vars$deck)) return(NULL)
  
  card <- head(vars$deck, 1) %>% unlist(recursive = F)
  
  vars$deck <- tail(vars$deck, -1)
  if(length(vars$deck) == 0) vars$deck <- NULL
  
  card
}

startGame <- function() {
  if(vars$started) return()
  
  print('Game starting...')

  vars$deck <- lapply(6:14, function(n) {
    lapply(1:4, function(suit) {
      list(n = n, suit = suit)
    })
  }) %>% unlist(recursive = F) %>% sample
  
  minCard <- 15
  minPlayer <- 1
  
  vars$players <- lapply(1:2, function(pIndx) {
    player <- vars$players[[pIndx]]
    
    player$cards <- lapply(1:MIN_HAND, function(indx) {
      card <- popDeck()
      
      if(card$suit == durak() && card$n < minCard) {
        minCard <<- card$n
        minPlayer <<- pIndx
      }
      
      card
    })
    
    player
  })
  
  vars$players <- lapply(vars$players, function(player) {
    player$cards <- player$cards[order(sapply(player$cards, '[[', 2), sapply(player$cards, '[[', 1))]
    player
  })
  
  if(minPlayer == 2)
    vars$players <- rev(vars$players)
  
  vars$started <- T
}

endGame <- function() {
  print('Game ending...')
  
  vars$deck <- NULL
  vars$field <- NULL
  vars$durak <- NULL
  vars$started <- F
}

server <- function(input, output, session) {
  sessionVars <- reactiveValues(id = runif(1, 1, 1e5))
  
  # Player joins
  isolate({
    if(length(vars$players) < 2)
      vars$players[[length(vars$players) + 1]] <- list(id = sessionVars$id, name = input$name, image = NULL, cards = NULL)
    
    if(length(vars$players) == 2)
      startGame()
  })
  
  # Remove players on session end
  session$onSessionEnded(function() {
    isolate({
      vars$players <- Filter(function(x) x$id != sessionVars$id, vars$players)
      endGame()
    })
  })
  
  # Listen for name changes
  observe({
    input$name
    
    isolate({
      vars$players <- lapply(vars$players, function(player) {
        if(player$id == sessionVars$id)
          player$name <- input$name
        player
      })
    })
  })
  
  # Update the sidebar whenever the players list changes
  output$online <- renderUI({
    lapply(1:2, function(indx) {
      fluidRow(class = 'name',
               column(12, h2(ifelse(indx > length(vars$players), 'Waiting...', vars$players[[indx]]$name))))
    })
  })
  
  # Toggle the turns when started or players changes
  observe({
    runjs(paste0('$("#pass").prop("disabled", ',
                 as.character(!vars$started || length(vars$players) < 2 || vars$players[[1]]$id != sessionVars$id) %>%
                   tolower,
                 ')'))
  })
  
  # Set the turn to the other player and draw cards if needed
  endTurn <- function(draw = F) {
    if(draw) {
      if(!is.null(vars$field)) {
        taking <- Map(function(stack) length(stack) == 1, vars$field) %>% unlist %>% any
        
        if(taking)
          vars$players[[1]]$cards <- append(vars$players[[1]]$cards, unlist(vars$field, recursive = F))
      }
      
      vars$players <- lapply(vars$players, function(player) {
        if(length(player$cards) < MIN_HAND && !is.null(vars$deck)) {
          toDraw <- MIN_HAND - length(player$cards)
          for(i in 1:toDraw)
            player$cards[[length(player$cards) + 1]] <- popDeck()
          
          rm(i)
        }
        
        player
      })
      
      if(any(unlist(Map(function(player) length(player$cards), vars$players)) == 0)) {
        # TODO Game over!
      }
      
      vars$players <- lapply(vars$players, function(player) {
        player$cards <- player$cards[order(sapply(player$cards, '[[', 2), sapply(player$cards, '[[', 1))]
        player
      })
      
      vars$field <- NULL
    }
    
    vars$players <- rev(vars$players)
  }
  
  # Listen to the end turn button
  observe({
    if(input$pass > 0)
      isolate({
        if(vars$started && length(vars$players) >= 2 && vars$players[[1]]$id == sessionVars$id)
          endTurn(T)
      })
  })
  
  # Listen for attempts to play cards
  observe({
    attempt <- input$played
    
    isolate({
      indx <- attempt %>% as.integer
      
      if(length(indx) == 1 && vars$started && length(vars$players) >= 2 &&
         vars$players[[1]]$id == sessionVars$id &&
         !is.na(indx) && indx > 0 && indx <= length(vars$players[[1]]$cards)) {
        card <- vars$players[[1]]$cards[[indx]]
        
        if(is.null(vars$field)) {
          vars$players[[1]]$cards[[indx]] <- NULL
          vars$field <- list(list(card))
          endTurn()
        } else if(any(unlist(Map(length, vars$field)) == 1)) { # Something to beat...
          if(all(unlist(Map(length, vars$field)) == 1) &&
             vars$field[[1]][[1]]$n == card$n &&
             length(vars$players[[2]]$cards) > sum(unlist(Map(length, vars$field)) == 1)) { # Matching it/them......
            vars$players[[1]]$cards[[indx]] <- NULL
            vars$field[[length(vars$field) + 1]] <- list(card)
            endTurn()
          } else {
            lapply(1:length(vars$field), function(index) { # Beating it/them...
              if(!is.null(card)) {
                stack <- vars$field[[index]]
                print(durak())
                print(stack)
                print(card)
                if(length(stack) == 1 && (stack[[1]]$suit == card$suit && stack[[1]]$n < card$n) ||
                   (stack[[1]]$suit != durak() && card$suit == durak())) {
                  vars$players[[1]]$cards[[indx]] <- NULL
                  vars$field[[index]][[2]] <- card
                  card <<- NULL
                }
              }
            })
            
            if(all(unlist(Map(length, vars$field)) == 2))
              endTurn()
          }
        } else if(card$n %in% unlist(Map(function(card) card$n, unlist(vars$field, recursive = F)))) { # More play
          vars$players[[1]]$cards[[indx]] <- NULL
          vars$field[[length(vars$field) + 1]] <- list(card)
          endTurn()
        }
      }
    })
  })
  
  # Show the game field
  output$game <- renderUI({
    if(vars$started) {
      fluidRow(
        column(2, # The deck
               playingCard(switch(is.null(vars$deck) + 1, unlist(tail(vars$deck, 1), recursive = F), NULL), durak = 'durak'),
               deckStack(length(vars$deck))
               ),
        column(10, # The cards in play
               playField(vars$field))
      )
    }
  })
  
  # Draw the opponent's hand
  output$opponent <- renderUI({
    if(vars$started) {
      opponent <- Filter(function(x) x$id != sessionVars$id, vars$players) %>% unlist(recursive = F)
      
      fluidRow(lapply(1:length(opponent$cards), function(indx) playingCardBack()))
    }
  })
  
  # Draw the player's hand
  output$player <- renderUI({
    if(vars$started) {
      me <- Filter(function(x) x$id == sessionVars$id, vars$players) %>% unlist(recursive = F)
      
      if(!is.null(me$cards))
        fluidRow(lapply(1:length(me$cards), function(indx) playingCard(me$cards[[indx]], indx)))
    }
  })
}