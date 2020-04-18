library(shiny)
library(shinyjs)

NAMES <- c('Sameeha Stein','Garry Allen','Amiyah Goldsmith','Kayne Maguire','Saniya Vang','Tom Sadler','Bella-Rose Brook','Neave Perry','Lester Hayden','Anais Navarro','Hira Oconnell','Connor Stacey','Aaryan Roach','Yvette Peterson','Grover Barron','Leonidas Lugo','Chantel Huff','Dawood Ellison','Layla-Mae Lindsay','Waseem Woodcock','Tre Millington','Elleanor Oliver','Mirza Barlow','Jocelyn Brennan','Mathew Hook','Dalia Hutton','Idrees Hough','Cole Jacobs','Daryl Mcculloch','Simona Phillips','Freja Povey','Agata Mann','Tabitha Velazquez','Cora Horner','Celeste Joyner','Jeanne Partridge','Ansh Wagstaff','Isobella Lord','Kristy Parkes','Dottie Watts','Edward Patterson','Marnie Davidson','Essa Hobbs','Elouise Barton','Danny Diaz','Johnny Clark','Kristian Winters','Sufyan Hayward','Danielius Keith','Desiree Rogers')

deckStack <- function(n) {
  if(n > 1) {
    shadow <- ceiling(n / 3)
    shadow <- paste0('-moz-box-shadow: 1px ', shadow, 'px 3px #000; -webkit-box-shadow: 1px ', shadow, 'px 3px #000; box-shadow: 1px ', shadow, 'px 3px #000;')
    span(class = 'col-sm-2 card-template card-back deck', style = shadow)
  }
}

playingCard <- function(card, indx = 0, durak = '') {
  if(!is.null(card)) {
    suit <- switch(card$suit, '&hearts;', '&diams;', '&spades;', '&clubs;')
    color <- paste0('"color: ', ifelse(card$suit < 3, 'red', 'black'), '"')
    
    column(width = 2,
           class = paste('card-template card-body', durak),
           style = paste0('visibility: ', ifelse(card$n == 0, 'hidden', 'unset')),
           index = indx,
           onclick = ifelse(indx == 0, '', 'playCard(this);'),
           div(class = 'card-top',
               div(ifelse(card$n > 10, switch(card$n - 10, 'J', 'Q', 'K', 'A'), card$n)),
               HTML(paste0('<div style=', color, '>', suit, '</div>'))
           ),
           HTML(paste0('<h1 style=', color, '>', suit, '</h1>')),
           div(class = 'card-bottom',
               HTML(paste0('<div style=', color, '>', suit, '</div>')),
               div(ifelse(card$n > 10, switch(card$n - 10, 'J', 'Q', 'K', 'A'), card$n))
           )
    )
  }
}

playField <- function(field) {
  if(!is.null(field)) {
    div(class = 'container-fluid', style = 'height: 100%',
        fluidRow(style = 'position: absolute; width: 100%; bottom: -15%;',
          lapply(field, function(stack) {
            playingCard(stack[[1]])
          })
        ),
        fluidRow(style = 'position: absolute; width: 100%;',
                 lapply(field, function(stack) {
                   if(length(stack) == 2)
                     playingCard(stack[[2]])
                   else
                     playingCard(list(n = 0, suit = 1))
                 })
        )
    )
  }
}

playingCardBack <- function() {
  column(width = 2, class = 'card-template card-back')
}

ui <- fluidPage(style = 'height: 100%',
  useShinyjs(),
  withTags(head(
    script(src = 'js/game.js'),
    script(src = 'https://kit.fontawesome.com/33dcd9d8f9.js', crossorigin = 'anonymous'), # TODO Update this for new proj?
    link(rel = 'stylesheet', type = 'text/css', href = 'css/style.css'),
    
    title(options('app.name')),
    meta(name = 'description', content = options('app.description')),
    meta(name = 'keywords', content = options('app.tags')),
    meta(name = 'author', content = options('app.author')),
    
    link(rel = 'icon', href = 'https://gemma.msl.ubc.ca/images/favicon.ico') # TODO Image
  )),
  
  sidebarLayout(
    sidebarPanel(width = 3, class = 'sidebar',
      textInput('name', 'Username', sample(NAMES, 1), placeholder = 'John Doe'),
      uiOutput('online', style = 'flex: 1'),
      actionButton('pass', 'End Turn', width = '100%', disabled = NA)
    ),
    
    mainPanel(width = 9,
              div(class = 'container-fluid game',
                  fluidRow(class = 'cards', uiOutput('opponent')),
                  fluidRow(class = 'field', uiOutput('game')),
                  fluidRow(class = 'cards', uiOutput('player'))
                  )
              )
  )
)