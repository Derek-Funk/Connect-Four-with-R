#library to deploy app: rsconnect
#command to deploy app: deployApp(appDir = "C:\\Users\\derek.funk\\Documents\\MSDS\\Shiny\\connectfour", account = "derek-funk")

source("global.R")

ui <- fluidPage(
  title = "Connect Four",
  useShinyjs(),
  fluidPage(
    fluidRow(
      h1("Connect Four")
    ),
    fluidRow(
      column(
        width = 3,
        wellPanel(
          h3("Game Settings"),
          textInput(inputId = "player1Name", label = "Player 1 Name (black)"),
          textInput(inputId = "player2Name", label = "Player 2 Name (red)"),
          radioButtons(inputId = "startingPlayer", label = "Player going 1st", choices = c("Black"="black", "Red"="red")),
          actionButton(inputId = "newGame", label = "New Game")
        )
      ),
      column(
        width = 7,
        lapply(1:6, function(i) {
          fluidRow(
            lapply(1:7, function(j) {
              column(
                width = 1,
                disabled(actionButton(
                  inputId = paste0("white", i, j),
                  label = "", #paste0(i, j),
                  style = "background-color: #FFF; border-color: #000; border-radius: 50%; height: 60px; width: 60px;"
                )),
                hidden(actionButton(
                  inputId = paste0("black", i, j),
                  label = "", #paste0(i, j),
                  style = "background-color: #000; border-color: #000; border-radius: 50%; height: 60px; width: 60px;"
                )),
                hidden(actionButton(
                  inputId = paste0("red", i, j),
                  label = "", #paste0(i, j),
                  style = "background-color: #FF0000; border-color: #000; border-radius: 50%; height: 60px; width: 60px;"
                ))
              )
            })
          )
        })
      )
    ),
    fluidRow(
      column(
        width = 3,
        br(),
        wellPanel(
          h3("Game Statistics"),
          uiOutput(outputId = "moveCounts")
        )
      ),
      column(
        width = 4,
        br(),
        wellPanel(
          h3("Music Player"),
          includeHTML("www/music.html")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    turn = NULL,
    gameNumber = 0,
    blackMoves = 0,
    redMoves = 0,
    totalMoves = 0,
    board = matrix(rep(0,6*7), nrow = 6)
  )
  
  resetGame <- function() {
    removeNotification(
      id = "redTurnNotification"
    )
    removeNotification(
      id = "blackTurnNotification"
    )
    rv$blackMoves <- 0
    rv$redMoves <- 0
    rv$totalMoves <- 0
    rv$board <- matrix(rep(0, 6*7), nrow = 6)
    lapply(1:6, function(i) {
      lapply(1:7, function(j) {
        hide(id = paste0("black", i, j))
        hide(id = paste0("red", i, j))
        show(id = paste0("white", i, j))
        disable(id = paste0("white", i, j))  
      })
    })
    toggleState(id = "player1Name")
    toggleState(id = "player2Name")
    toggleState(id = "startingPlayer")
  }
  
  observeEvent(
    eventExpr = input$newGame,
    handlerExpr = {
      if(input$player1Name!="" & input$player2Name!="") {
        toggleState(id = "player1Name")
        toggleState(id = "player2Name")
        toggleState(id = "startingPlayer")
        lapply(1:7, function(j) {
          enable(id = paste0("white", 6, j))
        })
        rv$turn <- input$startingPlayer
        rv$gameNumber <- rv$gameNumber + 1
        showModal(
          modalDialog(
            title = paste0("Game #", rv$gameNumber),
            if(rv$turn=="black") {
              paste0(input$player1Name, " (black) goes first!")
            } else {
              paste0(input$player2Name, " (red) goes first!")
            }
          )
        )
        if(rv$turn=="black") {
          showNotification(
            id = "blackTurnNotification",
            ui = "Black's turn!",
            duration = NULL,
            closeButton = FALSE
          )
        } else {
          showNotification(
            id = "redTurnNotification",
            ui = "Red's turn!",
            duration = NULL,
            closeButton = FALSE
          )  
        }
      } else {
        showNotification(ui = "Make sure both names are filled out!", duration = 3)
      }
    }
  )
  
  #main game play
  lapply(1:6, function(i) {
    lapply(1:7, function(j) {
      observeEvent(
        eventExpr = input[[paste0("white", i, j)]],
        handlerExpr = {
          hide(id = paste0("white", i, j))
          if(rv$turn == "black") {
            rv$board[i,j] <- 1
            show(id = paste0("black", i, j))
            rv$blackMoves <- rv$blackMoves + 1
            rv$totalMoves <- rv$totalMoves + 1
            if(checkWin(board = rv$board, player = 1)) {
              showModal(
                modalDialog(
                  title = paste0("Game #", rv$gameNumber, ": ", input$player1Name, " (black) wins!"),
                  HTML(
                    input$player1Name, " (black) moves: ", rv$blackMoves, "<br>",
                    input$player2Name, " (red) moves: ", rv$redMoves, "<br>",
                    "Total moves: ", rv$totalMoves, "<br>"
                  )
                )
              )
              resetGame()
            } else {
              if(i>=2) {
                enable(id = paste0("white", i-1, j))
              }
              rv$turn <- "red"
              removeNotification(
                id = "blackTurnNotification"
              )
              showNotification(
                id = "redTurnNotification",
                ui = "Red's turn!",
                duration = NULL,
                closeButton = FALSE
              )
            }
          } else {
            rv$board[i,j] <- 2
            show(id = paste0("red", i, j))
            rv$redMoves <- rv$redMoves + 1
            rv$totalMoves <- rv$totalMoves + 1
            if(checkWin(board = rv$board, player = 2)) {
              showModal(
                modalDialog(
                  title = paste0("Game #", rv$gameNumber, ": ", input$player2Name, " (red) wins!"),
                  HTML(
                    input$player2Name, " (red) moves: ", rv$redMoves, "<br>",
                    input$player1Name, " (black) moves: ", rv$blackMoves, "<br>",
                    "Total moves: ", rv$totalMoves, "<br>"
                  )
                )
              )
              resetGame()
            } else {
              if(i>=2) {
                enable(id = paste0("white", i-1, j))
              }
              rv$turn <- "black"
              removeNotification(
                id = "redTurnNotification"
              )
              showNotification(
                id = "blackTurnNotification",
                ui = "Black's turn!",
                duration = NULL,
                closeButton = FALSE
              )
            }
          }
          #debug
          print(rv$board)
          print(rv$turn)
        }
      )
    })
  })
  
  output$moveCounts <- renderUI(
    expr = {
      HTML(
        input$player1Name, " (black) moves: ", rv$blackMoves, "<br>",
        input$player2Name, " (red) moves: ", rv$redMoves, "<br>",
        "Total moves: ", rv$totalMoves, "<br>",
        "Game #", rv$gameNumber
      )
    }
  )
}

shinyApp(ui = ui, server = server)