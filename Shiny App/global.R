library(shiny)
library(ggplot2)
library(shinyWidgets)
library(shinyjs)

checkHorizontal <- function(board, player=1) {
  #board is the game matrix
  #player = 1 for black, 2 for red
  horizontalFour <- FALSE
  i <- 1
  while(horizontalFour==FALSE & i<=6) {
    j <- 1
    while(horizontalFour==FALSE & j<=4) {
      if(all(board[i, j:(j+3)]==player)) {
        horizontalFour <- TRUE
      } else {
        j <- j + 1
      }
    }
    i <- i + 1
  }
  return(horizontalFour)
}

checkVertical <- function(board, player=1) {
  #board is the game matrix
  #player = 1 for black, 2 for red
  verticalFour <- FALSE
  j <- 1
  while(verticalFour==FALSE & j<=7) {
    i <- 1
    while(verticalFour==FALSE & i<=3) {
      if(all(board[i:(i+3), j]==player)) {
        verticalFour <- TRUE
      } else {
        i <- i + 1
      }
    }
    j <- j + 1
  }
  return(verticalFour)
}

checkBackslash <- function(board, player=1) {
  #board is the game matrix
  #player = 1 for black, 2 for red
  backslashFour <- FALSE
  i <- 1
  while(backslashFour==FALSE & i<=3) {
    j <- 1
    while(backslashFour==FALSE & j<=4) {
      if(all(board[i,j]==player, board[i+1,j+1]==player, board[i+2,j+2]==player, board[i+3,j+3]==player)) {
        backslashFour <- TRUE
      } else {
        j <- j + 1
      }
    }
    i <- i + 1
  }
  return(backslashFour)
}

checkForwardslash <- function(board, player=1) {
  #board is the game matrix
  #player = 1 for black, 2 for red
  forwardslashFour <- FALSE
  j <- 4
  while(forwardslashFour==FALSE & j<=7) {
    i <- 4
    while(forwardslashFour==FALSE & i<=6) {
      if(all(board[i,j-3]==player, board[i-1,j-2]==player, board[i-2,j-1]==player, board[i-3,j]==player)) {
        forwardslashFour <- TRUE
      } else {
        i <- i + 1
      }
    }
    j <- j + 1
  }
  return(forwardslashFour)
}

checkWin <- function(board, player=1) {
  fourMade <- FALSE
  if(any(
    checkHorizontal(board, player),
    checkVertical(board, player),
    checkBackslash(board, player),
    checkForwardslash(board, player)
  )) {
    fourMade <- TRUE
  }
  return(fourMade)
}