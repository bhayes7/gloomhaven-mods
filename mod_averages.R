library(tidyr)
library(dplyr)
library(magrittr)

# made this in 2020 (?) to estimate expected values w/ a really good
# modifier deck in Jaws of the Lion

# SIMULATION METHODS

# standard modifier deck
standardDeckMods <- c(rep_len(0, 6), rep_len(1, 5), rep_len(-1, 5),
                  2, -2, NA, NA)
standardDeckEffects <- c(rep_len(NA, 18), "2x", "null")
standardDeck <- data.frame(standardDeckMods, standardDeckEffects)

# this is just for testing purposes
cards <- standardDeck

# info for current modifier deck
# cards <- read.csv("mod_cars.csv", na.strings = "none")

numCards <- nrow(cards) # just the num of cards in the deck
drawn <- rep_len(FALSE, numCards) # tracks which cards have been drawn already

# simulates a card draw, resetting the deck if miss or crit is drawn
# returns the index of the drawn card
drawCard <- function(){
  card <- sample(which(!drawn), 1)
  drawn[card] <<- TRUE
  if(is.na(cards[card, 1])){
    drawn <<-rep_len(FALSE, numCards)
  }
  return(card)
}

getNumAttacks <- function(handSize){return(sum(floor((handSize:2)/2)))}

# draws a certain num of cards and returns the results in a DF
simulateGame <- function(numAttacks){
  draws <- replicate(numAttacks, drawCard())
  mods <- sapply(draws, function(x){cards[x,1]})
  effects <- sapply(draws, function(x){cards[x,2]})
  drawnCards <- data.frame(mods, effects)
  drawn <<-rep_len(FALSE, numCards) # reset deck at end of game
  return(drawnCards)
}

modAverages <- function(game){
  modAverage <- mean(game$mods, na.rm = TRUE)
}

# Let's assume I make 1 attack per round, don't use X cards, and use as many of
# my attack cards per round as possible, short resting in between. Then, 
# assuming that the scenario goes until I'm exhausted, I would make:
# Before 1st rest: 5 attacks (10 cards)
# Before 2nd rest: 4 attacks (9 cards)
# Before 3rd rest: 4 attacks (8 cards)
# Before 4th rest: 3 attacks (7 cards)
# Before 5th rest: 3 attacks (6 cards)
# Before 6th rest: 2 attacks (5 cards)
# Before 7th rest: 2 attacks (4 cards)
# Before 8th rest: 1 attack (3 cards)
# Before 9th rest: 1 attacks (2 cards)
# 5 + 4 + 4 + 3 + 3 + 2 + 2 + 1 + 1 = 25 attacks in an "average" game
#
# We can make a general formula for this:
# Number of attacks  = sum(floor((handSize:2)/2))
