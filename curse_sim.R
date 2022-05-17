library(tidyr)
library(dplyr)
library(magrittr)

# simulating expected modifier values when the monster modifier deck
# is full of curses + all monster attacks have disasdvantage b/c
# Music Note is just broken

# HMMMM a deck of cards is a stack? Which R isn't optimal for BUT
# I feel like this is a relatively simple statistical operation
# that should be implementable without much hassle...gonna have to 
# think about it

# standard modifier deck
standardDeckMods <- c(rep_len(0, 6), rep_len(1, 5), rep_len(-1, 5),
                      2, -2, NA, NA)
standardDeckEffects <- c(rep_len(NA, 18), "2x", "null")
standardDeck <- data.frame(standardDeckMods, standardDeckEffects)

cards <- standardDeck

numCards <- nrow(cards) # just the num of cards in the deck
drawn <- rep_len(FALSE, numCards) # tracks which cards have been drawn already

# simulates a card draw, resetting the deck if miss or crit is drawn
# returns the index of the drawn card
drawCard <- function(){
  card <- sample(which(!drawn), 1)
  drawn[card] <<- TRUE
  # check if the deck needs to be shuffled (2x or null is drawn)
  if(cards[card, 2] %in% c("2x", "null")){
    drawn <<-rep_len(FALSE, numCards)
  }
  return(card)
}