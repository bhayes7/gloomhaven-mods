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

# what if the deck has entries for all 10 blessings and cursings and 
# a "contains" variable? that would work but it sounds dumb

# STANDARD MODIFIER DECK
standardDeckMods <- c(rep_len(0, 6), rep_len(1, 5), rep_len(-1, 5),
                      2, -2, NA, NA)
standardDeckEffects <- c(rep_len(NA, 18), "2x", "null")
standardDeck <- data.frame(standardDeckMods, standardDeckEffects)

# MODIFIER DECK EXTENDED W/ CURSES AND BLESSINGS
cnbMods <- c(rep_len(NA, 20))
cnbEffects <- c(rep_len("bless", 10), rep_len("curse", 10))

extendedDeck <- data.frame(c(standardDeckMods, cnbMods), 
                           c(standardDeckEffects, cnbEffects)) %>%
  cbind(rep_len(TRUE, 40))

colnames(extendedDeck) <- c("mod", "effect", "in_deck") 

# SIMULATING CARD DRAWS
cards <- extendedDeck

numCards <- nrow(cards) # just the num of cards in the deck
drawn <- rep_len(FALSE, numCards) # tracks which cards have been drawn already

# simulates a card draw, resetting the deck if miss or crit is drawn
# and discarding blessings and curses
# returns the index of the drawn card
drawCard <- function(){
  card <- sample(which(!drawn), 1)
  drawn[card] <<- TRUE
  # TODO check if drawn card was a blessing or curse
  
  # check if the deck needs to be shuffled (2x or null is drawn)
  if(cards[card, 2] %in% c("2x", "null")){
    drawn <<-rep_len(FALSE, numCards)
  }
  return(card)
}