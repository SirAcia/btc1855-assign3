# BTC1855 CODING IN R 
# ASSIGNMENT THREE 
# CRYSTAL LEE
# Version 2024.04.2+764 

# Make text file with words - 
# Choose a random word from the list - 
# Inform user how many letters are in the word
# Decide how many tries the person can have
# Ask user for input
# Make sure the input is one character
# Make sure both capital and lowercase are treated equally
# Check if the letter is included
# If yes, let user know and how many tries they have left
# If not, let user know and how many tries they have left
# If they ran out of tries, exit program
library(ggplot2)

# Welcome and instructions
while (TRUE) {
cont <- readline(prompt = "Welcome to hangman! Click R to continue: ")
if (cont == "R" | cont == "r") {
    cat("The game of hangman is very simple...\n",
        "We will choose a word for you.\n",
        "Guess one letter at a time.\n",
       "You have three lives before the hangman gets 'hanged'!\n")
    break
  } else {
    print("Come on, don't leave me hanging! Press 'R' to get started.") 
  }
} 
        
# Load word list and select a word
while (TRUE) {
  cont2 <- readline(prompt = "Click R to continue: ")
  if (cont2 == "R" | cont2 == "r") {
    wordlist <- read.delim("assign3-wordlist.txt", header = F)
    word <- sample(wordlist$V1, 1)
    masked_word <- rep("_", nchar(word))
    lives <- 3
    guessed_letters <- character()
    print(paste("Your word has", nchar(word), "characters!"))
    break
  } else {
    print("Oops! Are you sure you pressed the right button?") 
  }
} 

update_masked_word <- function(word, masked_word, guess) {
  for (i in seq_along(word)) {
    if (substr(word, i, i) == guess) {
      masked_word[i] <- guess
    }
  }
  return(masked_word)
}

# Game loop
while (TRUE) {
  print(paste(masked_word, collapse = " "))
  cont3 <- readline(prompt = "Guess a letter: ")
  cont3 <- to lower(cont3) # Change all letters to lower case 
 
  if (cont3 %in% guessed_letters) {
    print("You already guessed that! Try again")
    print(paste(masked_word, collapse = " "))
    next
  } else {
    print("Not quite! You have 2 tries left.") 
  }
} 



# DIFFICLUTY LEVELS
# ACTUAL HANGMAN 


