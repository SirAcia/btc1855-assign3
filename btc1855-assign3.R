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
intro <- function() {
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
}
        
game <- function() {
    update_masked_word <- function(word, masked_word, guess) {
    word <- unlist(strsplit(tolower(word), ""))  # Ensure word is split into characters
    for (i in seq_along(word)) {
      if (word[i] == guess) {
        masked_word[i] <- word[i]
      }
    }
    return(masked_word)
  }

  # Load word list and select a word
  while (TRUE) {
    cont2 <- readline(prompt = "Click R to continue: ")
    if (cont2 == "R" | cont2 == "r") {
      wordlist <- read.delim("assign3-wordlist.txt", header = FALSE)
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
  
# Game loop
  while (TRUE) {
    print(paste(masked_word, collapse = " "))
    cont3 <- readline(prompt = "Guess a letter or a word: ")
    cont3 <- tolower(cont3)  # Change all letters to lower case 
    
    if (nchar(cont3) != 1 && nchar(cont3) != nchar(word)) {
      print("Please enter a single letter or guess a word.")
      next
    }
    
    if (cont3 %in% guessed_letters) {
      print("You already guessed that! Try again.")
      next
    } 
    
    guessed_letters <- c(guessed_letters, cont3)
    
    if (nchar(cont3) == nchar(word)) { #??
      if (cont3 == word) {
        masked_word <- strsplit(word, "")[[1]]
        print("Congrats! You guessed the word!")
        break
      } else {
        lives <- lives - 1
        print(paste("Not quite! You have", lives, "tries left. Try again."))
      }
    } else {
      match <- grepl(cont3, word, ignore.case = TRUE)  # Check if the guessed letter is in the word
      
      if (any(match)) {
        masked_word <- update_masked_word(word, masked_word, cont3)
        print("You got it!")
      } else {
        lives <- lives - 1
        print(paste("Not quite! You have", lives, "tries left. Try again."))
      }
    }
    
    if (!("_" %in% masked_word)) {
      print("Congrats! You guessed the word!")
      break
    } else if (lives == 0) {
      print(paste("You ran out of tries! The word was:", word))
      break
    }
  }
}

repeat {
  intro()
  game()
  restart <- readline(prompt = "Do you want to play again? (Y/N): ")
  if (tolower(restart) != "y") {
    print("Thank you for playing! Goodbye!")
    break
  }
}

# DIFFICLUTY LEVELS
# ACTUAL HANGMAN 
# print plot of a cat as celebration
# add a prompt to restart the game 

