# BTC1855 CODING IN R 
# ASSIGNMENT THREE 
# CRYSTAL LEE
# Version 2024.04.2+764 

# Please ensure the assign3-wordlist.txt file is downloaded from my repo :)
# Click source to begin the game/restart after exiting the game

# Welcome and instructions
# Create function to run introductory instructions
intro <- function() {
  # While loop runs the loop infinitely until there is a break
  while (TRUE) {
  cont <- readline(prompt = "Welcome to hangman! Enter R to continue: ")
  # Accept either lower or upper case "R"
  if (cont == "R" | cont == "r") {
      cat("The game of hangman is very simple...\n")
      cat("We will choose a word for you.\n")
      cat("Guess one letter at a time.\n")
      cat("Depending on your difficulty level, you have\n")
      cat("a couple tries before you get hanged!\n")
      break
    } else {
      print("Come on, don't leave me hanging! Enter 'R' to get started.") 
    }
  } 
}

game <- function() {
  # Create function to update masked word every time there is a correct guess
  update_masked_word <- function(word, masked_word, guess) {
    # Ensure word is split into each character and in lower case
    word <- unlist(strsplit(tolower(word), ""))  
    # Iterate along the sequence of the word 
    for (i in seq_along(word)) { 
      # If the guess matches a letter, replace masked letter with the match
      if (word[i] == guess) { 
        masked_word[i] <- word[i]
      }
    }
    # Display the updated masked word with matches
    return(masked_word)
  }
  
  while (TRUE) {
    cont2 <- readline(prompt = "Enter R to continue: ")
    if (cont2 == "R" | cont2 == "r") { 
      cat("Easy: 1\n")
      cat("Medium: 2\n")
      cat("Hard: 3\n")
      diff <- readline(prompt = "Choose a difficulty level: ")    
      # Load the wordlist file and access the column with words
      # We do not have a header row so header = FALSE
      wordlist <- read.delim("assign3-wordlist.txt", header = FALSE)$V1
      # Define length of characters for each difficulty level and tell
      # player how many lives/tries they have per level
      if (diff == "1") {
        easy <- wordlist[nchar(wordlist) >= 2 & nchar(wordlist) <= 5]
        word <- sample(easy, 1)
        cat("You chose easy!\n")
        cat("You have 8 lives.\n")
        lives <- 8
      } else if (diff == "2") {
        med <- wordlist[nchar(wordlist) >= 6 & nchar(wordlist) <= 8]
        word <- sample(med, 1)
        cat("Medium it is!\n")
        cat("You have 12 lives.\n")
        lives <- 12
      } else if (diff == "3") {
        hard <- wordlist[nchar(wordlist) >= 9]
        word <- sample(hard, 1)
        cat("You chose hard! Looks like you want a challenge.\n")
        cat("You have 15 lives.\n")
        lives <- 15
      } else {
        # If they choose anything other than 1-3, we will choose easy for them
        print("Invalid selection. Let's try easy!")
        easy <- wordlist[nchar(wordlist) >= 2 & nchar(wordlist) <= 5]
        word <- sample(easy, 1)
        lives <- 8
      }
      
      cont2 <- readline(prompt = "Enter R to begin: ")
      while (TRUE) {
        if (cont2 == "R" | cont2 == "r") {
          # Display the masked word (_ _ _ _) based on number of characters for user
          masked_word <- rep("_", nchar(word))
          # Create an empty character vector to store all the guessed letters
          guessed_letters <- character()
          print(paste("Your word has", nchar(word), "characters!"))
          break 
        } else {
          # If user doesn't enter R to continue
          print("Oops! Are you sure you entered the right key?") 
        }
      }
      break # Get out of first while loop
    } else {
      print("Oops! Are you sure you entered the right key?") 
    } 
  } # Enter R to continue
  
  # Game loop
  while (TRUE) {
    # Print the masked word and separate underscores with a space
    print(paste(masked_word, collapse = " "))
    cont3 <- readline(prompt = "Guess a letter or a word: ")
    cont3 <- tolower(cont3)  # Change all letters to lower case 
    
    # If the entry is not a single letter or word that is the same length
    if (nchar(cont3) != 1 && nchar(cont3) != nchar(word)) { 
      print("Please enter a single letter or guess a word.")
      next
    }
    
    # No repeat guesses
    if (cont3 %in% guessed_letters) { 
      print("You already guessed that! Try again.")
      next
    } 
    
    # Combine all the prev guessed letters with the new guessed letter and store it
    guessed_letters <- c(guessed_letters, cont3)
    
    # Check if guessed word is same length
    if (nchar(cont3) == nchar(word)) { 
      # Check if the guessed word is the word
      if (cont3 == word) { 
        # Show all correct letters in the masked word 
        masked_word <- strsplit(word, "")[[1]] 
        print("Congrats! You guessed the word!")
        break # Exit the game if the word is correct
      } else {
        # Decrease number of tries if guessed word isn't correct
        lives <- lives - 1
        if (lives != 1) {
          print(paste("Not quite! You have", lives, "tries left. Try again."))
        } else {
          # Grammar for singular try
          print("Not quite! You have 1 try left. Try again.") 
        }
      }
    } else {
      # Check if the guessed letter is in the word
      match <- grepl(cont3, word, ignore.case = TRUE) 
      
      if (any(match)) {
        # If guessed letter is in word, display in masked word
        masked_word <- update_masked_word(word, masked_word, cont3) 
        print("You got it!")
      } else {
        lives <- lives - 1
        if (lives != 1) {
          print(paste("Not quite! You have", lives, "tries left. Try again."))
        } else {
          print("Not quite! You have 1 try left. Try again.") 
        }
      }
    }
    
    if (!("_" %in% masked_word)) {
      print(paste("Congrats! You guessed the word:", word))
      break # Exit game if word is guessed
    } else if (lives == 0) {
      print(paste("You ran out of tries! The word was:", word))
      break # Exit game if user runs out of tries
    }
  }
}

# Give user option to repeat game or exit
repeat {
  intro()
  game()
  
  restart <- readline(prompt = "Do you want to play again? (Y/N): ")
  
  if (tolower(restart) == "y") {
  } else if (tolower(restart) == "n") {
    print("Thank you for hanging out! Have a nice day :)")
    break # Exit out of game
  } else {
    # If user chooses something other than Y or N
    print("Invalid input. Please enter Y or N.")
  }
}
