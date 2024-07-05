# BTC1855 CODING IN R 
# ASSIGNMENT THREE 
# CRYSTAL LEE
# Version 2024.04.2+764 

# Code review completed by Zachery Chan, comments indicated by ZC 
# ZC - Overall, I thought your hangman game was very fun, well structured, 
#' clean, and loved the game's details/extras! The only things I noticed was that 
#' it would be nice if your code provided an output of incorrect letters after each guess
#' and that it could have some more comments to outline what is beign done to which variables, sometimes hard to keep track.
#' But, I think these things are very minor!  
# 

# Please ensure the assign3-wordlist.txt file is downloaded from my repo :)
# Click source to begin the game/restart after exiting the game

# Welcome and instructions
# Create function to run introductory instructions
intro <- function() {
  # While loop runs the loop infinitely until there is a break
  while (TRUE) {
  cont <- readline(prompt = "Welcome to hangman! Enter R to continue: ")
  # Accept either lower or upper case "R"
  # ZC - Nice use of conditionals here, thought it was much more complicated, nice to see that it is super clean and simple
  if (cont == "R" | cont == "r") {
      cat("The game of hangman is very simple...\n")
      cat("We will choose a word for you.\n")
      cat("Guess one letter at a time.\n")
      cat("Depending on your difficulty level, you have\n")
      cat("a couple tries before you get hanged!\n")
      # ZC - Really love the difficulty settings! Makes the game super fun + the fun messages are great as well 
      break
      
    } else {
      print("Come on, don't leave me hanging! Enter 'R' to get started.") 
    }
  } 
}

game <- function() {
  # Function to update masked word every time there is a correct guess
  update_masked_word <- function(word, masked_word, guess) {
    word <- unlist(strsplit(tolower(word), "")) 
    # ZC - Like the use of unlist here, simplifies code to one line rather than having to drill down 
    for (i in seq_along(word)) { 
      if (word[i] == guess) { 
        masked_word[i] <- word[i]
      }
    }
    return(masked_word)
  }
  
  while (TRUE) {
    cont2 <- readline(prompt = "Enter R to continue: ")
    if (cont2 == "R" | cont2 == "r") { 
      #ZC - if this ever comes up again, try using the toupper() function, then yopu dont have to incase in if statements 
      cat("Easy: 1\n")
      cat("Medium: 2\n")
      cat("Hard: 3\n")
      diff <- readline(prompt = "Choose a difficulty level: ")    
      
      wordlist <- read.delim("assign3-wordlist.txt", header = FALSE)$V1
      
      if (diff == "1") {
        easy <- wordlist[nchar(wordlist) >= 2 & nchar(wordlist) <= 5]
        #ZC - why set nchar(wordlist) >= 2, you never want to use the first word "go"? 
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
        print("Invalid selection. Let's try easy!")
        easy <- wordlist[nchar(wordlist) >= 2 & nchar(wordlist) <= 5]
        word <- sample(easy, 1)
        lives <- 8
      } # ZC - nice 'idiot-proofing' here
      
      while (TRUE) {
        cont2 <- readline(prompt = "Enter R to begin: ")
        if (cont2 == "R" | cont2 == "r") {
          masked_word <- rep("_", nchar(word))
          guessed_letters <- character()
          print(paste("Your word has", nchar(word), "characters!"))
          break 
        } else {
          print("Oops! Are you sure you entered the right key?") 
        }# ZC - nice 'idiot-proofing' here
      }
      break
    } else {
      print("Oops! Are you sure you entered the right key?")
    }# ZC - nice 'idiot-proofing' here
  }
  
  while (TRUE) {
    print(paste(masked_word, collapse = " "))
    cont3 <- readline(prompt = "Guess a letter or a word: ")
    cont3 <- tolower(cont3)  
    
    if (nchar(cont3) != 1 && nchar(cont3) != nchar(word)) { 
      print("Please enter a single letter or guess a word.")
      next
    }
    
    if (cont3 %in% guessed_letters) { 
      print("You already guessed that! Try again.")
      next
    } 
    
    guessed_letters <- c(guessed_letters, cont3)
    
    if (nchar(cont3) == nchar(word)) { 
      if (cont3 == word) { 
        masked_word <- strsplit(word, "")[[1]] 
        print("Congrats! You guessed the word!")
        break
      } else {
        lives <- lives - 1
        if (lives != 1) {
          print(paste("Not quite! You have", lives, "tries left. Try again."))
        } else {
          print("Not quite! You have 1 try left. Try again.") 
        }
      }
    } else {
      match <- grepl(cont3, word, ignore.case = TRUE) 
      #ZC - nice use of grepl here, makes finding matches easier than trying with logicals
      if (any(match)) {
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
      break
    } else if (lives == 0) {
      print(paste("You ran out of tries! The word was:", word))
      break
    }
  }
}

# Give user option to repeat game or exit
repeat {
  intro()
  game()
  
  restart <- readline(prompt = "Do you want to play again? (Y/N): ")
  #ZC - This detail was super cool
  if (tolower(restart) == "y") {
  } else if (tolower(restart) == "n") {
    print("Thank you for hanging out! Have a nice day :)")
    break # Exit out of game
  } else {
    # If user chooses something other than Y or N
    print("Invalid input. Please enter Y or N.")
  }
}
