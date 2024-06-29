# BTC1855 CODING IN R 
# ASSIGNMENT THREE 
# CRYSTAL LEE
# Version 2023.03.1+446

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
        
while (TRUE) {
  cont2 <- readline(prompt = "Click R to continue: ")
  if (cont == "R" | cont == "r") {
    wordlist <- read.delim("assign3-wordlist.txt", header = F)
    word <- sample(wordlist$V1, 1)
    print(paste("Your word has", nchar(word), "characters!"))
    break
  } else {
    print("Oops! Are you sure you pressed the right button?") 
  }
} 




      