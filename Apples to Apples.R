# Apples to Apples


library(knitr)
library(rmarkdown)
library(tidyverse)
setwd("C:/Users/Laura Shireman/OneDrive/Documents/Personal/Loaded Questions")

AllData <- list(Green = c(LETTERS[1:26], 
                          paste0(LETTERS[1:26], "A"),
                          paste0(LETTERS[1:26], "B")),
                Red = as.character(1:500))

Players <- c("Bean", "Art", "Aiden", "Elliott", "Sam", "Heath", "Dave",
             "Oma", "Opa", "Amadeo", "Avila", "Lucia", "Francesca")

# Green card report
GreenCard <- sample(AllData[["Green"]], 1)
AllData[["Green"]] <- setdiff(AllData[["Green"]], GreenCard)
render("Template for Apples to Apples GREEN cards.Rmd",
       output_file = paste0("Apples to Apples green card for round ", 1, ".html"))


# Round 1

PlayerCards <- list()
for(i in Players){
      PlayerCards[[i]] <- list()
      Red <- sample(AllData[["Red"]], 7, replace = FALSE)
      
      # removing the selected cards from the deck
      AllData[["Red"]] <- setdiff(AllData[["Red"]], Red)

      PlayerCards[[i]][["Red"]] <- data.frame(Cards = Red)
      
      rm(Red)
            
      # Writing the player hands
      render("Template for Apples to Apples RED cards.Rmd",
             output_file = paste0("Apples to Apples red cards for ", i, ".html"))
}

# Round 2 ....

# How would we refresh the deck quickly enough? How would we know what card the
# player played?



