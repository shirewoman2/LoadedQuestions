# Loaded Questions scoring

# This script reads a google drive worksheet with people's answers to the game
# "Loaded Questions" and scores those answers. Answers must be in a standard
# format! Anything in the Google sheet that is in rows of higher number than the
# number of players + 1 will be ignored, and anything in the columns of higher
# number than the number of questions + 1 will be ignored.

# !!! Important! You can't just source this document b/c it needs to interact
# with the Google API and get authorization! Run the "Authorization" section,
# give authorization, and then run the rest.

# Authorization -------------------------------------------------------------

library(googlesheets4)
library(googledrive)

MyURL <- "https://docs.google.com/spreadsheets/d/1Sc8_ulUgQjr5HbbDvvnQZzIcg0CkO7rCtNUczYfQvfM/edit#gid=2060014082"

sheets_get(MyURL)

# Housekeeping -----------------------------------------------------------
library(gridExtra)
library(tidyverse)
library(ggpubr)
library(scales)
library(LaurasHelpers)

ThemeLaura <- function (base_size = 12, base_family = "") {
      theme_gray(base_size = base_size, base_family = base_family) %+replace%
            theme(
                  panel.background = element_rect(fill="white", color=NA),
                  panel.grid.minor.y = element_line(color = NA),
                  panel.grid.minor.x = element_line(color = NA),
                  panel.grid.major = element_line(colour = NA),
                  plot.background = element_rect(fill="white", colour=NA),
                  panel.border = element_rect(color="black", fill=NA),
                  strip.background = element_rect(color=NA, fill="white"),
                  legend.background = element_rect(color=NA, fill=NA),
                  legend.key = element_rect(color=NA, fill=NA)
            )
}

scale_colour_discrete <- function(...) scale_colour_brewer(..., palette="Set1")
scale_fill_discrete <- function(...) scale_fill_brewer(... , palette="Set1")

# Call up that theme before plotting graphs.
theme_set(ThemeLaura())

colRainbow <- colorRampPalette(c("gray20", "antiquewhite4", "firebrick3",
                                 "darkorange", "green3", "seagreen3",
                                 "cadetblue", "dodgerblue3", "royalblue4",
                                 "darkorchid4"))

blueGreen <- colorRampPalette(c("green3", "seagreen3", "cadetblue", "dodgerblue3",
                                "royalblue4"))


# Reading in the files -----------------------------------------------------

Questions <- sheets_read(MyURL, sheet = "Questions 03/23")

AllQ <- names(Questions)
names(AllQ) <- c("Player", paste0("Q", 1:(ncol(Questions)-1)))

names(Questions) <- names(AllQ)

Players <- Questions %>% pull(Player) %>% str_to_title()

GuessSheets <- sheets_get(MyURL)$sheets %>% 
      filter(str_detect(name, "[Gg]uess")) %>% 
      pull(name)

# Removing people who did not give standard-format answers for now. 
GuessSheets <- GuessSheets[
      !GuessSheets %in% c("Jim's Guesses", "Aiden's guesses", "Dotty's Guesses")]

Guesses <- list()
for(i in GuessSheets){
      Guesses[[i]] <- sheets_read(MyURL, sheet = i, 
                                  range = cell_limits(ul = c(1, 1),
                                                      lr = c(length(Players), 
                                                             length(AllQ))))
      names(Guesses[[i]]) <- names(AllQ)
      Guesses[[i]] <- Guesses[[i]] %>% 
            mutate(SheetName = i)
}

Guesses <- bind_rows(Guesses) %>% filter(complete.cases(Player)) %>% 
      mutate(Player = str_to_title(Player), 
             SheetName = str_to_title(SheetName))


# Scoring -----------------------------------------------------------------

Guesses <- Guesses %>%
      mutate(Guesser = str_extract(SheetName, str_c(Players, collapse = "|"))) %>% 
      select(-SheetName) %>% 
      gather(key = "Question", value = "Answer", -Player, -Guesser)

Questions <- Questions %>% 
      mutate(Player = str_to_title(Player)) %>% 
      gather(key = "Question", value = "Answer", -Player) %>% 
      rename("CorrectAnswer" = "Answer")

Scores <- Guesses %>% left_join(Questions) %>% 
      mutate(Correct = Answer == CorrectAnswer)

# ScoresByQuestion <- list()
# for(i in names(AllQ)[names(AllQ) != "Player"]){
#       ScoresByQuestion[[i]] <- Scores %>% filter(Question == i) %>% 
#             group_by(Guesser) %>% 
#             
#       
#       
# }
#       
      
Q1 <- Scores %>% filter(Question == "Q1", Correct == TRUE)

# Who had the most correct answers? 
Q1 %>% group_by(Guesser) %>% summarize(NumCorrect = n()) %>% arrange(desc(NumCorrect))

# For each player, who guessed their answer correctly?
Q1 %>% group_by(Player) %>% summarize(WhoWasCorrect = str_c(Guesser, collapse = ", "))




