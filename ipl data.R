# Loading required libraries
library(tidyverse)
library(janitor)
library(lubridate)

# Setting working directory where the ZIP file is located
setwd("D:/Downloads") 

# Unzipping the dataset
unzip("ipl2023 dataset.zip", exdir = "ipl2023_data")

# Loading datasets
matches <- read_csv("ipl2023_data/IPL2023_Matches.csv") %>% clean_names()
scoreboard <- read_csv("ipl2023_data/IPL2023_Match_Scoreboard.csv") %>% clean_names()
batting <- read_csv("ipl2023_data/IPL2023_Batsman.csv") %>% clean_names()
bowling <- read_csv("ipl2023_data/IPL2023_Bowler.csv") %>% clean_names()

# Check basic structure
glimpse(matches)
glimpse(batting)
glimpse(bowling)
glimpse(scoreboard)

team_wins <- matches %>%
  count(winner, name = "wins") %>%
  arrange(desc(wins))

#Total Wins by Team in IPL 2023
ggplot(team_wins, aes(x = reorder(winner, -wins), y = wins, fill = winner)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Total Wins by Team in IPL 2023", x = "Team", y = "Wins") +
  theme_minimal()

top_batsmen <- batting %>%
  group_by(batsman) %>%
  summarise(total_runs = sum(run, na.rm = TRUE)) %>%
  arrange(desc(total_runs)) %>%
  head(10)

#Top 10 Run Scorers
ggplot(top_batsmen, aes(x = reorder(batsman, total_runs), y = total_runs, fill = batsman)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top 10 Run Scorers - IPL 2023", x = "Batsman", y = "Total Runs") +
  theme_minimal()

top_bowlers <- bowling %>%
  group_by(bowler) %>%
  summarise(wickets = sum(wicket, na.rm = TRUE)) %>%
  arrange(desc(wickets)) %>%
  head(10)

#Top 10 Wicket Takers - IPL 2023
ggplot(top_bowlers, aes(x = reorder(bowler, wickets), y = wickets, fill = bowler)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Top 10 Wicket Takers - IPL 2023", x = "Bowler", y = "Wickets") +
  theme_minimal()


