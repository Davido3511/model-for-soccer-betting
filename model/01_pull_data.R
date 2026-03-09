# First install your data. You use parenthesis to basically specify the word or package you are installing
install.packages("readxl")
install.packages("tidyverse")

# Next you load your data, you don't need the parenthesis.
library(readxl)
library(tidyverse)

# read the first excel sheet to check the data
test<- read_excel("Modeling for Soccer Betting.xlsx", sheet = 1)
print(test)

# look at the first few rows
head(test)

# See all the column names
colnames(test)

#check how many sheets are in the file
excel_sheets("Modeling for Soccer Betting.xlsx")

# Read all 5 sheets
pl_2526 <- read_excel("Modeling for Soccer Betting.xlsx", sheet = "2025-26")
pl_2425 <- read_excel("Modeling for Soccer Betting.xlsx", sheet = "2024-25")
pl_2324 <- read_excel("Modeling for Soccer Betting.xlsx", sheet = "2023-24")
pl_2223 <- read_excel("Modeling for Soccer Betting.xlsx", sheet = "2022-23")
pl_2122 <- read_excel("Modeling for Soccer Betting.xlsx", sheet = "2021-22")

# Add a season column to each so we know which season each game is from
pl_2526$Season <- "2025-26"
pl_2425$Season <- "2024-25"
pl_2324$Season <- "2023-24"
pl_2223$Season <- "2022-23"
pl_2122$Season <- "2021-22"

# Stack them all together into one dataset
pl_all <- bind_rows(pl_2526, pl_2425, pl_2324, pl_2223, pl_2122)

# Check how many games we have
nrow(pl_all)

# Now lets clean the data
pl_clean <- pl_all %>%
  
  # Remove columns we don't need by selecting the ones you actually want to keep
  select(Date, Home, Score, Away, Season) %>%
  
  # Remove rows where there's no score (unplayed games)
  filter(!is.na(Score))%>%
  filter(Score != "Score") %>%
  filter(Home != "Home") %>%
  
  # Fix the date column (convert from Excel number to actual date)
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
  
  # Split the score into Home Goals and Away Goals
  separate(Score, into = c("HomeGoals", "AwayGoals"), sep = "\u2013")%>%
  
    # Convert goals from text to numbers
  mutate(HomeGoals = as.numeric(HomeGoals), 
        AwayGoals = as.numeric(AwayGoals))

print(pl_clean)
head(pl_clean)
nrow(pl_clean) 
  
# Save the clean data as a CSV
write.csv(pl_clean, "pl_clean.csv", row.names = FALSE)  
  
