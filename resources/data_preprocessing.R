# Data Preprocessing 

################################################################################
# The data we are using are surveys in very long spread sheets across multiple
# years. The survey questions are not exactly the same in each year. We manually
# mapped some important questions into another spread sheet. This code file is 
# to generate new data to use later.  
################################################################################

library(tidyverse)
library(readxl)

years_to_use <- list(
  "2020" = "nyts2020.xlsx",
  "2019" = "nyts2019.xlsx",
  "2018" = "nyts2018.xlsx",
  "2017" = "nyts2017.xlsx"
)

codebook <- read_excel("data/nyts-codebook.xlsx")

path <- "data/NYTS/"

years = c("2020", "2019", "2018")

columns <- colnames(codebook) 
final <- data.frame(matrix(nrow=0, ncol=length(columns)))
colnames(final) <- columns

for (y in years) {
  df <- read_excel(paste0(path, years_to_use[[y]]))
  questions <- codebook %>% filter(Year == y) %>% select(-Year) %>% unlist(., use.names = FALSE)
  selected_df <- df %>% mutate(Year=y) %>% select(Year, all_of(questions))
  colnames(selected_df) <- columns
  final <- rbind(final, selected_df)
}

write_excel_csv(final, "data/nyts_grouped.csv")

