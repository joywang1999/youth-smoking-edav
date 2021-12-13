# Data Preprocessing 

################################################################################
# The data we are using are surveys in very long spread sheets across multiple
# years. The survey questions are not exactly the same in each year. We manually
# mapped some important questions into another spread sheet. This code file is 
# to generate new data to use later.  
################################################################################


# some code are different between years, need refactor

# [1] "Year"                                                                                                                                                                                   
# [2] "How old are you?"                                                                                                                                                                       
# [3] "What is your sex?"                                                                                                                                                                      
# [4] "What grade are you in?"                                                                                                                                                                 
# [5] "No, not of Hispanic, Latino, Latina or Spanish origin: Are you Hispanic, Latino, Latina, or of Spanish origin? (Select one or more)"                                                    
# [6] "Yes, Mexican, Mexican American, Chicano, or Chicana: Are you Hispanic, Latino, Latina, or of Spanish origin? (Select one or more)"                                                      
# [7] "Yes, Puerto Rican: Are you Hispanic, Latino, Latina, or of Spanish origin? (Select one or more)"                                                                                        
# [8] "Yes, Cuban: Are you Hispanic, Latino, Latina, or of Spanish origin? (Select one or more)"                                                                                               
# [9] "Yes, Another Hispanic, Latino, Latina, or Spanish origin: Are you Hispanic, Latino, Latina, or of Spanish origin? (Select one or more)"                                                 
# [10] "American Indian or Alaska Native: What race or races do you consider yourself to be? (Select one or more)"                                                                              
# [11] "Asian: What race or races do you consider yourself to be? (Select one or more)"                                                                                                         
# [12] "Black or African American: What race or races do you consider yourself to be? (Select one or more)"                                                                                     
# [13] "Native Hawaiian or Other Pacific Islander: What race or races do you consider yourself to be? (Select one or more)"                                                                     
# [14] "White: What race or races do you consider yourself to be? (Select one or more)"                                                                                                         
# [15] "Have you ever smoked a cigarette, even one or two puffs?"                                                                                                                               
# [16] "How old were you when you first smoked a cigarette, even one or two puffs?"                                                                                                             
# [17] "About how many cigarettes have you smoked in your entire life?"                                                                                                                         
# [18] "During the past 30 days, on how many days did you smoke cigarettes?"                                                                                                                    
# [19] "When was the last time you smoked a cigeratte, even one or two puffs? (Please choose the first answer that fits)"                                                                       
# [20] "During the past 30 days, on the days you smoked, about how many cigarettes did you smoke per day? A pack usually has 20 cigarettes in it."                                              
# [21] "During the past 30 days, what brand of cigarettes did you usually smoke?"                                                                                                               
# [22] "Menthol cigarettes are cigarettes that taste like mint. During the past 30 days, were the cigarettes that you usually smoked menthol?"                                                  
# [23] "Have you ever been curious about smoking a cigarette?"                                                                                                                                  
# [24] "Do you think that you will try a cigarette soon?"                                                                                                                                       
# [25] "Do you think that you will smoke a cigarette in the next year?"                                                                                                                         
# [26] "If one of your best friends were to offer you a cigarette, would you smoke it?"                                                                                                         
# [27] "Have you ever smoked a cigar, cigarillo, or little cigar, even one or two puffs?"                                                                                                       
# [28] "How old were you when you first smoked a cigar, cigarillo, or little cigar, even one or two puffs?"                                                                                     
# [29] "Have you ever used chewing tobacco, snuff, or dip, even just a small amount?"                                                                                                           
# [30] "How old were you when you first used chewing tobacco, snuff, or dip, even just a small amount?"                                                                                         
# [31] "Have you ever been curious about using an e-cigarette?"                                                                                                                                 
# [32] "Have you ever used an e-cigarette, even once or twice?"                                                                                                                                 
# [33] "When you are using the Internet, how often do you see ads or promotions for cigarettes or other tobacco products?"                                                                      
# [34] "When you go to a convenience store, supermarket, or gas station, how often do you see ads or promotions for cigarettes or other tobacco products?"                                      
# [35] "When you watch TV or streaming services (such as Netflix, Hulu, or Amazon Prime), or go to the movies, how often do you see ads or promotions for cigarettes or other tobacco products?"
# [36] "When you are using the Internet, how often do you see ads or promotions for e-cigarettes?"                                                                                              
# [37] "When you go to a convenience store, supermarket, or gas station, how often do you see ads or promotions for e-cigarettes?"                                                              
# [38] "When you watch TV or streaming services (such as Netflix, Hulu, or Amazon Prime), or go to the movies, how often do you see ads or promotions for e-cigarettes?"                        
# [39] "How often do you see posts related to e-cigarettes when you go on social media (such as YouTube, Instagram, Snapchat, Twitter, or Facebook)?"                                           


library(tidyverse)
library(readxl)

years_to_use <- list(
  "2020" = "nyts2020.xlsx",
  "2019" = "nyts2019.xlsx",
  "2018" = "nyts2018.xlsx",
  "2017" = "nyts2017.xlsx",
  "2016" = "nyts2016.xlsx"
)

codebook <- read_excel("data/nyts-codebook.xlsx")

factoring <- read_excel("data/nyts_factoring.xlsx")

path <- "data/NYTS/"

years = c("2020", "2019", "2018", "2017", "2016")

columns <- colnames(codebook) 
final <- data.frame(matrix(nrow=0, ncol=length(columns)))
colnames(final) <- columns

missing <- c(".", "E", "N", "S", "Z", ".N", ".S", ".Z", "*", "**")

for (y in years) {
  df <- read_excel(paste0(path, years_to_use[[y]]))
  questions <- codebook %>% filter(Year == y) %>% select(-Year) %>% unlist(., use.names = FALSE)
  selected_df <- df %>% select(any_of(questions))
  
  for (i in 1:length(questions)) {
    if (questions[i] == "-") {
      selected_df <- selected_df %>% add_column(NA, .before=i, .name_repair="unique")
    }
  }
  
  selected_df <- selected_df %>% mutate(Year=y) %>% select(Year, everything())
  
  # recode some characters meaning missing responses to NA
  for (x in missing) {
    selected_df <- selected_df %>% mutate(across(everything(), na_if, x))
  }
  
  colnames(selected_df) <- columns
  
  # selected_df <- mutate_all(selected_df, function(x) as.character(x))
  
  for (col in columns[2:length(columns)]) {
    for (c in (factoring %>% filter(Year==y))$Code) {
      selected_df[selected_df[col] == c & !is.na(selected_df[col]), col] = (factoring %>% filter(Code==c, Year==y) %>% select(col))[[1]]
    }
  }
  
  final <- rbind(final, selected_df)
}

final <- mutate_all(final, function(x) as.factor(x))

write_excel_csv(final, "data/nyts_grouped.csv")
