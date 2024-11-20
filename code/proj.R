install.packages("dplyr")
install.packages("tidyr")
install.packages("readr")

library(dplyr)
library(tidyr)
library(readr)

head(df)
str(df)
colnames(df)
class(df)


read_csv("US Armed Forces (6_2024) - Sheet1 (1).csv")

read_csv("US Armed Forces (6_2024) - Sheet1 (1).csv", skip = 2)



head(df)
str(df)
colnames(df)
class(df)

df_grouped <- df %>%
  group_by(`Pay Grade`) %>%
  summarize(
    Army_Male_Total = sum(Army_Male),
    Army_Female_Total = sum(Army_Female),
    Navy_Male_Total = sum(Navy_Male),
    Navy_Female_Total = sum(Navy_Female),
    Total = Army_Male_Total + Army_Female_Total + Navy_Male_Total + Navy_Female_Total
  )
df_individual <- df %>%
  rowwise() %>%
  do(data.frame(
    Pay_Grade = .$`Pay Grade`,
    Gender = c(rep("Male", .$Army_Male), rep("Female", .$Army_Female))
  ))
rank_mapping <- function(pay_grade) {
  case_when(
    pay_grade == "E1" ~ "Private",
    pay_grade == "E2" ~ "Private Second Class",
    TRUE ~ "Unknown Rank"
  )
}
df_individual <- df_individual %>%
  mutate(Rank = rank_mapping(Pay_Grade))


