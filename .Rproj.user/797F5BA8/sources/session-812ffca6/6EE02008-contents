library(ggplot2)
library(corrplot)
library(ggcorrplot)

source("functions.R")

set.seed(666)

print("start program\n")

df <- read.csv("pldb.csv") 

selected_columns <- c( 
  "factCount", 
  "bookCount", 
  "numberOfUsers", 
  "exampleCount", 
  "wikipedia.dailyPageViews", 
  "numberOfRepos", 
  "rank"
) 

df <- df[, selected_columns]
df <- na.omit(df) 
df <- df[sample(nrow(df), 200), ]

df$ГруппаФактов <- cut(df$factCount, breaks = 3, labels = c("Мало", "Средне", "Много"))

numeric_df <- df[sapply(df, is.numeric)]
cor_matrix <- my_correlation_matrix(numeric_df)

cor_sums <- apply(abs(cor_matrix), 1, sum) - 1
cor_sums_sorted <- sort(cor_sums, decreasing = TRUE)

most_dependent <- names(which.max(rowSums(abs(cor_matrix) - diag(diag(cor_matrix)))))


#---графики---

print(ggcorrplot(cor_matrix, lab = TRUE, colors = c("blue", "white", "red")))
cat("Наиболее зависимая переменная от других: ", most_dependent, "\n\n")

for (col in selected_columns) {
  print(plot_fact_groups(df, col))
}

for (col in selected_columns) {
  print_stats(df[[col]], col)
}