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

# 1 - 4

X <- df$factCount 
 
n <- length(X) 
mean_X <- mean_custom(X) 
sd_X <- sd_custom(X) 
mu0 <- 100  
alpha <- 0.05
t_stat <- (mean_X - mu0) / (sd_X / sqrt(n)) 
#t_crit <- 1.972 
t_crit <- qt(1 - alpha/2, df = n-1)
margin_error <- t_crit * sd_X / sqrt(n) 
CI_lower <- mean_X - margin_error 
CI_upper <- mean_X + margin_error 

cat("t-статистика:", t_stat, "\n")
cat("Критическое значение t:", t_crit, "\n")
cat("Доверительный интервал:", CI_lower, "-", CI_upper, "\n")

if(abs(t_stat) > t_crit){
  cat("Отвергаем нулевую гипотезу при значении", alpha, "\n")
} else {
  cat("Нет оснований отвергать нулевую гипотезу на значении", alpha, "\n")
}

p_value <- 2 * (1 - my_pnorm(abs(t_stat)))
cat("P-value:", p_value, "\n")
if(p_value < alpha){
  cat("Отвергаем нулевую гипотезу (p-value < α)\n")
} else {
  cat("Не отвергаем нулевую гипотезу (p-value ≥ α)\n")
}

# 5

df_norm <- df 
#зачем? градику нужны значения 0-1
for (col in colnames(df)) { 
  if (is.numeric(df[[col]])) { 
    df_norm[[col]] <- normalize_custom(df[[col]]) 
  } 
} 

X <- as.matrix(df_norm[, -1])                 # Признаки
y <- as.matrix(df_norm$factCount)             # Целевая переменная
n <- nrow(X)                                  # Размер выборки
X <- cbind(Intercept = 1, X)                  # Добавляем столбец единиц


result <- gradient_descent_custom(X, y)
losses <- result$losses

plot(losses, type = "l", col = "blue", lwd = 2,
     xlab = "Итерация", ylab = "Значение функции потерь",
     main = "Сходимость градиентного спуска")

set.seed(666)
n <- nrow(df_norm)
train_indices <- sample(1:n, size = 0.8 * n)
df_train <- df_norm[train_indices, ]
df_test <- df_norm[-train_indices, ]

X_train <- as.matrix(df_train[, -1])
y_train <- as.matrix(df_train$factCount)
X_train <- cbind(Intercept = 1, X_train)

#6

result <- gradient_descent_custom(X_train, y_train)
w <- result$weights

X_test <- df_test[, -1]
y_test <- df_test$factCount

y_pred <- predict_custom(X_test, w)

plot(y_test, y_pred,
     xlab = "Реальные значения",
     ylab = "Предсказания",
     main = "Сравнение реальных и предсказанных значений",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)  # идеальная линия

plot(y_test, type = "l", col = "black", lwd = 2,
     ylab = "Значение",
     main = "Фактические vs Предсказанные значения")
lines(y_pred, col = "blue", lwd = 2)
legend("topright", legend = c("Фактические", "Предсказания"),
       col = c("black", "blue"), lwd = 2)


# Разделение выборки на train/test 80/20
set.seed(666)
idx <- sample(1:n, size = floor(n * 0.8))

df_train <- df_norm[idx, ]
df_test <- df_norm[-idx, ]

X_train <- as.matrix(df_train[, -which(names(df_train) == "factCount")])
y_train <- as.matrix(df_train$factCount)

X_test <- as.matrix(df_test[, -which(names(df_test) == "factCount")])
y_test <- as.matrix(df_test$factCount)

X_train <- cbind(Intercept = 1, X_train)
X_test <- cbind(Intercept = 1, X_test)

model_train <- gradient_descent_custom(X_train, y_train)
w_final <- model_train$weights

#7

y_pred <- predict_custom(X_test[, -1], w_final)  # если predict_custom сама добавляет intercept, то можно без -1

model_lm <- lm(factCount ~ ., data = df_train)

predictions_lm <- predict(model_lm, df_test)

#8

#R²: Доля объяснённой дисперсии
#p-value: Статистическая значимость коэффициентов
#RMSE: Средняя ошибка прогноза

mse_val <- mse_custom(y_test, y_pred)
rmse_val <- rmse_custom(y_test, y_pred)
r2_val <- r2_custom(y_test, y_pred)

cat("\nМетрики кастомки:\n")
cat("MSE:", mse_val, "\n")
cat("RMSE:", rmse_val, "\n")
cat("R²:", r2_val, "\n\n")

#9

mse_lm <- mse_custom(y_test, predictions_lm)
rmse_lm <- rmse_custom(y_test, predictions_lm)
r2_lm <- r2_custom(y_test, predictions_lm)

cat("Метрики модели lm():\n")
cat("MSE:", mse_lm, "\n")
cat("RMSE:", rmse_lm, "\n")
cat("R²:", r2_lm, "\n")