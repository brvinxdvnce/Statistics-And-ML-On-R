#часть 1

#распределение 
plot_fact_groups <- function(df, variable) {
  df$ГруппаФактов <- cut(
    df$factCount,
    breaks = 3,
    labels = c("Мало", "Норм", "Много"))
  
  bw <- diff(range(df[[variable]], na.rm = TRUE)) / 25
  
  ggplot(df, aes(x = .data[[variable]], fill = ГруппаФактов, color = ГруппаФактов)) +
    geom_histogram(
      aes(y = ..count..),
      binwidth = bw,
      position = "identity",
      alpha = 0.5,
      size = 0.1
    ) +
    geom_freqpoly(
      aes(y = ..count..),
      binwidth = bw,
      linewidth = 1
    ) +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") +
    labs(
      title = paste("Распределение признака", variable),
      x = variable,
      y = "Кол-во языков",
      fill = "Группа по числу фактов",
      color = "Группа по числу фактов"
    )
}

# Функция для вывода статистик по одному этому самому сверху вниз забыл слово
print_stats <- function(x, var_name) {
  cat("Статистики для", var_name, ":\n")
  cat("  Среднее:", mean_custom(x), "\n")
  cat("  Медиана:", median_custom(x), "\n")
  cat("  Мода:", mode_custom(x), "\n")
  cat("  Дисперсия:", variance_custom(x), "\n")
  cat("  СКО:", sd_custom(x), "\n\n")
}

#подсчет матрциы корреляции
my_correlation_matrix <- function(df) {
  numeric_df <- df[sapply(df, is.numeric)]
  n <- ncol(numeric_df)
  result <- matrix(NA, n, n)
  colnames(result) <- rownames(result) <- colnames(numeric_df)
  
  # Вычисляем корреляцию между каждой парой
  for (i in 1:n) {
    for (j in 1:n) {
      x <- numeric_df[[i]]
      y <- numeric_df[[j]]
      valid <- complete.cases(x, y)
      x <- x[valid]
      y <- y[valid]
      mx <- mean(x)
      my <- mean(y)
      cov_xy <- sum((x - mx) * (y - my))
      sd_x <- sqrt(sum((x - mx)^2))
      sd_y <- sqrt(sum((y - my)^2))
      result[i, j] <- cov_xy / (sd_x * sd_y)
    }
  }
  return(result)
}

# среднее (ака матожидание)
mean_custom <- function(x) {
  x <- x[!is.na(x)]
  return(sum(x) / length(x))
}

# медиана
median_custom <- function(x) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n %% 2 == 1) {
    return(x[(n + 1) / 2])
  } else {
    mid1 <- n / 2
    mid2 <- mid1 + 1
    return((x[mid1] + x[mid2]) / 2)
  }
}

# мода
mode_custom <- function(x) {
  x <- x[!is.na(x)]
  uniq_x <- unique(x)
  counts <- sapply(uniq_x, function(val) sum(x == val))
  mode_val <- uniq_x[which.max(counts)]
  return(mode_val)
}

# Дисперсия
variance_custom <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  mu <- mean_custom(x)
  sq_diff <- (x - mu)^2
  return(sum(sq_diff) / (n - 1))
}

# Стандартное отклонение (СКО)
sd_custom <- function(x) {
  sqrt(variance_custom(x))
}

#часть 2

#P-value по формуле Абрамовича–Стегуна
my_pnorm <- function(x) {
  t <- 1 / (1 + 0.2316419 * abs(x))
  d <- 0.3989423 * exp(-x^2 / 2)
  p <- 1 - d * t * 
    (0.3193815 + t * (-0.3565638 + t * (1.781478 + t * (-1.821256 + t * 1.330274))))
  if (x < 0) p <- 1 - p
  return(p)
}

#нормализация
normalize_custom <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

#
gradient_custom <- function(X, y, w) {
  (2 / n) * t(X) %*% (X %*% w - y) 
}

#градиентный спуск
gradient_descent_custom <- function(X, y, lr = 0.01, epsilon = 1e-7, max_iter = 10000) {
  
  w <- matrix(0, ncol(X), 1)
  losses <- c()
  
  for (i in 1:max_iter) {
    grad <- gradient(X, y, w)             # Градиент функции потерь
    w_new <- w - lr * grad                # Обновление весов по градиенту
    current_loss <- loss(X, y, w_new)     # Считаем новую потерю
    losses <- c(losses, current_loss)     # Сохраняем потерю
    
    if (sum(abs(w_new - w)) < epsilon) {
      break
    }
    
    w <- w_new
  }
  
  return(list(weights = w, losses = losses))
}

# Функция потерь: среднеквадратичная ошибка
loss_custom <- function(X, y, w) { 
  n <- nrow(X)
  sum((X %*% w - y)^2) / n 
} 

#
predict_custom <- function(X_new, w) {
  X_new <- cbind(Intercept = 1, as.matrix(X_new))
  return(as.vector(X_new %*% w))
}

#R²: Доля объяснённой дисперсии
#p-value: Статистическая значимость коэффициентов
#RMSE: Средняя ошибка прогноза

# MSE
mse_custom <- function(y_true, y_pred) {
  sum((y_true - y_pred)^2) / length(y_true)
}

# RMSE
rmse_custom <- function(y_true, y_pred) {
  sqrt(mse_custom(y_true, y_pred))
}

# Метрика R^2
r2_custom <- function(y_true, y_pred) {
  ss_res <- sum((y_true - y_pred)^2)
  mean_y <- mean_custom(y_true)
  ss_tot <- sum((y_true - mean_y)^2)
  1 - ss_res / ss_tot
}