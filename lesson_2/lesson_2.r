vector_str <- c("str1", "str2", "str3")
vector_int <- c(1, 2, 3, 4, 5)
print(vector_str)
print(vector_int)


vector_int <- c(1, 2, 3, 4, 5)
cat("Длина второго вектора:", length(vector_int), "\n")
cat("Первый элемент второго вектора:", vector_int[1], "\n")
cat("Элементы со второго по четвёртый:", vector_int[2:4], "\n")


vector1 <- c(1, 2, 3, 4, 5)
vector2 <- c(6, 7, 8, 9, 10)
result <- vector1 + vector2
print(result)


mean_value <- mean(vector1)
cat("Среднее значение элементов вектора:", mean_value, "\n")




divide_by_two <- function(X) {
  result <- X / 2
  return(result)
}

# Пример использования функции
input_value <- 12
output_value <- divide_by_two(input_value)

# Вывод результата на экран
cat("Входное значение:", input_value, "\n")
cat("Результат деления на два:", output_value, "\n")





divide_by_two <- function(X) {
  result <- X / 2
  return(result)
}

vector1 <- c(1, 2, 3, 4, 5)
result_vector <- divide_by_two(vector1)
cat("Исходный вектор:", vector1, "\n")
cat("Результат применения функции к вектору:", result_vector, "\n")



matrix1 <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
matrix2 <- matrix(c(9, 8, 7, 6, 5, 4, 3, 2, 1), nrow = 3, ncol = 3)
cat("Матрица 1:\n")
print(matrix1)
cat("Матрица 2:\n")
print(matrix2)



cat("Первый столбец матрицы 1:\n")
print(matrix1[, 1])
cat("Вторая строка матрицы 2:\n")
print(matrix2[2, ])

cat("Элемент на пересечении двух диагоналей матрицы 1:\n")
print(matrix1[1, 3])



df <- read.csv("ДЗ2_vgsales.csv", header = TRUE, sep = ",")
cat("Первые несколько строк данных:\n")
print(head(df))
cat("\nОбщая структура данных:\n")
str(df)
cat("\nОбщая статистика данных:\n")
print(summary(df))
