int1 <- 10
float1 <- 3.14
string1 <- "Hello, World!"


result_division <- round(int1 / float1, 2)
cat("Результат деления:", result_division, "\n")


comparison_result <- int1 > float1
cat("Результат сравнения:", comparison_result, "\n")

# Вывод третьей переменной три раза подряд в одну строку через запятую
cat(string1, ",", string1, ",", string1, "\n")



for (year in 2021:2030) {
  # Используем условный оператор if-else для выбора правильной фразы
  if (year <= 2023) {
    cat("Маркетинговый проект запустился в", year, "году\n")
  } else {
    cat("Маркетинговый проект только запуститься в", year, "году\n")
  }
}
