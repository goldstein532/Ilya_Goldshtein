df <- read.csv("zd3.csv")
print(head(df))
df <- df %>%
  filter(Income > 30000)

print(nrow(df))
df <- df %>%
select(Id, Year_Birth, Education, Marital_Status, Income, Response)
current_year <- 2023
df <- df %>%
  mutate(Age = current_year - Year_Birth)

df <- df %>%
  mutate(Rich_flag = Income > 80000)
print(head(df))
  summary_df <- df %>%
  group_by(Education) %>%  
  summarize(Average_Income = mean(Income)) 
print(summary_df)
df <- df %>%
  left_join(summary_df, by = "Education")
print(head(df))
View(df)
#Удобство для первичного ознакомления, сравнение значений, фильтрация.
gg <- ggplot(df, aes(x = Education, fill = factor(Rich_flag))) +
  geom_bar(position = "dodge") +
  labs(title = "Столбчатая диаграмма по образованию с разделением по Rich_flag",
       x = "Образование",
       y = "Количество наблюдений") +
  scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "blue")) +
  theme_minimal()

# Сохранение графики в файл (например, PNG)
ggsave("my_plot.png", gg, width = 8, height = 6, dpi = 300)

# Вывод пути к сохраненному файлу (опционально)
cat("Графика сохранена в файл: my_plot.png\n")


gg <- ggplot(df, aes(x = Year_Birth)) +
  geom_line(stat = "count", color = "red") +  
  labs(title = "Линейная диаграмма по году рождения",
       x = "Год рождения",
       y = "Количество наблюдений") +
  theme_minimal()
gg
ggsave("line_plot.png", gg, width = 8, height = 6, dpi = 300)
cat("Графика сохранена в файл: line_plot.png\n")
