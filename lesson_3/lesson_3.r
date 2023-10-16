library(dplyr)
df11 <- read.csv(file = "R/Lesson3/ДЗ3_superstore_data.csv", header = TRUE, encoding = "utf-8")  # nolint
print(head(df11, 3))
# 1
df1 <- df11 %>% filter(Income > 30000)
# 2
df1 <- df1 %>% select("Id", "Year_Birth", "Education", "Marital_Status", "Income", "Response")
# 3
df1 <- mutate(df1, Age = 2023 - Year_Birth,RichFlag = ifelse(Income >80000, "True","False")) 
# 4
df2 <- df1 %>% group_by(Education) %>% summarize(mean = mean(Income))
# 5
df3 <- left_join(df1,df2,by = "Education")