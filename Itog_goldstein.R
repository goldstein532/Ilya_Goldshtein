library(shiny)
library(dplyr)
library(ggplot2)
library(reactable)

# Загрузка данных
data <- read.csv("D:/3uToolsV3/RRRR/data/transaction_data.csv")

# Преобразование времени транзакции в удобный формат и добавление столбца с периодом дня
data$TRANS_TIME <- as.POSIXct(strptime(data$TRANS_TIME, "%H%M"))
data$Time_Period <- ifelse(data$TRANS_TIME < as.POSIXct("12:00:00", format="%H:%M:%S"), "Before Noon", "After Noon")

# Подсчет числа транзакций по периоду дня
transactions_by_period <- table(data$Time_Period)

# Создание столбца с часом транзакции и подсчет числа транзакций по часам
data$Hour <- format(data$TRANS_TIME, format = "%H")
transactions_by_hour <- as.data.frame(table(data$Hour))
colnames(transactions_by_hour) <- c("Hour", "Transactions")

# Заполнение нулями для отсутствующих часов
all_hours <- sprintf("%02d", 0:23)
transactions_by_hour <- merge(transactions_by_hour, data.frame(Hour = all_hours), by = "Hour", all = TRUE)
transactions_by_hour[is.na(transactions_by_hour)] <- 0

# Сортировка по часам
transactions_by_hour <- transactions_by_hour[order(as.numeric(transactions_by_hour$Hour)), ]

# Определим интерфейс
ui <- fluidPage(
  titlePanel("Интерактивный дашборд"),
  
  sidebarLayout(
    sidebarPanel(
      # Можно добавить элементы управления, если нужно
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Графики", plotOutput("transactions_plot")),
        tabPanel("Топ-10 товаров", reactableOutput("top_products")),
        tabPanel("Топ-10 магазинов", reactableOutput("top_stores")),
        tabPanel("Информация о кампаниях", reactableOutput("campaign_info"))
        # Дополнительные вкладки, если нужно
      )
    )
  )
)

# Определим серверную часть
server <- function(input, output) {
  # График количества покупок по каждому часу дня
  output$transactions_plot <- renderPlot({
    ggplot(transactions_by_hour, aes(x = Hour, y = Transactions)) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Количество покупок по каждому часу дня",
           x = "Часы дня",
           y = "Количество покупок") +
      theme_minimal()
  })
  
  # Топ-10 товаров по количеству и сумме продаж
  output$top_products <- renderReactable({
    reactable(
      top_products_quantity,
      searchable = TRUE,
      defaultPageSize = 10
    )
  })
  
  # Топ-10 магазинов
  output$top_stores <- renderReactable({
    reactable(
      stores_without_top_products,
      searchable = TRUE,
      defaultPageSize = 10
    )
  })
  
  # Информация о кампаниях
  output$campaign_info <- renderReactable({
    # Загрузка данных
    campaign_data <- read.csv("D:/3uToolsV3/RRRR/data/campaign_desc.csv")
    transaction_data <- read.csv("D:/3uToolsV3/RRRR/data/transaction_data.csv")
    
    # 1. Текущее количество запущенных маркетинговых кампаний
    current_campaigns_count <- campaign_data %>%
      filter(START_DAY <= Sys.Date() & END_DAY >= Sys.Date()) %>%
      nrow()
    
    # 2. Количество клиентов, которые совершили покупки
    unique_customers_count <- transaction_data %>%
      distinct(household_key) %>%
      nrow()
    
    # 3. Средняя сумма покупок на одну семью
    average_purchase_per_family <- transaction_data %>%
      group_by(household_key) %>%
      summarise(total_sales = sum(SALES_VALUE)) %>%
      summarise(average_sales = mean(total_sales, na.rm = TRUE))
    
    # 4. Общее количество проданных товаров
    total_sold_items <- sum(transaction_data$QUANTITY)
    
    # Создание reactable с данными
    reactable(
      data.frame(
        "Показатель" = c("Текущее количество запущенных кампаний",
                         "Количество клиентов, совершивших покупки",
                         "Средняя сумма покупок на одну семью",
                         "Общее количество проданных товаров"),
        "Значение" = c(current_campaigns_count,
                       unique_customers_count,
                       average_purchase_per_family$average_sales,
                       total_sold_items)
      ),
      searchable = TRUE
    )
  })
}

# Запустим приложение Shiny
shinyApp(ui = ui, server = server)
