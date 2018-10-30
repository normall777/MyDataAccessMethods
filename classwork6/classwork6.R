#finstall.packages('gapminder')
library(gapminder)
install.packages('tidyverse')
install.packages('dplyr')
library(dplyr)
#Функция фильтр
filter(gapminder, lifeExp < 29)
filter(gapminder, country == "Afghanistan", year > 1981)
filter(gapminder, continent %in% c("Asia", "Africa"))
#Тоже самое для векторов
gapminder[gapminder$lifeExp < 29, ]
subset(gapminder, country == "Rwanda")



head(gapminder)
gapminder %>% head(3)


head(select(gapminder, year, lifeExp),4)
#Ниже то же самое, но с пайпом
gapminder %>%
  select(year, lifeExp) %>%
  head(4)

gapminder %>%
  filter(country == "Cambodia") %>%
  select(year, lifeExp)
#Ниже то же самое
gapminder[gapminder$country == "Cambodia", c("year", "lifeExp")]

#Для демонстрации следующих функций загрузим другой датасет
msleep <- read.csv("https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv")
head(msleep)
#Упорядочить по одной колонке
msleep %>% arrange(order) %>% head
#По нескольким
msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>% 
  head
#Отфильтруем и отсортируем по убыванию
msleep %>% 
  arrange(order, sleep_total) %>% 
  select(name, order, sleep_total) %>%
  filter(sleep_total >= 16)

#Добавление колонок
msleep %>%
  select(name, sleep_rem, sleep_total) %>% 
  mutate(rem_proportion = sleep_rem / sleep_total) %>%
  head
#Получение итогов
msleep %>% 
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total),
            max_sleep = max(sleep_total),
            total = n())

msleep %>% 
  group_by(order) %>%
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total), 
            max_sleep = max(sleep_total),
            total = n())


library("magrittr")
library("dplyr")
#install.packages("nycflights13")
library("nycflights13")
flights <- tbl_df(flights)
flights
glimpse(flights) #Позволяет посмотреть немного информации про каждую колонку в таблице
flights %>% filter(dest == "MSN") # Сколько полетов было в Мэдисон
flights %>% filter(dest == "MSN", month == 1, day <=7) # Сколько полетов было в Мэдисон в первую неделю января
flights %>% # Оператор ИЛИ
  filter(dest == "MSN" | dest == "ORD" | dest == "MDW")
flights %>%
  filter(is.element(dest, c("MSN", "ORD", "MDW")))
flights %>% 
  arrange(origin, year, month, day) # Сортировка
flights %>% 
  filter(dest == "MSN") %>%
  arrange(desc(dep_delay)) #Сортировка по убыванию

flights %>%
  select(origin, year, month, day) #Выбор нужных колонок

flights %>%
  select(origin, year:day, starts_with("dep")) #Выбор разных колонок

flights %>%
  select(-dest, -starts_with("arr"),
         -ends_with("time")) # Исключение лишних колонок

# convert to a tbl_df - nice print method
# glimpse - some of each column
# filter - subsetting
# arrange - sorting (desc to reverse the sort)
# select - picking (and omiting) columns

flights %>% 
  rename(y = year, m = month, d = day) # Переименование колонок

flights %>%
  mutate(
    gain = arr_delay - dep_delay,
    speed = (distance / air_time) * 60,
    gain_per_hour = gain / (air_time / 60)) %>%
  select(gain:gain_per_hour)

aggregate(dep_delay ~ month, flights, mean, subset = flights$dest =="MSN") #Среднее время задержки в месяц

msn_by_month <- flights %>%
  filter(dest == "MSN") %>%
  group_by(month) 
msn_by_month 
  
msn_by_month %>% 
  summarise(
    flights = n(),
    avg_delay = mean(dep_delay, na.rm = TRUE),
    n_planes = n_distinct(tailnum)) # Тоже позволяет найти среднюю задержку в месяц

flights %>%
  group_by(dest, month) %>%
  tally # Показывает число элементов в группе

msn_by_month %>% ungroup # Разгруппировка

# day gets peeled off
per_day <- flights %>%
  group_by(dest, year, month, day) %>%
  summarise(flights = n())
per_day

per_month <- per_day %>%
  summarise(flights = sum(flights))
per_month

per_year <- per_month %>%
  summarise(flights = sum(flights))
per_year #summarise отслаивается от одного слоя группировки

flights %>%
  group_by(dest, month) %>%
  mutate(timely = row_number(dep_delay),
         late = row_number(desc(dep_delay))) %>%
  select(dep_delay, timely:late) # Ранжирует задержки рейсов в пределах каждого пункта назначения и месяца

mean_center <- function(xs) {
  xs - mean(xs, na.rm = TRUE)
}
flights %>%
  group_by(dest, month) %>%
  mutate(c_delay = mean_center(dep_delay)) %>%
  select(dep_delay, c_delay)

#That covers 80% of dplyr
#select
#filter
#arrange
#glimpse
#rename
#mutate
#group_by, ungroup
#summarise

#Other 20%
#assembly: bind_rows, bind_cols
#column-wise operations: mutate_each, summarise_each
#join tables together: left_join, right_join, inner_join, full_join
#filtering joins: semi_join, anti_join
#do: arbitrary code on each chunk
#different types of tabular data (databases, data.tables)