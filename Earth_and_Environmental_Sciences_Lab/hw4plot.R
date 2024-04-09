library(tidyverse)
setwd("C:/서울대학교/7학기/지구환경과학실험/text/대기")
raw_04 = read.table('230404.txt', sep = "|")
raw_05 = read.table('230405.txt', sep = "|")
raw_06 = read.table('230406.txt', sep = "|")
raw_07 = read.table('230407.txt', sep = "|")

# problem 2

# summarise by hour
raw = rbind(raw_04, raw_05, raw_06, raw_07)[,c('V2', 'V3', 'V11', 'V12')]
raw = as_tibble(raw)
raw <- raw %>%
  mutate(datehour = ymd_h(substring(paste(V2, V3), 1, 13))) %>%
  mutate(pm10 = V11 %>% str_replace("PM10 ", "")) %>%
  mutate(pm2.5 = V12 %>% str_replace("PM2.5 ", ""))
raw <- raw[,c('datehour', 'pm10', 'pm2.5')]
raw <- raw %>%
  mutate(pm10 = as.numeric(pm10)) %>%
  mutate(pm2.5 = as.numeric(pm2.5))
raw <- raw[!duplicated(raw), ]

raw_hour <- raw %>%
  group_by(datehour) %>%
  summarise(totratio = sum(pm10)/sum(pm2.5))

library(ggthemr)
ggthemr("dust")
raw_hour %>% ggplot() +
  geom_line(mapping = aes(x = datehour, y = totratio), size = 1.5) + 
  ylim(c(1, 3)) + 
  scale_x_datetime(date_breaks = "day") + 
  xlab("시간") + 
  ylab("PM10과 PM2.5의 농도 비율") + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))

raw %>%
  group_by(datehour) %>%
  summarise(PM10 = mean(pm10), PM2.5 = mean(pm2.5)) %>%
  gather('PM10', 'PM2.5', key = "type", value = "value") %>% 
  ggplot() +
  geom_line(mapping = aes(x = datehour, y = value, color = type), size = 1.5) + 
  scale_x_datetime(date_breaks = "day") + 
  xlab("시간") + 
  ylab(expression("농도("~mu~"g/"~m^3~")")) + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
  guides(color=guide_legend(title="종류"))

# problem 1
raw_com <- raw %>%
  group_by(datehour) %>%
  summarise(PM10 = mean(pm10), PM2.5 = mean(pm2.5))

library(readxl)
kwan <- read_excel("관악구.xls")[-1,]
kwan <- kwan[, c("날짜", "PM10", "PM2.5")]
kwan <- kwan %>%
  mutate(datehour = paste("2023-", substring(`날짜`, 1, 5), "-", substring(`날짜`, 7, 8), "-00-00", sep = ""))
kwan <- kwan %>%
  mutate(datehour = ymd_hms(datehour))
kwan <- kwan[, c('datehour', 'PM10', 'PM2.5')]

kum <- read_excel("금천구.xls")[-1,]
kum <- kum[, c("날짜", "PM10", "PM2.5")]
kum <- kum %>%
  mutate(datehour = paste("2023-", substring(`날짜`, 1, 5), "-", substring(`날짜`, 7, 8), "-00-00", sep = ""))
kum <- kum %>%
  mutate(datehour = ymd_hms(datehour))
kum <- kum[, c('datehour', 'PM10', 'PM2.5')]

dong <- read_excel("동작구.xls")[-1,]
dong <- dong[, c("날짜", "PM10", "PM2.5")]
dong  <- dong %>%
  mutate(datehour = paste("2023-", substring(`날짜`, 1, 5), "-", substring(`날짜`, 7, 8), "-00-00", sep = ""))
dong <- dong %>%
  mutate(datehour = ymd_hms(datehour))
dong <- dong[, c('datehour', 'PM10', 'PM2.5')]

young <- read_excel("영등포구.xls")[-1,]
young <- young[, c("날짜", "PM10", "PM2.5")]
young  <- young %>%
  mutate(datehour = paste("2023-", substring(`날짜`, 1, 5), "-", substring(`날짜`, 7, 8), "-00-00", sep = ""))
young <- young %>%
  mutate(datehour = ymd_hms(datehour))
young <- young[, c('datehour', 'PM10', 'PM2.5')]

total_data <- rbind(raw_com, kwan, kum, dong, young)
total_data <- total_data %>%
  filter(datehour >= ymd_hms("2023-04-04-00-00-00")) %>%
  filter(datehour <= ymd_hms("2023-04-07-23-00-00"))
total_data <- total_data %>%
  mutate(PM10 = as.numeric(PM10)) %>%
  mutate(PM2.5 = as.numeric(PM2.5))
total_data <- total_data %>%
  gather('PM10', 'PM2.5', key = "type", value = "value")
total_data$location = rep(c(rep("서울대", 96), rep("관악구", 96), rep("금천구", 96), rep("동작구", 96), rep("영등포구", 96)), 2)
total_data <- total_data[, c("location", "datehour", "type", "value")]
library(zoo)
total_data <- total_data %>%
  mutate(value = na.approx(value))


total_data %>%
  filter(type == "PM10") %>%
  ggplot() + 
  geom_line(mapping = aes(x = datehour, y = value, color = location), size = 1) + 
  xlab("시간") + 
  ylab(expression("PM10 농도("~mu~"g/"~m^3~")")) + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
  guides(color=guide_legend(title="관측소")) + 
  ylim(c(0, 300))
  
total_data %>%
  filter(type == "PM2.5") %>%
  ggplot() + 
  geom_line(mapping = aes(x = datehour, y = value, color = location), size = 1) + 
  xlab("시간") + 
  ylab(expression("PM2.5 농도("~mu~"g/"~m^3~")")) + 
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold")) +
  guides(color=guide_legend(title="관측소")) + 
  ylim(c(0, 300))
