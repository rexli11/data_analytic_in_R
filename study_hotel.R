# import packages
library(tidyverse)
library(dplyr)
library(skimr)
library(ggplot2)
library(janitor)
library(lubridate)
library(tidyr)
library(ggmap)
library(ColorBrewer)

# --------------------------File loc---------------------------------
# Data Set Locarion
setwd("D:/Github_version_file_R/data_set/data_frame_hotel")

# ------------------------CSV File Import-----------------------------------
# File Check
hotel_df <- read.csv("hotel_bookings.csv")

# ------------------------Check Data Value-----------------------------------
str(hotel_df)
head(hotel_df)
colnames(hotel_df)
View(hotel_df)
nrow(hotel_df) # 119390行
ncol(hotel_df) # 32列

# company > null > 112593
# agent > null > 16340
summary(hotel_df)

# check unique >> chr col
unique(hotel_df$hotel)
unique(hotel_df$meal)
unique(hotel_df$country)
unique(hotel_df$market_segment)
unique(hotel_df$distribution_channel)
unique(hotel_df$deposit_type)
unique(hotel_df$reserved_room_type)
unique(hotel_df$assigned_room_type)
unique(hotel_df$agent)
unique(hotel_df$company)
unique(hotel_df$customer_type)
unique(hotel_df$reservation_status)
unique(hotel_df$is_repeated_guest)

# -------------------------Fix Data----------------------------------
# check na
# na <- 0 and re check
sum(is.na(hotel_df))

if (sum(is.na(hotel_df)) > 0) {
    print(sum(is.na(hotel_df)))
    hotel_df[is.na(hotel_df)] <- 0
} else {
    print("not na")
}

# 將chr轉為factor確認類別levels
hotel_df <- hotel_df %>%
    mutate(
        hotel = as.factor(hotel),
        is_canceled = as.factor(is_canceled),
        meal = as.factor(meal),
        country = as.factor(country),
        market_segment = as.factor(market_segment),
        reserved_room_type = as.factor(reserved_room_type),
        assigned_room_type = as.factor(assigned_room_type),
        deposit_type = as.factor(deposit_type),
        agent = as.factor(agent),
        company = as.factor(company),
        customer_type = as.factor(customer_type),
        reservation_status = as.factor(reservation_status),
        reservation_status_date = as.factor(reservation_status_date),
        is_repeated_guest = as.factor(is_repeated_guest)
    )

# 確認col levels
levels(hotel_df$agent) # null level >> 334
levels(hotel_df$company) # null level >> 353
levels(hotel_df$is_canceled)
levels(hotel_df$is_repeated_guest)
ㄆ
# null值轉換，agent、Company
null_agent <- which(levels(hotel_df$agent) == "NULL")
levels(hotel_df$agent)[null_agent] <- "no_record"

null_company <- which(levels(hotel_df$company) == "NULL")
levels(hotel_df$company)[null_company] <- "no_record"

null_check <- which(levels(hotel_df$is_canceled) == "0")
levels(hotel_df$is_canceled)[null_check] <- "check_in"
null_canceled <- which(levels(hotel_df$is_canceled) == "1")
levels(hotel_df$is_canceled)[null_canceled] <- "canceled"

null_customer <- which(levels(hotel_df$is_repeated_guest) == "0")
levels(hotel_df$is_repeated_guest)[null_customer] <- "new_customer"
null_repeated <- which(levels(hotel_df$is_repeated_guest) == "1")
levels(hotel_df$is_repeated_guest)[null_repeated] <- "repeated_customer"

# -----------------------Merge data------------------------------------
# 增加顧客總數、小孩總數、平均客單(小孩加大人，不含嬰兒)
hotel_df <- hotel_df %>%
    mutate(all_guest = adults + children + babies) %>%
    mutate(all_kids = children + babies) %>%
    mutate(avg_adr = (adr / (adults + children)))

# -------------------Visualization & Analytic--------------------------
# 飯店取消率
ggplot(data = hotel_df) +
    geom_bar(mapping = aes(x = is_canceled, fill = hotel)) +
    scale_fill_manual(values = c("#ffd700", "steelblue")) +
    labs(title = "甚麼樣的原因促使客戶取消?", subtitle = "各飯店取消率") +
    xlab("是否取消") +
    ylab("數量計算")

# 飯店預定狀況
ggplot(data = hotel_df) +
    geom_bar(mapping = aes(x = reservation_status, fill = hotel)) +
    labs(title = "預定後實際入住情形?", subtitle = "各飯店預定關係圖") +
    scale_fill_manual(values = c("deeppink", "mediumseagreen")) +
    xlab("預訂後情形") +
    ylab("數量")

# 是否重複客
ggplot(data = hotel_df) +
    geom_bar(mapping = aes(x = is_repeated_guest, fill = hotel)) +
    labs(title = "是否不斷有回流客?", subtitle = "各飯店新舊旅客關係圖") +
    scale_fill_manual(values = c("mediumblue", "sienna4")) +
    xlab("新/舊客戶") +
    ylab("數量")

# 各月分預定時間分布
ggplot(data = hotel_df) +
    geom_boxplot(mapping = aes(x = arrival_date_month, y = lead_time, fill = hotel)) +
    labs(title = "預定時間與準備作業", subtitle = "訂房日期至到達日期間的時間") +
    scale_fill_manual(values = c("yellowgreen", "orange3")) +
    xlab("月份") +
    ylab("預定時間") +
    theme(axis.text.x = element_text(angle = 45))

# 不同市場的訂單量
ggplot(data = hotel_df) +
    geom_point(mapping = aes(x = market_segment, y = all_guest, color = hotel, shape = hotel)) +
    labs(title = "客戶總量&來源", subtitle = "訂單來源分布情形") +
    xlab("來源") +
    ylab("旅客總量") +
    theme(axis.text.x = element_text(angle = 60)) +
    facet_wrap(~hotel)

# 均客單價
ggplot(data = hotel_df) +
    geom_point(mapping = aes(x = arrival_date_month, y = avg_adr, color = hotel, shape = hotel)) +
    labs(title = "各月客單是否穩定?", subtitle = "平均客單價 / 月") +
    xlab("月份") +
    ylab("平均客單(不含嬰兒)") +
    theme(axis.text.x = element_text(angle = 45)) +
    facet_wrap(~hotel)

# 不同市場的取消率
ggplot(data = hotel_df) +
    geom_bar(mapping = aes(x = is_canceled, fill = market_segment)) +
    labs(title = "訂單增加時入住率是否提升?", subtitle = "訂購市場間的取消率") +
    xlab("是否取消") +
    ylab("數量統計")

# 假日家庭客流量
ggplot(data = hotel_df) +
    geom_point(mapping = aes(x = children, y = stays_in_weekend_nights, fill = children)) +
    labs(title = "周末住宿是否以孩子居多?", subtitle = "孩童總數與周末關係圖") +
    xlab("孩童總數") +
    ylab("周末住宿")

# 平日家庭客流量
ggplot(data = hotel_df) +
    geom_point(mapping = aes(x = children, y = stays_in_week_nights, fill = children)) +
    labs(title = "平日日否均為非家庭旅客?", subtitle = "平日入住與孩童總數關係圖") +
    xlab("孩童總數") +
    ylab("平日住宿")

# 月總客戶量
ggplot(data = hotel_df) +
    geom_point(mapping = aes(x = arrival_date_month, y = all_guest, color = hotel, shape = hotel)) +
    labs(title = "不同飯店每月來客是否平均?", subtitle = "飯店總來客與月份關係") +
    xlab("月份") +
    ylab("旅客總數") +
    theme(axis.text.x = element_text(angle = 60)) +
    facet_wrap(~hotel)

# 旅客預付情形
ggplot(data = hotel_df) +
    geom_point(mapping = aes(x = deposit_type, y = adults, color = hotel, shape = hotel)) +
    labs(title = "旅客付訂與退款方式", subtitle = "預付方式與是否退款關係圖") +
    xlab("預付型態") +
    ylab("旅客(不含孩童)") +
    facet_wrap(~hotel)