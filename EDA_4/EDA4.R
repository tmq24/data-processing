library(dplyr)
library(ggplot2)
library(boot)
data_birth <- read.table("birthwt.txt", header = TRUE)
set.seed(123)

# Bài tập 1: Xét dữ liệu birthwt.csv. Sử dụng phương pháp bootstrap để xác định sai số chuẩn và khoảng
# tin cậy cho các tham số sau:
# (a) trung bình của lwt;
# Hàm tính trung bình
boot_mean <- function(data, i) {
  return(mean(data[i]))
}
# Bootstrap
lwt_boot <- boot(data_birth$lwt, statistic = boot_mean, R = 1000)
lwt_boot

boot.ci(lwt_boot, type = "perc")

# Vẽ histogram
ggplot(data.frame(t = lwt_boot$t), aes(x = t)) +
  geom_histogram(fill = "gray80", color = "black", bins = 30) +
  geom_vline(xintercept = lwt_boot$t0, color = "blue", linetype = "dashed") +
  xlab("bootstrap mean lwt") + ylab("Frequency") +
  theme_bw()


# (b) Trung vị của age
# Hàm tính trung vị
boot_median <- function(data, i) {
  return(median(data[i]))
}

# Bootstrap
age_boot <- boot(data_birth$age, statistic = boot_median, R = 1000)
age_boot

boot.ci(age_boot, type = "perc")

# Vẽ histogram
ggplot(data.frame(t = age_boot$t), aes(x = t)) +
  geom_histogram(fill = "gray80", color = "black", bins = 30) +
  geom_vline(xintercept = age_boot$t0, color = "blue", linetype = "dashed") +
  xlab("bootstrap median age") + ylab("Frequency") +
  theme_bw()


# (c) Tương quan giữa lwt và bwt
# Hàm tính tương quan
boot_cor <- function(data, i) {
  return(cor(data[i, "lwt"], data[i, "bwt"]))
}

# Bootstrap
cor_boot <- boot(data_birth, statistic = boot_cor, R = 1000)
cor_boot

boot.ci(cor_boot, type = "perc")

# Vẽ histogram
ggplot(data.frame(t = cor_boot$t), aes(x = t)) +
  geom_histogram(fill = "gray80", color = "black", bins = 30) +
  geom_vline(xintercept = cor_boot$t0, color = "blue", linetype = "dashed") +
  xlab("bootstrap correlation between lwt and bwt") + ylab("Frequency") +
  theme_bw()

# Bài tập 2: Xét dữ liệu state.csv. Hãy xác định khoảng tin cậy bootstrap cho trung bình có trọng số của
# tỷ lệ vụ án giết người.
state_data <- read.csv("state.csv")
# Hàm tính trung bình có trọng số
weighted_mean_murder <- function(data, i) {
  d <- data[i,]
  return(weighted.mean(d$Murder.Rate, d$Population))
}

# Bootstrap
boot_results <- boot(state_data, statistic = weighted_mean_murder, R = 10000)
boot_results

boot.ci(boot_results, type = "perc")


# Bài tập 3: Xét dữ liệu flights. Hãy xác định khoảng tin cậy bootstrap cho trung bình cho số phút cất
# cánh trễ trong cách kịch bản sau:
library(nycflights13)
library(dplyr)
library(boot)
library(ggplot2)
set.seed(224)

# Hàm tính trung bình
boot_mean <- function(data, i) {
  return(mean(data[i], na.rm = TRUE))
}
# (a) Mẫu ngẫu nhiên của 250 chuyến bay bất kỳ trong năm 2013
sample_a <- flights %>% 
  slice_sample(n = 250) %>% 
  pull(dep_delay)

boot_a <- boot(sample_a, statistic = boot_mean, R = 1000)
boot_a
boot.ci(boot_a, type = "perc")

# (b) một mẫu ngẫu nhiên của 300 chuyến bay bất kỳ của hãng United Airline
sample_b <- flights %>% 
  filter(carrier == "UA") %>% 
  slice_sample(n = 300) %>% 
  pull(dep_delay)

boot_b <- boot(sample_b, statistic = boot_mean, R = 1000)
boot_b
boot.ci(boot_b, type = "perc")

# (c) một mẫu ngẫu nhiên của 250 chuyến bay bất kỳ của hãng Delta
sample_c <- flights %>% 
  filter(carrier == "DL") %>% 
  slice_sample(n = 250) %>% 
  pull(dep_delay)

boot_c <- boot(sample_c, statistic = boot_mean, R = 1000)
boot_c
boot.ci(boot_c, type = "perc")

# Bài tập 4: Xây dựng khoảng tin cậy bootstrap cho sự khác biệt giữa trung bình số phút cất cánh trễ của
# hai mẫu ngẫu nhiên được tạo trong câu (b) và (c) của bài tập 3.
# Lấy lại mẫu từ bài 3 (b) và (c)
library(tidyr)
set.seed(123)
sample_b <- flights %>% 
  filter(carrier == "UA") %>% 
  sample_n(300) %>% 
  pull(dep_delay)

sample_c <- flights %>% 
  filter(carrier == "DL") %>% 
  sample_n(250) %>% 
  pull(dep_delay)

mean_diff_1000 <- replicate(
  n = 1000,
  expr = {
    resample_UA <- sample(sample_b, size = 300, replace = TRUE)
    resample_DL <- sample(sample_c, size = 250, replace = TRUE)
    mean(resample_UA, na.rm = TRUE) - mean(resample_DL, na.rm = TRUE)
  }
)
mean_diff_1000
bootstrap_distn <- tibble(resample_mean_diff = mean_diff_1000)
bootstrap_distn

ggplot(bootstrap_distn, aes(x = resample_mean_diff )) +
  geom_histogram( bins = 30, fill = "gray80", color = "black") 


n1 <- length(sample_b)
n2 <- length(sample_c)
B = 1000
diff = numeric(B)
for (i in 1:B) {
  sample1 <- sample(sample_b, n1, replace = TRUE)
  sample2 <- sample(sample_c, n2, replace = TRUE)
  diff[i] <- mean(sample1, na.rm = TRUE) - mean(sample2, na.rm = TRUE)
}

hist(diff, breaks = 30, col = "gray80", border = "black")
quantile(diff, probs =  c(0.025, 0.5, 0.975))


# Bài tập 5: Xét dữ liệu flights. Chọn ngẫu nhiên một mẫu với cỡ 1000 chuyến bay trong các tháng mùa
# đông (tháng 11, 12, 1).
winter_flights <- flights %>%
  filter(month %in% c(11, 12, 1)) %>%
  slice_sample(n = 1000)

# (a) Tính tỷ lệ chuyến bay có số phút cất cánh trễ nhiều hơn 30 phút.
mean(winter_flights$dep_delay > 30, na.rm = TRUE)

# (b) Xác định khoảng tin cậy bootstrap cho tỷ lệ chuyến bay có số phút cất cánh trễ nhiều hơn 30 phút.
boot_prop <- function(data, i) {
  return(mean(data$dep_delay[i] > 30, na.rm = TRUE))
}

boot_prop <- boot(winter_flights, statistic = boot_prop, R = 1000)
boot_prop
boot.ci(boot_prop, type = "perc")


# (c) Xác định khoảng tin cậy bootstrap cho chênh lệch tỷ lệ giữa tỷ lệ chuyến bay có số phút cất cánh trễ
# nhiều hơn 30 phút của hai hãng EV và DL
ev_dl_flights <- flights %>%
  filter(carrier %in% c("EV", "DL"), month %in% c(11, 12, 1))

boot_diff_prop <- function(data, i) {
  ev_prop <- mean(data$dep_delay[data$carrier == "EV"][i] > 30, na.rm = TRUE)
  dl_prop <- mean(data$dep_delay[data$carrier == "DL"][i] > 30, na.rm = TRUE)
  return(ev_prop - dl_prop)
}

boot_diff_prop <- boot(ev_dl_flights, statistic = boot_diff_prop, R = 1000)
boot_diff_prop
boot.ci(boot_diff_prop, type = "perc")







