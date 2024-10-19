library(nycflights13)
library(dplyr)
data(flights)
glimpse(flights)

# Bài tập 1: Xét cột dep_delay, arr_delay, air_time và distance. Hãy tạo bảng tóm tắt cho một biến, có
# chứa giá trị trung vị, giá trị lớn nhất, giá trị nhỏ nhất, khoảng tứ phân vị.
flights %>%
  summarise(
    dep_delay_median = median(dep_delay, na.rm = TRUE),
    dep_delay_max = max(dep_delay, na.rm = TRUE),
    dep_delay_min = min(dep_delay, na.rm = TRUE),
    dep_delay_iqr = IQR(dep_delay, na.rm = TRUE))
flights %>%
  summarise(
    arr_delay_median = median(arr_delay, na.rm = TRUE),
    arr_delay_max = max(arr_delay, na.rm = TRUE),
    arr_delay_min = min(arr_delay, na.rm = TRUE),
    arr_delay_iqr = IQR(arr_delay, na.rm = TRUE))
flights %>%
  summarise(
    air_time_median = median(air_time, na.rm = TRUE),
    air_time_max = max(air_time, na.rm = TRUE),
    air_time_min = min(air_time, na.rm = TRUE),
    air_time_iqr = IQR(air_time, na.rm = TRUE))
flights %>%
  summarise(
    distance_median = median(distance, na.rm = TRUE),
    distance_max = max(distance, na.rm = TRUE),
    distance_min = min(distance, na.rm = TRUE),
    distance_iqr = IQR(distance, na.rm = TRUE))


# Bài tập 2: Dữ liệu state.csv cung cấp tỷ lệ tội phạm giết người và dân số của 53 bang của Mỹ. Hãy tạo
# một bảng số liệu tổng hợp cho population và cho murder_rate.
data_state <- read.csv("state.csv")
data_state %>%
  summarise(
    population_min = min(Population, na.rm = TRUE),
    population_max = max(Population, na.rm = TRUE),
    population_mean = mean(Population, na.rm = TRUE),
    population_median = median(Population, na.rm = TRUE),
    population_sd = sd(Population, na.rm = TRUE),
    population_iqr = IQR(Population, na.rm = TRUE))
data_state %>% 
  summarise(
    murder_rate_min = min(Murder.Rate, na.rm = TRUE),
    murder_rate_max = max(Murder.Rate, na.rm = TRUE),
    weighted_mean_murder_rate = weighted.mean(Murder.Rate, na.rm = TRUE))


# Bài tập 3: Tạo bảng kết quả tổng hợp cho các biến dep_delay, arr_delay, air_time, theo các tháng khác
# nhau.
library(dplyr)
library(tidyr)
library(nycflights13)

flights_renamed <- flights %>%
  rename(depDelay = dep_delay,
         arrDelay = arr_delay,
         airTime = air_time)

df_monthly_summary <- flights_renamed %>%
  group_by(month) %>%
  summarise(across(c("depDelay", "arrDelay", "airTime"),
                   list(gtnn = min, gtln = max, tv = median, tb = mean, dlc = sd), na.rm = TRUE))
df_monthly_summary

df_tidy <- df_monthly_summary %>%
  pivot_longer(cols = -month, names_to = "ten", values_to = "gt") %>%
  separate(ten, into = c("bien", "tk"), sep = "_") %>% 
  pivot_wider(names_from = tk, values_from = gt) %>% 
  select(month, bien, gtnn, gtln, tv, tb, dlc)
print.data.frame(df_tidy, digits = 4)


# Bài tập 4:
# 1. Nhập dữ liệu này vào trong không gian làm việc của R, sau đó, kiểm tra xem liệu các tên biến, loại biến
# đã đúng theo quy chuẩn chưa? Nếu chưa hãy hiệu chỉnh lại cho đúng.
library(dplyr)
data_pima <- read.csv("Pima.csv")
glimpse(data_pima)
# 2. Lập bảng thống kê tổng hợp cho 1 biến định lượng (glu)
data_pima %>%
  summarise(gtnn = min(glu, na.rm = TRUE),
            gtln = max(glu, na.rm = TRUE),
            tv = median(glu, na.rm = TRUE),
            tb = mean(glu, na.rm = TRUE),
            dlc = sd(glu, na.rm = TRUE))

# 3. Lập bảng thống kê tổng hợp cho 1 vài biến định lượng bất kỳ (glu, bmi, age)
summary_multiple <- data_pima %>%
  summarise(across(c("glu", "bmi", "age"),
                   list(gtnn = min, gtln = max, tv = median, tb = mean, dlc = sd), na.rm = TRUE))
summary_multiple
df_tidy <- summary_multiple %>%
  gather(ten, gt) %>%
  separate(ten, into = c("bien", "tk"), sep = "_") %>% 
  pivot_wider(names_from = tk, values_from = gt) %>% 
  select(bien, gtnn, gtln, tv, tb, dlc)
print.data.frame(df_tidy, digits = 4)


# Bài tập 5: 
data_birth <- read.table("birthwt.txt", header = TRUE)
data_birth <- data_birth %>%
  mutate(race = factor(race, labels = c("white", "black", "other")),
         smoke = factor(smoke, labels = c("no", "yes")))
# • tạo bảng thống kê tổng hợp cho biến bwt theo các nhóm của trạng thái hút thuốc của mẹ;
summary_by_smoke <- data_birth %>%
  group_by(smoke) %>%
  summarise(n = n(), tb = mean(bwt, na.rm = TRUE), dlc = sd(bwt, na.rm = TRUE))
summary_by_smoke
# • tạo bảng thống kê tổng hợp cho biến bwt theo các nhóm của chủng tộc và trạng thái hút thuốc của mẹ;
summary_by_race_smoke <- data_birth %>%
  group_by(race, smoke) %>%
  summarise(n = n(), tb = mean(bwt, na.rm = TRUE), dlc = sd(bwt, na.rm = TRUE))
summary_by_race_smoke
# • lập lại các bảng trên, nhưng thêm một cột tính độ lệch chuẩn của trung bình mẫu.                                                         
summary_by_smoke_1 <- data_birth %>%
  group_by(smoke) %>%
  summarise(n = n(), tb = mean(bwt, na.rm = TRUE), dlc = sd(bwt, na.rm = TRUE))
summary_by_smoke_1 
summary_by_race_smoke_1 <- data_birth %>%
  group_by(race, smoke) %>%
  summarise(n = n(), tb = mean(bwt, na.rm = TRUE), dlc = sd(bwt, na.rm = TRUE))
summary_by_race_smoke_1



# Bài tập 6:
library(nycflights13)
library(dplyr)
data(flights)
# • Số phút cất cánh trễ của chuyến bay là giống nhau cho các sân bay đi?
flights %>%
  group_by(origin) %>%
  summarise(tb = mean(dep_delay, na.rm = TRUE),
            dlc = sd(dep_delay, na.rm = TRUE))
# Liệu sự trễ chuyến bay có bị ảnh hưởng bởi các tháng trong năm? Tìm các tháng có chuyến bay trễ
# nhất hoặc ít trễ nhất
flights %>%
  group_by(month) %>% 
  summarise(tb = mean(dep_delay, na.rm = TRUE),
            dlc = sd(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(tb))
# Các hãng hàng không có phải nguyên nhân khiến các chuyến bay cất cánh trễ? Tìm các hãng hàng
# không tệ nhất. Liệu sự cất cánh trễ của các hãng hàng không này, có khác biệt giữa các sân bay xuất
# phát?
flights %>%
  group_by(carrier, origin) %>%
  summarise(tb = mean(dep_delay, na.rm = TRUE),
            dlc = sd(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(tb))




# Bài tập 7:
library(ggplot2)
# vẽ lại biểu đồ tần số và biểu đồ boxplot cho biến bwt, thay đổi tên các trục số (sử dụng hàm xlab(),
# ylab() hoặc labs()), hiệu chỉnh màu (sử dụng đối số color = "ten_mau") và hình nền (tìm hiểu các
# hàm theme_bw(), theme_classic() và một số hàm tương tự);
# • sử dụng hàm geom_vline() hoặc hàm geom_hline() để tạo một đường thẳng biểu thị giá trị trung
# bình của bwt, chỉnh màu đường thẳng và kiểu đường thẳng, bằng hai đối số linetype và color;
# histogram
ggplot(data_birth, aes(x = bwt)) +
  geom_histogram(binwidth = 100, fill = "white", color = "black") +
  geom_vline(aes(xintercept = mean(bwt, na.rm = TRUE)),
             color = "red", linetype = "dashed", size = 1) +  
  xlab("Cân nặng khi sinh (gram)") +  
  ylab("Tần số") + 
  labs(title = "Biểu đồ tần số cho cân nặng khi sinh") +  
  theme_bw() 

# boxplot
ggplot(data_birth, aes(y = bwt)) +
  geom_boxplot(fill = "pink", color = "black") +
  geom_hline(aes(yintercept = mean(bwt, na.rm = TRUE)),
             color = "blue", linetype = "dotted", size = 1) +  
  xlab("") +  
  ylab("Cân nặng khi sinh (gram)") +  
  labs(title = "Biểu đồ Boxplot cho cân nặng khi sinh") +  
  theme_classic()  

# vẽ biều đồ tần số và biểu đồ boxplot cho biến age trong dữ liệu data_birth, thay đổi tiêu đề tên trục
# số và màu sắc của biểu đồ;
# sử dụng hàm geom_vline() hoặc hàm geom_hline() để tạo một đường thẳng biểu thị giá trị trung
# bình của age, chỉnh màu đường thẳng và kiểu đường thẳng;

#histogram
ggplot(data_birth, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "white", color = "black") +
  geom_vline(aes(xintercept = mean(age, na.rm = TRUE)),
             color = "purple", linetype = "dashed", size = 1) +
  xlab("Tuổi") +  
  ylab("Tần số") + 
  labs(title = "Biểu đồ tần số cho tuổi của mẹ") +  
  theme_bw()
#boxplot
ggplot(data_birth, aes(y = age)) +
  geom_boxplot(fill = "pink", color = "black") +
  geom_hline(aes(yintercept = mean(age, na.rm = TRUE)),
             color = "green", linetype = "dotted", size = 1) +  
  xlab("") + 
  ylab("Tuổi của mẹ") + 
  labs(title = "Biểu đồ Boxplot cho tuổi của mẹ") +  
  theme_bw() 

# nếu thêm coord_flip() thì ta thu được gì? BIỂU ĐỒ XOAY NGANG
ggplot(data_birth, aes(y = age)) +
  geom_boxplot(fill = "pink", color = "black") +
  geom_hline(aes(yintercept = mean(age, na.rm = TRUE)),
             color = "green", linetype = "dotted", size = 1) +  
  xlab("") + 
  ylab("Tuổi của mẹ") + 
  labs(title = "Biểu đồ Boxplot cho tuổi của mẹ") +  
  theme_bw() +
  coord_flip()  

# Bài tập 8:
# • Tìm hiểu hàm geom_rug(). Nếu kết hợp với đoạn vẽ ước lượng hàm mật độ, kết quả thu được như thế
# nào?
ggplot(data_birth, aes(x = bwt)) +
  geom_density(color = "blue", bw = "nrd0", kernel = "gaussian") +
  geom_rug(sides = "b", color = "red") 

# Kết hợp histogram và biểu đồ hàm mật độ. Chú ý, cần sử dụng aes(y = ..density..) trong hàm
# geom_histogram() để biểu diễn tần số tương đối thay cho tần số tuyệt đối
ggplot(data_birth, aes(x = bwt)) +
  geom_histogram(aes(y = ..density..), binwidth = 100, fill = "blue", color = "black") +
  geom_density(color = "red", bw = "nrd0", kernel = "gaussian")

# Biểu diễn ước lượng mật độ xác suất của cân nặng trẻ sơ sinh tương ứng với hai trạng thái hút thuốc
# của người mẹ. (Hai biểu đồ riêng biệt).
# Vẽ biểu đồ mật độ riêng biệt cho hai trạng thái hút thuốc của mẹ
ggplot(data_birth, aes(x = bwt)) +
  geom_density(aes(color = smoke), bw = "nrd0", kernel = "gaussian") +
  facet_wrap(~smoke, scales = "free_y")

# Sử dụng hàm geom_vline() để vẽ thêm đường thẳng mô tả trung bình của cân nặng của trẻ sơ sinh.
# Điều chỉnh kiểu đường và màu sắc.
ggplot(data_birth, aes(x = bwt)) +
  geom_density(color = "blue", bw = "nrd0", kernel = "gaussian") + 
  geom_vline(aes(xintercept = mean(bwt, na.rm = TRUE)), 
             color = "red", linetype = "dashed", size = 1) 

# Thử với các cách chọn bandwidth và kernel khác nhau.
# Sử dụng kernel Epanechnikov và bandwidth sj
ggplot(data_birth, aes(x = bwt)) +
  geom_density(color = "purple", bw = "sj", kernel = "epanechnikov") 

# Sử dụng Rectangular kernel và bandwidth ucv
ggplot(data_birth, aes(x = bwt)) +
  geom_density(color = "orange", bw = "ucv", kernel = "rectangular") 

# Bài tập 9
# Tìm hiểu hàm facet_wrap(), và áp dụng để chia biểu đồ hộp đã vẽ ở trên thành hai biểu đồ con tương
# ứng trạng thái hút thuốc của người mẹ. Ta có thể nhận xét được gì?
ggplot(data_birth, aes(x = smoke, y = bwt)) +
  geom_boxplot() +
  facet_wrap(~ smoke)
#Nhận xét: trọng lượng sơ sinh của trẻ có mẹ hút thuốc thường thấp hơn so với trẻ có mẹ không hút thuốc

# Vẽ lại biểu đồ boxplot cho biến bwt theo race, nhưng dùng thêm tham số fill = smoke; so sánh kết
# quả thu được ở câu trên, khi sử dụng facet_wrap().
ggplot(data = data_birth, aes(x = race, y = bwt, fill = smoke)) +
  geom_boxplot()   
# So sánh: fill = smoke: cho phép so sánh trực tiếp ảnh hưởng của việc hút thuốc đến trọng lượng của trẻ sơ sinh trong từng nhóm chủng tộc
#facet_wrap(~smoke): tạo ra 2 biểu đồ riêng biệt làm việc so sánh khó khăn hơn 


library(dplyr)
library(ggplot2)
# Tìm hiểu cách sử dụng hàm cut() để tạo mới một biến định tính miêu tả nhóm tuổi của mẹ dựa trên
# dữ liệu age; vẽ biểu đồ boxplot miêu tả cân nặng của trẻ theo nhóm tuổi của mẹ.
data_birth_age <- data_birth %>%
  mutate(age_group = cut(age, 
                         breaks = c(10, 20, 30, 40, 50), 
                         labels = c("10-20", "20-30", "30-40", "40-50")))

ggplot(data_birth_age, aes(x = age_group, y = bwt)) +
  geom_boxplot()

# Lập lại biểu đồ boxplot trên, nhưng có thêm nhân tố trạng thái hút thuốc của mẹ; 
data_birth_age_smoke <- data_birth %>%
  mutate(age_group = cut(age, 
                         breaks = c(10, 20, 30, 40, 50), 
                         labels = c("10-20", "20-30", "30-40", "40-50")),
         smoke = factor(smoke, labels = c("No", "Yes")))
ggplot(data_birth_age_smoke, aes(x = age_group, y = bwt, fill = smoke)) +
  geom_boxplot()

# • Dùng biểu đồ boxplot để miêu tả thay đổi cân nặng của trẻ sơ sinh theo 3 biến định tính: nhóm tuổi
# của mẹ, chủng tộc của mẹ, trạng thái hút thuốc.
data_birth_age_race_smoke <- data_birth %>%
  mutate(age_group = cut(age, 
                         breaks = c(10, 20, 30, 40, 50), 
                         labels = c("10-20", "20-30", "30-40", "40-50")),
         smoke = factor(smoke, labels = c("No", "Yes")),
         race = factor(race, labels = c("White", "Black", "Other")))
ggplot(data_birth_age_race_smoke, aes(x = race, y = bwt, fill = smoke)) +
  geom_boxplot() +
  facet_wrap(~ age_group) + 
  coord_flip() # Thêm coord_flip() để xoay biểu đồ ngang

# Tìm hiểu hàm coord_trans(). Áp dụng để tạo biểu đồ hộp cho log(), sqrt() của cân nặng.
ggplot(data_birth, aes(y = bwt)) + 
  geom_boxplot() +
  coord_trans(y = "log10") 

ggplot(data_birth, aes(y = bwt)) +
  geom_boxplot() +
  coord_trans(y = "sqrt")

ggplot(data_birth, aes(x = factor(1), y = bwt)) + 
  geom_violin() +
  geom_boxplot(width = 0.1) +
  coord_trans(y = "log10") +
  labs(x = "Cân nặng", y = "Cân nặng (log10)")



# Bài tập 10:
# Tìm hiểu hàm facet_wrap(), và áp dụng để chia biểu đồ hộp đã vẽ ở trên thành hai biểu đồ con tương
# ứng trạng thái hút thuốc của người mẹ. Ta có thể nhận xét được gì?
ggplot(data_birth, aes(x = smoke, y = bwt)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  facet_wrap(~ smoke)
#Nhận xét: trọng lượng sơ sinh của trẻ có mẹ hút thuốc thường thấp hơn so với trẻ có mẹ không hút thuốc

# Vẽ lại biểu đồ boxplot cho biến bwt theo race, nhưng dùng thêm tham số fill = smoke; so sánh kết
# quả thu được ở câu trên, khi sử dụng facet_wrap().
ggplot(data = data_birth, aes(x = race, y = bwt, fill = smoke)) +
  geom_violin() +
  geom_boxplot(width = 0.1)      
# So sánh: fill = smoke: cho phép so sánh trực tiếp ảnh hưởng của việc hút thuốc đến trọng lượng của trẻ sơ sinh trong từng nhóm chủng tộc
#facet_wrap(~smoke): tạo ra 2 biểu đồ riêng biệt làm việc so sánh khó khăn hơn 


# Tìm hiểu cách sử dụng hàm cut() để tạo mới một biến định tính miêu tả nhóm tuổi của mẹ dựa trên
# dữ liệu age; vẽ biểu đồ boxplot miêu tả cân nặng của trẻ theo nhóm tuổi của mẹ.
data_birth_age <- data_birth %>%
  mutate(age_group = cut(age, 
                         breaks = c(10, 20, 30, 40, 50), 
                         labels = c("10-20", "20-30", "30-40", "40-50")))

ggplot(data_birth_age, aes(x = age_group, y = bwt)) +
  geom_violin() +
  geom_boxplot(width = 0.1) 

# Lập lại biểu đồ boxplot trên, nhưng có thêm nhân tố trạng thái hút thuốc của mẹ; 
data_birth_age_smoke <- data_birth %>%
  mutate(age_group = cut(age, 
                         breaks = c(10, 20, 30, 40, 50), 
                         labels = c("10-20", "20-30", "30-40", "40-50")),
         smoke = factor(smoke, labels = c("No", "Yes")))
ggplot(data_birth_age_smoke, aes(x = age_group, y = bwt, fill = smoke)) +
  geom_violin() +
  geom_boxplot(width = 0.1)

# • Dùng biểu đồ boxplot để miêu tả thay đổi cân nặng của trẻ sơ sinh theo 3 biến định tính: nhóm tuổi
# của mẹ, chủng tộc của mẹ, trạng thái hút thuốc.
data_birth_age_race_smoke <- data_birth %>%
  mutate(age_group = cut(age, 
                         breaks = c(10, 20, 30, 40, 50), 
                         labels = c("10-20", "20-30", "30-40", "40-50")),
         smoke = factor(smoke, labels = c("No", "Yes")),
         race = factor(race, labels = c("White", "Black", "Other")))
ggplot(data_birth_age_race_smoke, aes(x = race, y = bwt, fill = smoke)) +
  geom_violin() +
  geom_boxplot(width = 0.1) +
  facet_wrap(~ age_group) + 
  coord_flip() # Thêm coord_flip() để xoay biểu đồ ngang

ggplot(data_birth, aes(x = factor(1), y = bwt)) + 
  geom_violin() +
  geom_boxplot(width = 0.1) +
  coord_trans(y = "log10") +
  labs(x = "Cân nặng", y = "Cân nặng (log10)")

ggplot(data_birth, aes(x = factor(1), y = bwt)) + 
  geom_violin() +
  geom_boxplot(width = 0.1) +
  coord_trans(y = "sqrt") +
  labs(x = "Cân nặng", y = "Cân nặng (sqrt)")



#Bài tập 11
# vẽ lại biểu đồ phân tán cho biến bwt theo age, thay đổi tên các trục số, hiệu chỉnh màu, hình dạng của
# điểm và hình nền, sau đó, hãy thử với log của cân nặng;
ggplot(data_birth, aes(x = age, y = bwt)) +
  geom_point(color = "red", shape = 17) 

ggplot(data_birth, aes(x = age, y = log(bwt))) +
  geom_point(color = "blue", shape = 19)

# ghép nối đoạn lệnh vẽ biểu đồ cột phía trên với hàm facet_wrap(∼ smoke) để tách thành hai biểu đồ
# con theo trạng thái hút thuốc của người mẹ; ta có thể nhận xét gì dựa trên biểu đồ về vẽ;
ggplot(data_birth, aes(x = age, y = bwt)) +
  geom_point(color = "red", shape = 17) +
  facet_wrap(~ smoke)

# vẽ lại biểu đồ phân tán cho biến bwt theo age, nhưng dùng thêm tham số color = smoke; so sánh kết
# quả thu được ở câu trên, khi sử dụng facet_wrap().
ggplot(data_birth, aes(x = age, y = bwt, color = smoke)) +
  geom_point(shape = 17) 
# So sánh: dễ quan sát sự tương quan giữa tuổi và cân nặng của trẻ sơ sinh trong từng nhóm hút thuốc khác nhau
# thay vì so sánh giữa 2 biểu đồ riêng lẻ
ggplot(data_birth_age_race_smoke, aes(x = race, y = bwt, fill = smoke)) +
  geom_boxplot() +
  facet_wrap(~ age_group) 


# 2.5
library(corrplot)
data("mtcars")
cor_mtcars <- cor(mtcars, method = "pearson")
# Bài tập 12: Thử các dạng khác nhau của đồ thị bằng đối số method = ... (tìm hiểu các lựa chọn trong
# help page của hàm corrplot()).
corrplot(cor_mtcars, method = "circle")  
corrplot(cor_mtcars, method = "square")  
corrplot(cor_mtcars, method = "ellipse") 
corrplot(cor_mtcars, method = "number")  

# Bài tập 13: Vẽ biểu đồ hệ số tương quan hạng đơn điệu Spearman cho các biến trong dữ liệu mtcars.
cor_mtcars_spearman <- cor(mtcars, method = "spearman")
corrplot(cor_mtcars_spearman, method = "circle")

# Bài tập 14: Vẽ biểu đồ hệ số tương quan cho các biến trong dữ liệu Advertising.csv
advertising <- read.csv("Advertising.csv")
head(advertising)
# lọc biến định lượng
numeric_vars <- advertising[, c("TV", "radio", "newspaper", "sales")]
head(numeric_vars)
cor_advertising <- cor(numeric_vars)