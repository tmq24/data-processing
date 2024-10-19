library(dplyr)
library(nycflights13)
data(flights)
glimpse(flights)

# Bài tập 1: Trong một quy trình duy nhất cho từng điều kiện, hãy tìm tất cả các chuyến bay đáp ứng điều
# kiện:
#   • Đến nơi trễ từ hai giờ trở lên
flights %>% 
  filter(arr_delay >= 120) %>% 
  arrange(desc(arr_delay))

# • Bay tới Houston (IAH hoặc HOU)
flights %>% 
  filter(dest %in% c("IAH", "HOU"))

# • Được điều hành bởi United, American hoặc Delta
flights %>% 
  filter(carrier %in% c("UA", "DL", "AA"))
# • Khởi hành vào mùa hè (tháng 7, tháng 8, tháng 9)
flights %>% 
  filter(month %in% c(7, 8, 9))

# • Đến muộn hơn hai tiếng nhưng không cất cánh muộn
flights %>% 
  filter(arr_delay >= 120 & dep_delay <= 0)

# • Bị trì hoãn ít nhất một giờ nhưng lại kéo dài hơn 30 phút trên chuyến bay
flights %>% 
  filter(dep_delay >= 60 & dep_delay - arr_delay > 30)

# Bài tập 2: Sắp xếp flights để tìm chuyến bay có thời gian khởi hành trễ nhất. Tìm các chuyến bay khởi
# hành sớm nhất vào buổi sáng.
flights  %>% 
  arrange(desc(dep_delay), sched_dep_time)  %>% 
  relocate(dep_delay, sched_dep_time)



# Bài tập 3: Sắp xếp flights để tìm chuyến bay có vận tốc nhanh nhất. (Gợi ý: Hãy thử đưa phép tính toán
#                                                                      vào bên trong hàm của bạn.)
flights  %>% 
  mutate(speed = distance / (air_time / 60))  %>% 
  arrange(desc(speed))  %>% 
  relocate(speed, carrier, origin, dest)

#  Bài tập 4: Các chuyến bay hàng ngày trong năm 2013, đúng hay không?
flights  %>% 
  distinct(year, month, day)  %>% 
  nrow()


# Bài tập 5: Chuyến bay nào có quãng đường xa nhất? Chuyến nào có quãng đường ngắn nhất?
flights  %>% 
  arrange(desc(distance))  %>% 
  relocate(distance)

flights  %>% 
  arrange(distance)  %>% 
  relocate(distance)


# Bài tập 6: So sánh các biến dep_time, sched_dep_time và dep_delay, liệu chúng có mối liên hệ nào với nhau.
flights  %>% 
  relocate(dep_time, sched_dep_time, dep_delay)
trans_time_delay <- function(sched, delay){
  temp <- sched %/% 100 * 60 + sched %% 100 + delay
  res <- temp %/% 60 * 100 + temp %% 60
  res <- if_else(res > 2400, res - 2400, res)
  return(res)
}
flights_6 <- flights  %>% 
  mutate(dep_time_guess = trans_time_delay(sched_dep_time, dep_delay))  %>% 
  relocate(dep_time, dep_time_guess, sched_dep_time, dep_delay)
flights_6
all.equal(flights_6$dep_time, flights_6$dep_time_guess)


# Bài tập 7: Tìm hiểu các hàm starts_with, ends_with, contains(). Áp dụng chúng vào trong nhiệm vụ
# lựa chọn các cột dep_time, dep_delay, arr_time và arr_delay.
# 1. sử dụng starts_with():
flights  %>% 
  select(starts_with(c("dep", "arr")))
# 2. sử dụng ends_with() kết hợp contains():
flights  %>% 
  select(ends_with(c("_time", "_delay")), -contains(c("sched", "air")))
# 3. Một cách khác, đó là lấy theo cụm khu vực và dùng contains() để loại bỏ đi các cột không cần thiết:
flights  %>% 
  select(dep_time:arr_delay, -contains("sched"))

# Bài tập 8: Hàm any_of() làm gì? Tại sao nó có thể hữu ích khi kết hợp với vectơ này?
variables <- c("year", "month", "day", "dep_delay", "arr_delay")
# Hàm any_of() được dùng để lựa chọn bất kỳ tên biến nào có trong một vector chỉ định, miễn là tên biến đó xuất hiện trong dữ liệu.
flights  %>% 
  select(any_of(variables))

# Ví dụ khác:
variables_2 <- c("year", "month", "day", "dep_delay", "arr_delay", "student")
flights  %>% 
  select(any_of(variables_2))

# Bài tập 9: Tại sao đoạn chương trình sau không hoạt động? Lỗi được báo có ý nghĩa gì?
flights  %>%  select(tailnum)  %>% 
  arrange(arr_delay)
# Đoạn chương trình này không hoạt động. Lỗi được báo có ý nghĩa là biến arr_delay không được tìm thấy
# trong bộ dữ liệu được lựa chọn trước đó. Bởi lẽ hàm select() chỉ lựa chọn một biến tailnum.

# Bài tập 10: Đổi tên cột air_time thành air_time_min để chỉ rõ đơn vị đo, đồng thời, di chuyển cột này
# về vị trí bắt đầu của bảng dữ liệu.
flights  %>% 
  rename(air_time_min = air_time)  %>% 
  relocate(air_time_min)

