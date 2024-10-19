library(dplyr)
library(nycflights13)
data(flights)
flights
glimpse(flights)

# Sử dụng tabyl() để tạo bảng tần số cho 1 biến định tính trong bảng dữ liệu. Nhận xét được gì từ
# bảng kết quả.
library(janitor)
flights %>%
  tabyl(carrier) %>%
  arrange(desc(n))
# Nhận xét:
# "UA" có số chuyến bay lớn nhất với 58665 chuyến bay, chiếm khoảng 17.4% tổng số chuyến bay trong dữ liệu. Điều này cho thấy UA là hãng hàng không chiếm ưu thế nhất.
# Các hãng lớn thứ 2 và 3:"B6" và "EV" lần lượt có 54635 và 54173 chuyến bay, chiếm khoảng 16.2% và 16.1% tổng số chuyến bay.
# Hãng hàng không có số lượng chuyến bay ít nhất là "OO" chỉ có 32 chuyến bay (0.0001%) -> hãng hàng không này có sự hiện diện rất ít trong bộ dữ liệu. 


# Sử dụng tabyl() để tạo bảng tần số cho 2 biến định tính trong bảng dữ liệu. Nhận xét được gì từ
# bảng kết quả.
flights %>%
  tabyl(carrier, origin)
# Nhận xét: 
# "UA" chỉ hoạt động tại EWR với 46087 chuyến bay(~79% số chuyến bay của hãng), cho thấy UA tập trung hoạt động tại sân bay này.
# "B6" có 42076 chuyến bay xuất phát từ JFK.
# Hầu hết các chuyến bay của EV xuất phát từ EWR (43939 chuyến bay) với một số ít chuyến bay từ JFK (1408 chuyến bay) và LGA (8826 chuyến bay).
# Một số hãng hàng không như YV, HA, FL, F9 và AS chỉ hoạt động tại một sân bay duy nhất


# Tạo ra bảng tỷ số chéo của 2 biến định tính, với tỷ số tính theo dòng.
flights %>%
  tabyl(carrier, origin) %>%
  adorn_percentages("row")