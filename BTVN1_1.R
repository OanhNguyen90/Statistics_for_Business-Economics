setwd("C:/Users/dieuo/OneDrive/Tài liệu/Storage/UEH/Thống kê ứng dụng trong kinh tế");

#Bảng dữ liệu gốc
BTVN <- read.delim("BTVN 1.txt");

#Bảng dữ liệu tách thành 2 bảng dựa trên nhà sản xuất - Định tính
chia.bảng.theo.nsx <- data.frame(split(BTVN, f=BTVN$Nhà.sản.xuầt));
data.1 <- subset.data.frame(BTVN, Nhà.sản.xuầt==1);
data.2 <- subset.data.frame(BTVN, Nhà.sản.xuầt==2);

#Bảng phân phối tần xuất theo hai nhà sản xuất - Định tính
library(dplyr);
Tần.xuất.theo.nsx <- count(BTVN, Nhà.sản.xuầt, sort=TRUE, name="Tần suất");

#data.1 - Bảng phân phối theo nhóm của nhà sản xuất A
Độ.rộng.nhóm.1 <- (max(data.1$Trọng.lượng)- min(data.1$Trọng.lượng))/5;
nhóm.Trọng.lượng.1 <- cut(data.1$Trọng.lượng, seq(22,38,3), lowestclass=22, 
              upperestclass=35, 
              include.lowest = TRUE);
phân.phối.Định.lượng.data.1 <- data.frame(table(nhóm.Trọng.lượng.1));

#data.2 - Bảng phân phối theo nhóm của nhà sản xuất B
Độ.rộng.nhóm.2 <- (max(data.2$Trọng.lượng)- min(data.2$Trọng.lượng))/5;
nhóm.Trọng.lượng.2 <- cut(data.2$Trọng.lượng, seq(22,38,3), lowestclass=22, 
                        upperestclass=35, 
                        include.lowest = TRUE);
phân.phối.Định.lượng.data.2 <- data.frame(table(nhóm.Trọng.lượng.2));

#Biểu đồ phân phối (Histogram) nhà sản xuất A
hist.1 = barplot(table(nhóm.Trọng.lượng.1), space=0, 
                 main="Biểu đồ phân phối nhà sản xuất A", col = blues9, 
                 xlab="Trọng lượng", ylab="Tần xuất");

#Biểu đồ phân phối (Histogram) nhà sản xuất B
hist.2 = barplot(table(nhóm.Trọng.lượng.2), space=0, 
                 main="Biểu đồ phân phối nhà sản xuất B", col = blues9, 
                 xlab="Trọng lượng", ylab="Tần xuất");

#Tính trung bình của file gốc
cal.1 <- mean(BTVN$Trọng.lượng);

#Tính trung vị của file gốc
cal.2 <- median(BTVN$Trọng.lượng);

#Tạo function() để tính số mode
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  mode_index <- which(tab == max(tab))[1]
  u[mode_index]
};

#Tính mốt của file gốc
cal.3 <- find_mode(BTVN$Trọng.lượng);

#Tính độ lệch chuẩn của file gốc
cal.4 <- sd(BTVN$Trọng.lượng);
#Tính hệ số biến thiên về trọng lượng của file gốc
cal.5 <- (sd(BTVN$Trọng.lượng)/mean(BTVN$Trọng.lượng))*100

#Tạo biểu đồ nhánh lá cho trọng lượng sản xuất A
stem.data.1 <- c(data.1$Trọng.lượng);
stem(stem.data.1,atom=20);

#Tạo biểu đồ nhánh lá cho trọng lượng sản xuất B
stem.data.2 <- c(data.2$Trọng.lượng);
stem(stem.data.2,atom=20)