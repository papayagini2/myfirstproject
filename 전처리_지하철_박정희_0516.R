# 1. 데이터 읽기 및 전처리
# 1-1. subway.csv(main), subway_latlong.csv(sub) 읽어와서 구조 확인
library(dplyr); library(lubridate); library(data.table); library(ggplot2) 
library(reshape2); library(plyr); library(funModeling)
rm(list=ls())
setwd("D:/ADP실기/5월빅분기실기/전처리_지하철_0516")
subway <- fread("subway.csv", stringsAsFactors = TRUE, encoding="UTF-8")

# 1-2. income_date를 date타입으로 변경하고 다른 컬럼들 타입 정리 
subway$station <- subway$station %>%  as.factor()
subway$income_date <-  subway$income_date %>%  ymd()
tmp <- subset(subway, select = on_tot:off_24) %>% mutate_if(is.factor, as.numeric) 
subway <- bind_cols(subway[ , 1:3], tmp)

# 1-3. 7월까지 밖에 없는 income_date 2014년도 데이터 제외하기
subway$income_date %>% range()
subway <- subway %>%  filter(income_date < "2014-01-1") 
subway$income_date %>% range()

# 1-4. 다른 호선의 같은 역명(stat_name뒤에 괄호)을 하나의 역명으로 처리 ex) 천호(5),천호(8) -> 천호
subway$stat_name <-  subway$stat_name %>%  as.character()
subway$stat_name %>% tail()

gsub("[:(:][0-9][:):]", "", "모란(8)")
subway$stat_name <-  gsub("[:(:][0-9][:):]", "", subway$stat_name)
subway$stat_name <- subway$stat_name %>%  as.factor()

# 1-5. income_date에서 추출한 연,월 컬럼 추가

subway$year  <- subway$income_date %>% year()
subway$month <- subway$income_date %>% month()

# 2. 탑승객 상위 5위 역 구하고 해당 탑승객수 출력 및 호선 정보 출력
# 2-1. stat_name 기준으로 탑승객(on_tot) 상위 5개 출력
aggregate(on_tot ~ stat_name, subway, sum)  %>%  arrange(desc(on_tot)) %>%  head(5) 

# 2-2. stat_name 기준으로 left join으로 sub파일 내 STATION_NM과 조인해서 역 호선 정보(LINE_NUM) 출력
subway_latlong <- fread("subway_latlong.csv", stringsAsFactors = TRUE)

subway %>%  dim() # 220110     47
subway_latlong %>%  dim() #  539   9

subway_merge <- merge(subway, subway_latlong, by.x = "stat_name" , by.y = "STATION_NM", all.x=TRUE) 
subway_merge  %>% dim() # 220110     55
subway_merge %>%  colnames()

# 2-3. 노선별로 정렬하기(LINE_NUM)
subway_merge <- subway_merge %>% janitor::clean_names()
subway_merge %>%  group_by(line_num) %>% arrange(line_num) %>% head() 

# 2-4. 역이름 factor타입으로 변경
subway_merge$stat_name %>%  class() 

# 3. 탑승객 수 상위 10개 역 구하고, x축:역(stat_name)별 y축:탑승객 수(on_tot) 막대그래프 그리기 (노선별로는 색으로 구분)

top10 <- aggregate(on_tot ~ stat_name + line_num, subway_merge, sum) %>% 
  arrange(desc(on_tot)) %>% head(10) 
ggplot(top10) + aes(x = stat_name, y = on_tot, fill = line_num) + 
    geom_bar(stat="identity") + scale_fill_hue() + theme_minimal()

# 4. 탑승객 상위 10개역의 2013년도 월별 역별 승객 수 구하고 추이도 그래프 그리기. (x=월(month),y=탑승객(on_tot), line=역(stat_name) )

tmp <- filter(subway_merge, stat_name %in% top10$stat_name ) %>%  filter(year=="2013") 
tmp <- aggregate(on_tot ~ stat_name + month, tmp, sum) 
tmp %>%  head()
tmp$month  <-  tmp$month  %>%  as.factor()

ggplot(tmp) + aes(x=month,y=on_tot, colour=stat_name, group=stat_name) +
  geom_line() + geom_point()

# 
# 5. 노선별 평균 지하철 탑승객 수 구하고 파이차트 그리기
# 
tmp2 <- aggregate(on_tot ~ line_num, subway_merge, mean)  

ggplot(tmp2) + aes(x=line_num, y=on_tot) + geom_bar(stat = "identity")
ggplot(tmp2) + aes(x="", y=on_tot, fill=line_num) + geom_bar(stat = "identity")
ggplot(tmp2) + aes(x="", y=on_tot, fill=line_num) + geom_bar(stat = "identity") + coord_polar("y", start = 0 )

# 6. 노선별 누적 승객 수의 상대 비교하고 영역차트 그리기 (x축 YYYY-MM 년월, y축 누적승객수, fill=호선)
# 
subway_merge$line_num <- paste0(subway_merge$line_num, "호선") 
subway_merge$line_num <-  subway_merge$line_num %>%  as.factor() 

subway_merge <- mutate(subway_merge, yearmon = paste(year,month, sep="-" ))

subway_merge %>%  is.na() %>%  sum()
subway_merge <- subway_merge[complete.cases(subway_merge),]
subway_merge %>%  is.na() %>%  sum()

aggregate(on_tot ~ line_num + yearmon, subway_merge, sum) %>% 
  ggplot(.) + aes(x=yearmon, y = on_tot, fill = line_num, group = line_num) + geom_area()

# 7. 시간대별 호선별 평균 탑승객(on_HH컬럼) 수의 상대 비교하고 추이도 그래프 그리기 (x축:탑승시간대(00~24), y축:탑승객수, group=호선)
# (**시간대on_HH컬럼 pivot 필요)
subway_merge %>% select(line_num, on_05:on_24) %>%  melt(., id.vars = "line_num") %>%  
  dcast(., line_num ~ variable, sum) %>%  melt(., id.vars = "line_num") %>% 
  ggplot(.) + aes(x = variable, y= value, color = line_num, group = line_num ) + geom_line() + geom_point()

# 8. USArrests.csv 파일 읽어서 Feature Scaling
# 8-1. Murder, Assault 변수를 z 표준화 후 히스토그램 그리기
USArrests <- fread("USArrests.csv", stringsAsFactors = TRUE)

USArrests %>% plot_num()
USArrests %>%  str()

USArrests2 <- USArrests
USArrests2$Murder  <-  scale(USArrests2$Murder, center=TRUE, scale=TRUE)
USArrests2$Assault <-  scale(USArrests2$Assault, center=TRUE, scale=TRUE)

par(mfrow=c(2,2))
USArrests$Murder   %>%  hist()
USArrests$Assault  %>%  hist()
USArrests2$Murder  %>%  hist()
USArrests2$Assault %>%  hist()

# 8-2. Murder 변수를 min max 정규화 후 히스토그램 그리기
USArrests3 <- USArrests
normalize <- function(x){
  return( (x-min(x))/(max(x)-min(x)))
}
USArrests3$Murder <-  normalize(USArrests3$Murder)
par(mfrow=c(1,2))
USArrests$Murder   %>%  hist()
USArrests3$Murder  %>%  hist()

