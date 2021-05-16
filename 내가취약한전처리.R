# 각 factor 별 평균 구하기 
stats::aggregate(Sepal.Length ~ Species, iris, mean) 
base::tapply(iris$Sepal.Length, iris$Species, mean) 
plyr::ddply(iris, .(Species), function(sub){
  data.frame(Sepal.width.meann=mean(sub$Sepal.Width))  })
doBy::summaryBy(Sepal.Width + Sepal.Length ~ Species, iris)


# 각 선수별 통산 평균 게임수 
aggregate(g ~ id, baseball, mean)  %>%  head()
ddply(baseball, .(id), function(sub){
                  mean(sub$g)   }) %>%  head()
# 각 선수별 데뷔년도 
aggregate(year ~ id, baseball, min) %>%  head() 
ddply(baseball, .(id), function(sub){
                 min(sub$year)  }) %>%  head()

# 각 선수별 해당 기록의 연차열 새로 추가 
ddply(baseball, .(id), transform, cyear = year - min(year) + 1)                      %>% head()
ddply(baseball, .(id), mutate,    cyear = year - min(year) + 1, log_cyear=log(cyear))%>% head()

# 각 선수별 데뷔년도를 새로운 열로 만들기 
ddply(baseball, .(id), summarise, minyear = min(year)) %>%  head()

# 각 선수별 데뷔년도, 마지막해 연도 열로 만들기 
ddply(baseball, .(id), summarise, minyear = min(year), maxyear=max(year)) %>%  head()

# 각 선수별 연도별 최대 게임 출장 기록 
ddply(baseball, .(id), subset , g==max(g) )  %>%  head()

# bar 그래프 그리기
# x축만 지정하기 - 기본이 stat = count 임 
ggplot(diamonds) + aes(x=cut) + geom_bar()
ggplot(diamonds) + aes(x=cut, y = price) + geom_bar() # 이러면 에러가 발생 ! 
ggplot(diamonds) + aes(x=cut, y = price) + geom_bar(stat = "identity")
ggplot(diamonds) + aes(x=cut, y = price, fill = color) + geom_bar(stat = "identity")
ggplot(diamonds) + aes(x=cut, y = price, fill = color) + geom_bar(stat = "identity", position = "dodge")
ggplot(diamonds) + aes(x=cut, y = price, fill = color) + geom_bar(stat = "identity", position = "fill")
ggplot(diamonds) + aes(x=cut, y = price, fill = color) + geom_bar(stat = "identity", position = "dodge") + coord_flip()

