library(dplyr); library(data.table); library(tibble); library(skimr); library(reshape2); library(ggplot2)
library(funModeling); library(caret); library(recipes); library(rpart); library(randomForest)

rm(list=ls())
setwd("D:/ADP실기/5월빅분기실기/수입분류_0516")
train_set <- fread("train_set.csv", stringsAsFactors = TRUE)
test_set  <- fread("test_set.csv" , stringsAsFactors = TRUE)
test_Y    <- fread("test_Y.csv"   , stringsAsFactors = TRUE)

train_set %>%  dim()
test_set  %>%  dim()
test_Y    %>%  dim()



bind_rows(
  train_set                     %>% add_column(index = "training"), 
  bind_cols(test_set, test_Y)   %>% add_column(index = "testing")
)   -> alldata
alldata$index <- alldata$index %>% as.factor()
alldata <- alldata %>% janitor::clean_names()

alldata %>%  skim()

alldata %>%  dplyr::select(-c("index", "income")) %>%  sapply(., is.factor)  %>%  which() -> factors
alldata %>%                                            sapply(., is.numeric) %>%  which() -> numbers
alldata <- subset(alldata, select = c(index, income, factors, numbers ))

# 변수 순서를 바꾼 다음에 which를 다시 추출해 주어야 함 !! 
alldata %>%  dplyr::select(-c("index", "income")) %>%  sapply(., is.factor)  %>%  which() -> factors
alldata %>%                                     sapply(., is.numeric) %>%  which() -> numbers
alldata %>%  colnames()

# 범주형변수들의영향도 탐색 
summary(income ~ ., data = alldata, method = "reverse")
alldata %>%  colnames()
# mosaicplot(income ~ workclass + education +marital_status + occupation + relationship + race + sex + native_country, data = alldata2 )

mosaicplot(income ~ workclass, data = alldata )
xtabs( ~ income + workclass, data = alldata )

xtabs( ~ income + education, data = alldata )

mosaicplot(income ~ sex, data = alldata )
xtabs( ~ income + sex, data = alldata ) %>% prop.table() %>%  round(.,2)

# 수치형변수들 영향도 탐색 
alldata %>% subset(., select = c(income, numbers)) %>%  
  melt(., id.vars = c("income")) %>% 
  ggplot() +  aes(x = income, y = value) +
  geom_boxplot() + facet_wrap(vars(variable), scales = "free_y")

# 수치형 스케일링 
alldata %>%  plot_num()
income_recipe <- recipe(income ~ ., data = alldata) %>% 
  step_normalize( all_numeric(), -all_outcomes()) 
income_recipe <- prep(income_recipe, training = alldata  )
alldata2 <- bake(income_recipe, new_data = alldata )
alldata2 %>%  plot_num()

# train / test 분할 - 원래 가지고 있는 데이터 셋으로 다시 분할 
train <- alldata2 %>%  filter(index == "training")  %>%  select(-c("index"))
test  <- alldata2 %>%  filter(index == "testing")   %>%  select(-c("index"))

train %>% dim()
test %>% dim()




# 1. train_set 데이터를 이용하여 'income' 상태를 분류하는 의사결정나무를 만든 뒤, 모델의 비용복잡도에 기준해 가지치기(Pruning) 과정을 수행한 최종 모델을 만드시오.
set.seed(2021)
m_rpart <-  rpart(income ~ . , data = train); m_rpart 
m_rpart$cptable
m_rpart_prune <-  prune(m_rpart, cp = m_rpart$cptable[4,1])
m_rpart_prune %>%  summary()
m_rpart_prune$variable.importance
pred_rpart_prune <- predict(m_rpart_prune, newdata = test, type = "class")
confusionMatrix(pred_rpart_prune, test$income) # Accuracy : 0.8458 


trainControl(method="repeatedcv", number = 10 , repeats = 10) -> fitControl
set.seed(123)
train(income ~ . , data = train, method = "rpart", trControl = fitControl) -> dt_fit
dt_fit # CP = 0.03650566 일 때,  Accuracy  0.8381258 일 때 최고치  

pred_dt <- predict(dt_fit, newdata = test, type = "raw")
confusionMatrix(pred_dt, test$income) # Accuracy : 0.8365 

# 2. 랜덤포레스트를 만들고 변수 중요도(feature importance)를 bar graph로 시각화하시오. 포레스트를 만들 때 다음 조건을 참고하시오. 단, AUC 그래프/AUROC를 참고하여 random forest의 트리 개수, 각 트리의 깊이, 노드의 split을 결정하는 최소 크기, 하위 leaf가 만들어지는 최소한의 샘플 데이터 수(min_sample_split)를 튜닝하는 과정을 포함할 것.
# 3. 가장 성능이 좋은 모델을 이용하여 test_set 데이터의 income 을 분류하고 csv 파일로 제출할 것 (제출명 : 이름_0516모델링.csv)
# 
# * validation set의 크기는 자유롭게 결정
# * 최종 모델을 pipeline 형태로 구현하여, test set 분류 시 별도의 전처리 과정 없이 함수 하나로 이루어질 수 있도록 코딩
# * 코드는 pdf파일, 혹은 html 파일로 업로드 (제출명 : 이름_0516모델링코드)

set.seed(2022)
m_rf <- randomForest(income ~ ., data = train, importance = TRUE)
m_rf %>%  importance()
m_rf %>%  varImpPlot()
pred_rf <- predict(m_rf, newdata = test, type = "class")
confusionMatrix(pred_rf, test$income) # Accuracy : 0.869

result <- cbind(test_Y, pred_rpart_prune, pred_dt, pred_rf)
write.csv(result, file="D:/ADP실기/5월빅분기실기/수입분류_0516/박정희_result.csv")


