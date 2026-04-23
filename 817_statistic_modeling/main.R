setwd('/Users/fangsiyu/Desktop/sdu-2026-code/817_statistic_modeling')

data <- readRDS("data/real_diabetes.RDS")
summary(data)

data_c = data[, -c(15,16)]
inds = rowSums(is.na(data_c)) == 0 
data_cc = data_c[inds,]

dim(data_cc) # 366  17

plot(data_cc)
summary(data_cc)

model_lm <- lm(glyhb ~ ., data = data_cc)
summary(model_lm)

model_glm <- glm(glyhb ~ ., 
                 data = data_cc,
                 family = binomial)
summary(model_glm)

head(data_cc)
head(data_cc$glyhb)

plot(data$stab.glu, data$glyhb)


data_cc[["family"]]

# 這邊的問題是我不能相信我的 p value 但是我還是可以作點什麼
