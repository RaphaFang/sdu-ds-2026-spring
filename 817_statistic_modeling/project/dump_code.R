
# ** 初步使用LASSO
# 這邊要作一次包含 bmi 但是沒人被踢掉。
# however, this shouldn't happened, since we can definitely sure bmi is multicollinearity
# 於是下面剔除 bmi 但是更怪了 M/F 都怪
# 這邊要論證，F 這邊增加一公分身高，體脂肪增加 181.07371515 %

# ==============================================================================
df_with_bg_variable_M <- df %>% filter(sex == "M") %>% select(-(c("sex", "study", "bmi")))
x <- model.matrix(fat_percent ~ ., data = df_with_bg_variable_M)[, -1] 
y <- df_with_bg_variable_M$fat_percent

lasso_model <- cv.glmnet(x, y, alpha = 1)
coef(lasso_model, s = "lambda.min")

# 17 x 1 sparse Matrix of class "dgCMatrix"
#               lambda.min
# (Intercept) -5.454104725
# weight      -0.004484128
# height      -6.190508712
# bmi          .          
# age          0.060405441
# abdomen      0.693471258
# ankle        .          
# biceps       .          
# calf         .          
# chest        .          
# elbow        .          
# forearm      0.231254193
# hip         -0.009026770
# knee         .          
# neck        -0.260113124
# thigh        .          
# wrist       -1.472578300


df_with_bg_variable_F <- df %>% filter(sex == "F") %>% select(-(c("sex", "study", "bmi")))
x <- model.matrix(fat_percent ~ ., data = df_with_bg_variable_F)[, -1] 
y <- df_with_bg_variable_F$fat_percent

lasso_model <- cv.glmnet(x, y, alpha = 1)
coef(lasso_model, s = "lambda.min")
# 16 x 1 sparse Matrix of class "dgCMatrix"
#                lambda.min
# (Intercept) -401.46755383
# weight        -2.71334239
# height       181.07371515
# age            1.29729600
# abdomen        0.07934044
# ankle         -0.20800559
# biceps        -4.94046570
# calf          -0.02888173
# chest          2.25738680
# elbow          0.14716864
# forearm       -0.15748825
# hip           -4.97024839
# knee          -7.93800592
# neck          -0.22740700
# thigh         16.79720467
# wrist          0.01827661

# ==============================================================================
# ** 下面做了兩個改變，一個是增加了剔除的col，一個是增加到剔除 1se
# 但是F 還是很頑固，沒有東西被踢掉

df_with_bg_variable_M_pure <- df %>% filter(sex == "M") %>% select(-(c("sex", "study", "weight", "height", "bmi")))
x <- model.matrix(fat_percent ~ ., data = df_with_bg_variable_M_pure)[, -1] 
y <- df_with_bg_variable_M_pure$fat_percent

lasso_model <- cv.glmnet(x, y, alpha = 1)
coef(lasso_model, s = "lambda.1se")
# 14 x 1 sparse Matrix of class "dgCMatrix"
#               lambda.1se
# (Intercept) -16.38955933
# age           0.05688074
# abdomen       0.64707284
# ankle         .         
# biceps        .         
# calf          .         
# chest         .         
# elbow         .         
# forearm       .         
# hip           .         
# knee          .         
# neck         -0.05072047
# thigh         .         
# wrist        -1.37590129

df_with_bg_variable_F_pure <- df %>% filter(sex == "F") %>% select(-(c("sex", "study", "weight", "height", "bmi")))
x <- model.matrix(fat_percent ~ ., data = df_with_bg_variable_F_pure)[, -1] 
y <- df_with_bg_variable_F_pure$fat_percent

lasso_model <- cv.glmnet(x, y, alpha = 1)
coef(lasso_model, s = "lambda.1se")
# 14 x 1 sparse Matrix of class "dgCMatrix"
#               lambda.1se
# (Intercept) -25.94129773
# age           0.05433343
# abdomen       0.17345232
# ankle        -0.49010072
# biceps       -0.29752833
# calf         -0.03402037
# chest         0.18034981
# elbow         .         
# forearm      -0.16761921
# hip          -0.39266951
# knee         -0.60745006
# neck         -0.12244962
# thigh         2.03422185
# wrist        -0.70977089