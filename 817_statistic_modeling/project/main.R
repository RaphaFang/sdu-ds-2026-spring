# ==============================================================================
# STEP 0: load data, and inquire
# ==============================================================================
library(car)
library(glmnet)
library(dplyr)
setwd('/Users/fangsiyu/Desktop/sdu-2026-code/817_statistic_modeling/project')
df <- read.csv("data/examdata.csv")
df$height <- df$height * 100
# I like to try this step and see the calculation difference

# table(df$study, df$sex) # inspect study & sex cols of df, which i want to make sure the relation of it


# ==============================================================================
# STEP 1.1: Try PCA, figure out should I split the data base on sex
#           PCA shows two clear clusters
# ==============================================================================
pca_res <- prcomp(df[, -c(1, 2)], center = TRUE, scale. = TRUE)
pc1 <- pca_res$x[, 1]
pc2 <- pca_res$x[, 2]

point_colors <- ifelse(df$sex == "F", "red", "blue") # we have to label the color here, sth similar to py if else syntax
plot(pc1, pc2, 
     col = point_colors,
     pch = 16,
     xlab = "PC1", 
     ylab = "PC2", 
     main = "PCA, by Sex")
legend("topright",
       legend = c("F", "M"),
       col = c("red", "blue"),
       pch = 16)

summary(pca_res)
#                           PC1    PC2 
# Proportion of Variance 0.6891 0.1210
# this shows the cluster separation in PCA is meaningful


# ==============================================================================
# STEP 1.2: Try t-test on fat_percent and sex
#           p-value = 5.364e-05
# ==============================================================================
t.test(fat_percent ~ sex, data = df)

# ==============================================================================
# STEP 1.3: try cor() see the diff between sex
# ==============================================================================
target_col <- c('sex','fat_percent','abdomen', 'ankle', 'biceps', 'calf', 'chest', 'elbow', 'forearm', 'hip', 'knee', 'neck', 'thigh', 'wrist')
df_filtered <- df %>% select(target_col)

df_M <- df_filtered %>% filter(sex == "M") %>% select(-sex)
df_F <- df_filtered %>% filter(sex == "F") %>% select(-sex)

cor_M <- cor(df_M)[, "fat_percent"]
cor_F <- cor(df_F)[, "fat_percent"]
cor_both <- abs(cor_M - cor_F) # this will show the difference cor base on sex

sort(cor_M, decreasing = TRUE) %>% head(6)
sort(cor_F, decreasing = TRUE) %>% head(6)
sort(cor_both, decreasing = TRUE) %>% head(6)

# fat_percent     abdomen       chest         hip       thigh       elbow 
#   1.0000000   0.8136159   0.7027925   0.6254808   0.5604496   0.5598001 
# fat_percent       thigh     abdomen         hip      biceps        knee 
#   1.0000000   0.7968302   0.7400749   0.7147579   0.6702480   0.6279577 

#     chest     thigh   forearm    biceps     ankle      calf 
# 0.2644831 0.2363806 0.2235879 0.1770609 0.1644393 0.1609260

model_M <- lm(fat_percent ~ abdomen + chest + hip, data = df_M)
summary(model_M)
# Multiple R-squared:  0.6995,    Adjusted R-squared:  0.6959 

# ==============================================================================
# STEP 2.1: LASSO, we have to fix the Multicollinearity, and place this process before AIC
# ** vif + lasso + vif, i almost rebuild all the stuff......
# ==============================================================================
df_with_bg_variable_M <- df %>% filter(sex == "M") %>% select(-(c("sex", "weight", "elbow", "calf", "study", "bmi")))
full_model_M <- lm(fat_percent ~ ., data = df_with_bg_variable_M)
vif(full_model_M)
#    height       age   abdomen     ankle    biceps     chest   forearm       hip      knee      neck     thigh     wrist 
#  1.323890  2.150579 11.284686  1.843532  3.507065  7.885762  2.192033 10.468714  4.305919  3.956481  7.749802  3.305003 
#  But I decide to keep the abdomen
x <- model.matrix(fat_percent ~ ., data = df_with_bg_variable_M)[, -1] 
y <- df_with_bg_variable_M$fat_percent
lasso_model <- cv.glmnet(x, y, alpha = 1)
coef(lasso_model, s = "lambda.1se")
# 13 x 1 sparse Matrix of class "dgCMatrix"
#               lambda.1se
# (Intercept) -13.90246930
# height       -0.05959010
# age           0.03630252
# abdomen       0.60273735
# ankle         .         
# biceps        .         
# chest         .         
# forearm       .         
# hip           .         
# knee          .         
# neck          .         
# thigh         .         
# wrist        -0.75966433

df_final_check_M <- df %>% filter(sex == "M") %>% select(c("fat_percent", "height", "abdomen", "age","wrist"))
check_model_M <- lm(fat_percent ~ ., data = df_final_check_M)
vif(check_model_M)
#   height  abdomen      age    wrist 
# 1.211642 1.674420 1.133065 1.891557 


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
df_with_bg_variable_F <- df %>% filter(sex == "F") %>% select(-(c("sex", "weight", "study", "bmi", "thigh")))
full_model_F <- lm(fat_percent ~ ., data = df_with_bg_variable_F)
vif(full_model_F)
#   height      age  abdomen    ankle   biceps     calf    chest    elbow  forearm      hip     knee     neck    wrist 
# 1.324884 1.084810 3.721331 2.636363 4.450099 1.657814 1.960119 3.122618 6.374919 4.436765 2.631282 2.517028 3.776540 

x <- model.matrix(fat_percent ~ ., data = df_with_bg_variable_F)[, -1] 
y <- df_with_bg_variable_F$fat_percent
lasso_model <- cv.glmnet(x, y, alpha = 1)
coef(lasso_model, s = "lambda.1se")
# 14 x 1 sparse Matrix of class "dgCMatrix"
#              lambda.1se
# (Intercept) -28.2985410
# height       -0.0486490
# age           .        
# abdomen       0.3029398
# ankle         .        
# biceps        0.2275421
# calf          .        
# chest         .        
# elbow         .        
# forearm       .        
# hip           0.2214825
# knee          0.2686598
# neck          .        
# wrist         .          

df_final_check_F <- df %>% filter(sex == "F") %>% select(c("fat_percent", "height", "abdomen", "biceps", "hip", "knee"))
check_model_F <- lm(fat_percent ~ ., data = df_final_check_F)
vif(check_model_F)
#   height  abdomen   biceps      hip     knee 
# 1.225051 3.134297 2.753503 3.910855 2.293491 

# ==============================================================================
# STEP 2.2: try AIC,
# ==============================================================================

full_model_M <- lm(fat_percent ~ ., data = df_final_check_M)
step_model_M <- step(full_model_M, direction = "both")
summary(step_model_M)
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.08450    6.51707  -0.013  0.98967    
# height      -0.06911    0.03185  -2.170  0.03095 *  
# abdomen      0.70271    0.03239  21.696  < 2e-16 ***
# age          0.07030    0.02280   3.084  0.00228 ** 
# wrist       -2.01586    0.39761  -5.070 7.81e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 4.276 on 247 degrees of freedom
# Multiple R-squared:  0.723,     Adjusted R-squared:  0.7185 
# F-statistic: 161.2 on 4 and 247 DF,  p-value: < 2.2e-16

full_model_F <- lm(fat_percent ~ ., data = df_final_check_F)
step_model_F <- step(full_model_F, direction = "both")
summary(step_model_F)
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -17.01545    7.61681  -2.234   0.0267 *  
# height       -0.20488    0.04708  -4.352 2.26e-05 ***
# abdomen       0.35702    0.06303   5.664 5.81e-08 ***
# hip           0.34041    0.07960   4.277 3.08e-05 ***
# knee          0.42419    0.16781   2.528   0.0123 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 3.475 on 179 degrees of freedom
# Multiple R-squared:  0.6503,    Adjusted R-squared:  0.6425 
# F-statistic: 83.22 on 4 and 179 DF,  p-value: < 2.2e-16

# ==============================================================================
# STEP 2.3: try interaction, but have anova to evaluate the need for extra complicated
# ==============================================================================

model_simple_M <- lm(fat_percent ~ height + abdomen + age + wrist, data = df_final_check_M)
model_interaction <- lm(fat_percent ~ height + abdomen + age + wrist + (height * abdomen), data = df_final_check_M)
anova(model_simple_M, model_interaction)
# Model 1: fat_percent ~ height + abdomen + age + wrist
# Model 2: fat_percent ~ height + abdomen + age + wrist + (height * abdomen)
#   Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1    247 4516.2                           
# 2    246 4486.1  1    30.039 1.6472 0.2005

model_simple_F <- lm(fat_percent ~ height + abdomen + hip + knee, data = df_final_check_F)
model_interaction <- lm(fat_percent ~ height + abdomen + hip + knee + (height * hip), data = df_final_check_F)
anova(model_simple_F, model_interaction)
# Model 1: fat_percent ~ height + abdomen + hip + knee
# Model 2: fat_percent ~ height + abdomen + hip + knee + (height * hip)
#   Res.Df    RSS Df Sum of Sq     F Pr(>F)
# 1    179 2161.9                          
# 2    178 2144.6  1    17.361 1.441 0.2316


# ==============================================================================
# STEP 3.1: Regression Diagnostics
# ==============================================================================
library(ggplot2)
library(dplyr)

# try robust regression
library(dplyr)
library(MASS)
model_rlm_with_39 <- rlm(fat_percent ~ height + abdomen + age + wrist, data = df_final_check_M[-c(42), ])
model_lm_39 <- lm(fat_percent ~ height + abdomen + age + wrist, data = df_final_check_M[-c(42), ])
summary(model_rlm_with_39)
cbind(OLS = coef(model_lm_39), Robust = coef(model_rlm_with_39))
model_rlm_with_39$w["39"]

par(mfrow = c(2, 2))
plot(model_rlm_with_39)
par(mfrow = c(1, 1))



png("plot/diagnostic_plots_M_before_adjustment.png", width = 1200, height = 1200, res = 120)
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0)) 
plot(model_simple_M, sub.caption = "Regression Diagnostics: Male Model - Before Adjustment")
dev.off()

model_simple_M_no_39_42 <- lm(fat_percent ~ height + abdomen + age + wrist, data = df_final_check_M[-c(39,42), ])
png("plot/diagnostic_plots_M_after_no_39_42.png", width = 1200, height = 1200, res = 120)
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0)) 
plot(model_simple_M_no_39_42, sub.caption = "Regression Diagnostics: Male Model - remove 39,42")
dev.off()

png("plot/diagnostic_plots_M_lm_after_keep_39_no_42.png", width = 1200, height = 1200, res = 120)
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0)) 
plot(model_lm_39, sub.caption = "Regression Diagnostics: Male Model - lm - keep_39_no_42")
dev.off()

png("plot/diagnostic_plots_M_rlm_after_keep_39_no_42.png", width = 1200, height = 1200, res = 120)
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0)) 
plot(model_rlm_with_39, sub.caption = "Regression Diagnostics: Male Model - rlm - keep_39_no_42")
dev.off()

png("plot/diagnostic_plots_F.png", width = 1200, height = 1200, res = 120)
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0)) 
plot(model_simple_F, sub.caption = "Regression Diagnostics: Female Model")
dev.off()

par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))



# ==============================================================================
# STEP 3.2: Sensitivity Analysis
# ==============================================================================
outlierTest(model_simple_M)
#     rstudent unadjusted p-value Bonferroni p
# 39 -4.342596         2.0594e-05    0.0051896

stu_res <- rstudent(model_simple_M)
outliers <- which(abs(stu_res) > 3)
print(outliers)
# 39 
# 39 

cooksd <- cooks.distance(model_simple_M)
plot(cooksd, type="h")
abline(h = 0.3, col="red", lty=2)

# r$> df_final_check_M[39, ]
#    fat_percent height abdomen age wrist
# 39        34.5    184   148.1  46  21.4

# r$> df_final_check_M[42, ]
#    fat_percent height abdomen age wrist
# 42        32.3     75   104.3  44  17.4
#     fat_percent height abdomen age wrist
# 182           0    173    69.4  40  16.5


outlierTest(model_simple_F)
# No Studentized residuals with Bonferroni p < 0.05
# Largest |rstudent|:
#      rstudent unadjusted p-value Bonferroni p
# 107 -3.003096          0.0030573      0.56254

stu_res <- rstudent(model_simple_F)
outliers <- which(abs(stu_res) > 3)
print(outliers)
# 107 
# 107 

cooksd <- cooks.distance(model_simple_F)
plot(cooksd, type="h", main="庫克距離 (影響力檢測)")
abline(h = 4/length(cooksd), col="red", lty=2)



model_1_ols_all <- lm(fat_percent ~ height + abdomen + age + wrist, data = df_final_check_M[-c(42), ])
model_2_robust <- rlm(fat_percent ~ height + abdomen + age + wrist, data = df_final_check_M[-c(42), ])
model_3_ols_clean <- lm(fat_percent ~ height + abdomen + age + wrist, data = df_final_check_M[-c(39,42), ])
model_3_ols_clean <- lm(fat_percent ~ height + abdomen + age + wrist, data = df_final_check_M[-c(39,42), ])

cbind(
  "1. with 39" = coef(model_1_ols_all),
  "2. with 39 rlm" = coef(model_2_robust),
  "3. no 39" = coef(model_3_ols_clean))

#                       1. with 39 2. with 39 rlm 3. no 39
# (Intercept)        6.11892734         6.24177912      3.75218257
# height            -0.11533985        -0.12673267     -0.12571297
# abdomen            0.70556656         0.72873414      0.74278610
# age                0.06179403         0.05660408      0.05255156
# wrist             -1.89588662        -1.89202774     -1.82628310


pct_change <- (coef(model_3_ols_clean) - coef(model_1_ols_all)) / coef(model_1_ols_all) * 100
round(pct_change, 2)
# (Intercept)      height     abdomen         age       wrist 
#      -38.68        8.99        5.28      -14.96       -3.67 

c("with 39 RSE" = sigma(model_1_ols_all),
  "no 39 RSE" = sigma(model_3_ols_clean))

c("with 39 Adj R2" = summary(model_1_ols_all)$adj.r.squared,
  "no 39 Adj R2" = summary(model_3_ols_clean)$adj.r.squared)
# with 39 RSE no 39 RSE 
#             4.269580             4.115877 
# with 39 Adj R2 no 39 Adj R2 
#         0.7174023         0.7344574 

confint(model_1_ols_all)["abdomen", ]
#     2.5 %    97.5 % 
# 0.6417259 0.7694072 
confint(model_3_ols_clean)["abdomen", ]
#     2.5 %    97.5 % 
# 0.6790663 0.8065059 



# dfbetas, a different way to detect the outlier, but im not sure how it works, thus not going to put it in report
influencePlot(model_1_ols_all, main="Comprehensive Influence Bubble Plot", id=TRUE)
dfb <- dfbetas(model_1_ols_all)
threshold <- 2 / sqrt(nrow(model_1_ols_all$model))
cat("the threshold will be, bigger than", threshold, "or smaller than", -threshold)
print(dfb["39", ])
#        StudRes        Hat      CookD
# 39  -4.4403079 0.11823623 0.49137024
# 41  -0.8279383 0.08382582 0.01255976
# 224 -2.4659401 0.01646171 0.01994345
# 250 -1.6694051 0.04625571 0.02683760
# the threshold will be, bigger than 0.1262389 or smaller than -0.1262389(Intercept)      height     abdomen         age       wrist 
#   0.3058731   0.2274868  -1.1912055   0.4052721  -0.1772796 


# ==============================================================================
# STEP 3.4: Bootstrapping
# ==============================================================================

set.seed(67) 

boot_results <- Boot(model_3_ols_clean, R = 10000)
summary(boot_results)
# Number of bootstrap replications R = 10000 
#              original    bootBias   bootSE   bootMed
# (Intercept)  3.752183 -1.8595e-02 8.156566  3.779099
# height      -0.125713 -1.2896e-04 0.048628 -0.126142
# abdomen      0.742786  1.4936e-04 0.030303  0.742575
# age          0.052552 -1.9872e-05 0.021420  0.052771
# wrist       -1.826283  2.0956e-03 0.380709 -1.821048
confint(boot_results, type = "perc")
#                    2.5 %      97.5 %
# (Intercept) -12.41631218 19.64830510
# height       -0.22060097 -0.03085507
# abdomen       0.68456929  0.80391719
# age           0.01000905  0.09419641
# wrist        -2.56401158 -1.08394744


boot_results_F <- Boot(model_simple_F, R = 10000)
summary(boot_results_F)
# Number of bootstrap replications R = 10000 
#              original    bootBias   bootSE   bootMed
# (Intercept)  3.752183 -1.8595e-02 8.156566  3.779099
# height      -0.125713 -1.2896e-04 0.048628 -0.126142
# abdomen      0.742786  1.4936e-04 0.030303  0.742575
# age          0.052552 -1.9872e-05 0.021420  0.052771
# wrist       -1.826283  2.0956e-03 0.380709 -1.821048
confint(boot_results_F, type = "perc")
#                    2.5 %      97.5 %
# (Intercept) -33.8287486 -0.2221347
# height       -0.2980716 -0.1059085
# abdomen       0.2513989  0.4707317
# hip           0.1748906  0.4851760
# knee          0.1251323  0.7462641

# ==============================================================================
# STEP 4.1 : prediction and result
# ==============================================================================
df_without_bf <- read.csv("data/examdata2.csv")
df_without_bf$height <- df_without_bf$height * 100

df_without_bf_M <- df_without_bf %>% filter(sex == "M") %>% select(-c("study", "sex"))
df_without_bf_F <- df_without_bf %>% filter(sex == "F") %>% select(-c("study", "sex"))


pred_4_points <- predict(model_3_ols_clean, newdata = df_without_bf_M)
#         1         2         3         4 
# 32.286768  5.521586 -3.015744 55.926913 

pred_2_points <- predict(model_simple_F, newdata = df_without_bf_F)
#        1        2 
# 15.05497 19.77243 

pred_combined <- bind_rows(
  df_without_bf_M %>% mutate(sex = "M", predicted = round(pred_4_points, 3)),
  df_without_bf_F %>% mutate(sex = "F", predicted = round(pred_2_points,3))
)

df_final_check_M[df_final_check_M$fat_percent < 10, ]
df_final_check_M[df_final_check_M$abdomen < 70, ]


# ==============================================================================
# STEP 4.2 : prediction plot
# ==============================================================================
png("plot/M_prediction_4plot.png", width = 1200, height = 1200, res = 120)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

vars <- c("abdomen", "height", "age", "wrist")
titles <- c("Male - Abdomen (cm)", "Male - Height (cm/m)", "Male - Age (years)", "Male - Wrist (cm)")
for (i in 1:4) {
  v <- vars[i]
  x_range <- range(c(df_final_check_M[[v]], df_without_bf_M[[v]]))
  y_range <- range(c(df_final_check_M$fat_percent, pred_4_points))
  
  plot(df_final_check_M[[v]], df_final_check_M$fat_percent,
       col = "grey75", pch = 16, xlim = x_range, ylim = y_range,
       main = paste("Diagnostic:", titles[i]),
       xlab = titles[i], ylab = "Fat Percent (%)")
  
  v_seq <- seq(x_range[1], x_range[2], length.out = 200)
  line_df <- data.frame(
    abdomen = mean(df_final_check_M$abdomen),
    height  = mean(df_final_check_M$height),
    age     = mean(df_final_check_M$age),
    wrist   = mean(df_final_check_M$wrist)
  )
  line_df <- line_df[rep(1, 200), ]
  line_df[[v]] <- v_seq
  line_df$pred <- predict(model_3_ols_clean, newdata = line_df)
  lines(v_seq, line_df$pred, col = "blue", lwd = 2)

  points(df_without_bf_M[[v]], pred_4_points, col = "red", pch = 16, cex = 1.2)
  text(df_without_bf_M[[v]], pred_4_points,
       labels = paste0("P", seq_along(pred_4_points), ": ", round(pred_4_points, 1), "%"),
       col = "red", pos = 3, font = 2, cex = 0.8)
}
dev.off()


# ==============================================================================
pred_2_points <- predict(model_simple_F, newdata = df_without_bf_F)
png("plot/F_prediction_4plot.png", width = 1200, height = 1200, res = 120)
par(mfrow = c(2, 2), mar = c(5, 5, 4, 2))

vars_F <- c("abdomen", "height", "hip", "knee")
titles_F <- c("Female - Abdomen (cm)", "Female - Height (cm/m)", "Female - Hip (cm)", "Female - Knee (cm)")

for (i in 1:4) {
  v <- vars_F[i]
  x_range <- range(c(df_final_check_F[[v]], df_without_bf_F[[v]]))
  y_range <- range(c(df_final_check_F$fat_percent, pred_2_points))
  
  plot(df_final_check_F[[v]], df_final_check_F$fat_percent,
       col = "grey75", pch = 16, xlim = x_range, ylim = y_range,
       main = paste("Diagnostic:", titles_F[i]),
       xlab = titles_F[i], ylab = "Fat Percent (%)")
  
  v_seq <- seq(x_range[1], x_range[2], length.out = 200)
  line_df_F <- data.frame(
    abdomen = mean(df_final_check_F$abdomen),
    height  = mean(df_final_check_F$height),
    hip     = mean(df_final_check_F$hip),
    knee    = mean(df_final_check_F$knee)
  )
  line_df_F <- line_df_F[rep(1, 200), ]
  line_df_F[[v]] <- v_seq
  line_df_F$pred <- predict(model_simple_F, newdata = line_df_F)
  lines(v_seq, line_df_F$pred, col = "blue", lwd = 2)
  
  points(df_without_bf_F[[v]], pred_2_points, col = "red", pch = 16, cex = 1.2)
  
  text(df_without_bf_F[[v]], pred_2_points,
       labels = paste0("P", seq_along(pred_2_points), ": ", round(pred_2_points, 1), "%"),
       col = "red", pos = 3, font = 2, cex = 0.8)
}
dev.off()


# the distribution plot for Male on 4 different coefficients, plot by claude
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

predictors <- c("abdomen", "height", "age", "wrist")
labels     <- c("Abdomen (cm)", "Height (cm)", "Age (years)", "Wrist (cm)")
pt_colors  <- c("red", "darkorange", "purple", "darkred")

for (i in seq_along(predictors)) {
  var <- predictors[i]
  
  m <- mean(df_final_check_M[[var]])
  s <- sd(df_final_check_M[[var]])
  med <- median(df_final_check_M[[var]])
  
  pt_vals <- df_without_bf_M[[var]]
  n_pts   <- length(pt_vals)
  
  x_min <- min(m - 3.5*s, pt_vals)
  x_max <- max(m + 3.5*s, pt_vals)
  x_seq <- seq(x_min, x_max, length.out = 400)
  y_seq <- dnorm(x_seq, mean = m, sd = s)
  y_max <- max(y_seq)
  
  plot(x_seq, y_seq, type = "n",
       main = labels[i],
       xlab = labels[i], ylab = "Density",
       ylim = c(-y_max * 0.45, y_max * 1.1))
  
  shade_region <- function(lo, hi, col) {
    xs <- x_seq[x_seq >= lo & x_seq <= hi]
    if (length(xs) < 2) return()
    polygon(c(xs, rev(xs)), c(dnorm(xs, m, s), rep(0, length(xs))),
            col = col, border = NA)
  }
  shade_region(m - 3*s, m + 3*s, "grey95")
  shade_region(m - 2*s, m + 2*s, "grey88")
  shade_region(m - s,   m + s,   "grey80")
  
  lines(x_seq, y_seq, lwd = 2, col = "black")
  
  abline(v = m,           col = "blue",       lty = 2, lwd = 1.5)
  abline(v = c(m-s, m+s), col = "steelblue4", lty = 3, lwd = 1)
  abline(v = c(m-2*s, m+2*s), col = "darkorange", lty = 3, lwd = 1)
  abline(v = c(m-3*s, m+3*s), col = "firebrick",  lty = 3, lwd = 1)
  
  abline(v = med, col = "darkgreen", lty = 2, lwd = 1.5)
  axis(side = 1, at = med, labels = round(med, 1),
       col.axis = "darkgreen", font = 2, cex.axis = 0.8)
  axis(side = 1, at = m, labels = round(m, 1),
       col.axis = "blue", font = 2, cex.axis = 0.8)
  
  text(m,       y_max * 1.05, "μ",   col = "blue",       cex = 0.75, font = 2)
  text(m + s,   y_max * 1.05, "+1σ", col = "steelblue4", cex = 0.7)
  text(m - s,   y_max * 1.05, "-1σ", col = "steelblue4", cex = 0.7)
  text(m + 2*s, y_max * 1.05, "+2σ", col = "darkorange", cex = 0.7)
  text(m - 2*s, y_max * 1.05, "-2σ", col = "darkorange", cex = 0.7)
  text(m + 3*s, y_max * 1.05, "+3σ", col = "firebrick",  cex = 0.7)
  text(m - 3*s, y_max * 1.05, "-3σ", col = "firebrick",  cex = 0.7)
  
  text(med, y_max * 0.5, "Median", col = "darkgreen", cex = 0.7, font = 2, srt = 90)
  
  y_base <- -y_max * 0.25
  points(pt_vals, rep(y_base, n_pts),
         col = pt_colors[seq_len(n_pts)], pch = 16, cex = 2)
  
  for (j in seq_len(n_pts)) {
    lines(c(pt_vals[j], pt_vals[j]),
          c(y_base, dnorm(pt_vals[j], m, s)),
          col = pt_colors[j], lty = 2, lwd = 1.2)
  }
  
  z_scores <- round((pt_vals - m) / s, 2)
  text(pt_vals, rep(y_base, n_pts),
       labels = paste0("P", seq_len(n_pts), "\n",
                       ifelse(z_scores >= 0, "+", ""), z_scores, "σ"),
       col = pt_colors[seq_len(n_pts)],
       pos = 1, cex = 0.85, font = 2)
}

par(mfrow = c(1, 1))
# ==============================================================================
# STEP 4.3 : Prediction Interval
# ==============================================================================
pi_check <- predict(model_simple_M, newdata = df_without_bf_M, interval = "prediction", level = 0.95)
df_4_points_with_pi <- cbind(df_without_bf_M, pi_check)
df_4_points_with_pi$margin_error <- round(((df_4_points_with_pi$upr - df_4_points_with_pi$lwr) / 2), 3)

pi_M_check <- predict(model_simple_M, newdata = df_final_check_M, interval = "prediction", level = 0.95)
df_M_with_pi <- cbind(df_final_check_M, pi_M_check)
df_M_with_pi$margin_error <- round(((df_M_with_pi$upr - df_M_with_pi$lwr) / 2), 3)

pi_check <- predict(model_simple_F, newdata = df_without_bf_F, interval = "prediction", level = 0.95)
df_2_points_with_pi <- cbind(df_without_bf_F, pi_check)
df_2_points_with_pi$margin_error <- round(((df_2_points_with_pi$upr - df_2_points_with_pi$lwr) / 2), 3)

pi_F_check <- predict(model_simple_F, newdata = df_final_check_F, interval = "prediction", level = 0.95)
df_F_with_pi <- cbind(df_final_check_F, pi_F_check)
df_F_with_pi$margin_error <- round(((df_F_with_pi$upr - df_F_with_pi$lwr) / 2), 3)


# ==============================================================================
# STEP 4.4 : Refactor, fix the PI, or try to prove the data is not good enough to have result better.
# ==============================================================================

df_M_re <- df %>% filter(sex == "M") %>% select(-(c("sex", "study", "bmi")))
df_M_re$height <- df_M_re$height * 100


model_simple_M <- lm(fat_percent ~ height + abdomen + age + wrist, data = df_M_re[-c(39,42,182),])
# 4.108717
model_with_chest_M <- lm(fat_percent ~ height + abdomen + chest + age + wrist,data = df_M_re[-c(39,42,182),])
# 4.151072
model_with_weight_M <- lm(fat_percent ~ height + abdomen + age + wrist + weight, data = df_M_re[-c(39,42,182),])
# 4.114175
model_more_interaction_M <- lm(fat_percent ~ abdomen * height + neck * height + weight * height, data = df_M_re[-c(39,42,182),])
# 4.176986
model_advanced_M_M <- lm(fat_percent ~ poly(abdomen, 2) + height + age + wrist + abdomen:height, data = df_final_check_M[-c(39,42,182),])
# 4.079906

model_advanced_M_log <- lm(log(fat_percent) ~ poly(abdomen, 2) + height + age + wrist + abdomen:height, data = df_M_re[-c(39,42,182),])
pi_ad_M <- predict(model_advanced_M_log, newdata = df_M_re[-c(39,42,182),], interval = "prediction", level = 0.95)
pi_normal <- exp(pi_ad_M)
df_M <- cbind(df_M_re[-c(39,42,182),], pi_normal)
df_M$margin_error <- round(((df_M$upr - df_M$lwr) / 2), 3)
mean_me_log_M <- mean(df_M$margin_error, na.rm = TRUE) / 2
# 5.912797
model_all_M <- lm(fat_percent ~ weight + height + age + abdomen + ankle + biceps + 
                  calf + chest + elbow + forearm + hip + knee + neck + thigh + wrist, 
                data = df_M_re[-c(39,42,182),])
# 4.099424

summary(model_simple_M)$sigma


            # ==============================================================================
# model_simple_F <- lm(fat_percent ~ height + abdomen + hip + knee, data = df_final_check_F)
df_F_re <- df %>% filter(sex == "F") %>% select(-(c("sex", "study", "bmi")))
df_F_re$height <- df_F_re$height * 100


model_simple_M <- lm(fat_percent ~ height + abdomen + age + wrist, data = df_M_re[-c(39,42,182),])

model_simple_F <- lm(fat_percent ~ height + abdomen + hip + knee, data = df_F_re)

model_with_chest_F <- lm(fat_percent ~ height + abdomen + chest + hip + knee,data = df_F_re)

model_with_weight_F <- lm(fat_percent ~ height + abdomen + hip + kneed + weight, data = df_F_re)

model_more_interaction_F <- lm(fat_percent ~ abdomen * height + neck * height + weight * height, data = df_F_re)

model_advanced_M <- lm(fat_percent ~ poly(abdomen, 2) + height + hip + knee + abdomen:height, data = df_F_re)

model_advanced_M_log <- lm(log(fat_percent) ~ poly(abdomen, 2) + height + hip + knee + abdomen:height, data = df_F_re)
pi_ad_M <- predict(model_advanced_M_log, newdata = df_F_re, interval = "prediction", level = 0.95)
pi_normal <- exp(pi_ad_M)
df_F <- cbind(df_F_re, pi_normal)
df_F$margin_error <- round(((df_F$upr - df_F$lwr) / 2), 3)
mean_me_log_M <- mean(df_F$margin_error, na.rm = TRUE) / 2


model_all_F <- lm(fat_percent ~ weight + height + age + abdomen + ankle + biceps + 
                  calf + chest + elbow + forearm + hip + knee + neck + thigh + wrist, 
                data = df_F_re)

summary(model_with_chest)$sigma



# ==============================================================================
# STEP 5: Other plots
# ==============================================================================
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1) 
with(df_final_check_M, {  plot(abdomen, fat_percent, 
       col = "grey50", pch = 16, 
       main = "Outlier Detect",
       xlab = "Abdomen",
       ylab = "Fat Percent")
    abline(lm(fat_percent ~ abdomen), col = "black", lty = 2, lwd = 2)
    points(abdomen[c(39, 42)], fat_percent[c(39, 42)], 
         col = c("red", "blue"), pch = 16, cex = 2)
    text(abdomen[c(39, 42)], fat_percent[c(39, 42)], 
       labels = c("Point 39", "Point 42"), 
       col = c("red", "blue"), pos = 3, font = 2)
})


# model_simple_F <- lm(fat_percent ~ height + abdomen + hip + knee, data = df_final_check_F)
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1) 
with(df_final_check_F, {  plot(hip, fat_percent, 
       col = "grey50", pch = 16, 
       main = "Outlier Detect",
       xlab = "hip",
       ylab = "Fat Percent")
    abline(lm(fat_percent ~ hip), col = "black", lty = 2, lwd = 2)
    points(hip[c(107)], fat_percent[c(107)], 
         col = c("red"), pch = 16, cex = 2)
    text(hip[c(107)], fat_percent[c(107)], 
       labels = c("Point 107"), 
       col = c("red"), pos = 3, font = 2)
})


# ==============================================================================
# STEP 6: last
# ==============================================================================
# Call:
# lm(formula = fat_percent ~ height + abdomen + age + wrist, data = df_M_re[-c(39, 
#     42, 182), ])

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -10.0431  -2.9352  -0.3554   3.1948   8.9104 

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   4.57809    7.76623   0.589  0.55608    
# height      -12.62349    4.55808  -2.769  0.00605 ** 
# abdomen       0.73881    0.03243  22.785  < 2e-16 ***
# age           0.05288    0.02286   2.313  0.02156 *  
# wrist        -1.84590    0.39251  -4.703  4.3e-06 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 4.109 on 244 degrees of freedom
# Multiple R-squared:  0.7347,    Adjusted R-squared:  0.7303 
# F-statistic: 168.9 on 4 and 244 DF,  p-value: < 2.2e-16



# r$> summary(model_simple_F)
# Call:
# lm(formula = fat_percent ~ height + abdomen + hip + knee, data = df_final_check_F)

# Residuals:
#      Min       1Q   Median       3Q      Max 
# -10.0747  -2.2324  -0.0262   2.3619   8.6667 

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -17.01545    7.61681  -2.234   0.0267 *  
# height       -0.20488    0.04708  -4.352 2.26e-05 ***
# abdomen       0.35702    0.06303   5.664 5.81e-08 ***
# hip           0.34041    0.07960   4.277 3.08e-05 ***
# knee          0.42419    0.16781   2.528   0.0123 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 3.475 on 179 degrees of freedom
# Multiple R-squared:  0.6503,    Adjusted R-squared:  0.6425 
# F-statistic: 83.22 on 4 and 179 DF,  p-value: < 2.2e-16