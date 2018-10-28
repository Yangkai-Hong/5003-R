
# install the needed packages if not exists
# install.packages(c("VIM", "readr", "randomForest", "mice"))

# change the working directory
setwd("D:\\Steve_Data\\Comp5318\\assignment_2")

library(VIM)
library(readr)
library(randomForest)
library(mice)

# it take quite a while for all the imputation work down.
# Particularly the random forest imputation part is slow
# Overall it takes45 - 50 Minutes to run the imputation

# 1. Using VIM KNN
df_all <- read_csv("adult_with missing_value.csv") # this csv was generated in python project
last_col <-  dim(df_all)[2]
predict_feature = c('age', 'fnlwgt', 'education_num', 'marital_status', 'relationship',
                    'race', 'sex', 'capital_gain', 'capital_loss', 'hours_per_week')

df_all_knn <- kNN(df_all, k = 5, dist_var = predict_feature, useImputedDist = FALSE)
# only take the imputed data not the rest of impuatational infomation
write_csv(df_all_knn[1:last_col], "adult_with missing_value_knn.csv")

###########################################################################
# Using mice, first create an function

myimpute = function(df, method = c("rf", "rf", "rf")) {
  init = mice(df, maxit = 5) 
  meth = init$method
  predM = init$predictorMatrix
  
  # remove the variable as predictor but still will be imputed.
  # predM[, c('workclass', 'occupation', 'native_country')]=0
  
  #  skip a variable from imputation use the code below. This variable will be used for prediction.
  predict_feature = c('age', 'fnlwgt', 'education_num', 'marital_status', 'relationship',
                      'race', 'sex', 'capital_gain', 'capital_loss', 'hours_per_week')  # not include the earning_labels
  # predict_feature = c('age', 'fnlwgt', 'education_num', 'marital_status',
  #                     'race', 'sex', 'capital_gain','hours_per_week', 'earning_label')  
  
  meth[predict_feature] = ""
  
  # specify the methods for imputing the missing values
  meth[c("workclass", "occupation", "native_country")] = method
  
  
  imputed = mice(df, method = meth, predictorMatrix=predM, m = 5)
  
  
  imputed <- complete(imputed)
  
  return(imputed)
}

df_all <- read_csv("adult_with missing_value.csv") # this csv was generated in python project
last_col <-  dim(df_all)[2]

# 2. using mice cart

set.seed(1234)

tempData_cart <- myimpute(df_all, c("cart", "cart", "cart")) # all 3 parameters use cart
write_csv(tempData_cart[1:last_col], "adult_with missing_value_cart.csv")

# 3. using mice randon forest 

set.seed(1234)
tempData_rf <- myimpute(df_all, c("rf", "rf", "rf")) # all 3 parameters use rf
# df_imputed_rf <- complete(tempData)
write_csv(tempData_rf[1:last_col], "adult_with missing_value_rf.csv")


df_all <- read_csv("adult_with missing_value.csv") # this csv was generated in python project
last_col <-  dim(df_all)[2]

str(df_all)

df_all["workclass"] <- factor(df_all["workclass"])


