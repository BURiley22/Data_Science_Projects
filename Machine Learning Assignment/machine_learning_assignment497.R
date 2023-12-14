# load in the libraries

library(tidyverse)
install.packages("tidyverse")
library(reshape2)

# load in the data
housing = read.csv('housing.csv')

# look over data, check for weirdness
head(housing)

# checking in the #'s are #'s and categoricals are categoricals
summary(housing)

par(mfrow=c(2,5))

# checking all column names
colnames(housing)

# checking the variables
ggplot(data = melt(housing), mapping = aes(x = value)) +
  geom_histogram(bins = 30) + facet_wrap(~variable, scales = 'free_x')

# impute missing values.  Using median instead of mean 
housing$total_bedrooms[is.na(housing$total_bedrooms)] = median(housing$total_bedrooms , na.rm = TRUE)

# fix the total columns

housing$mean_bedrooms = housing$total_bedrooms/housing$households
housing$mean_rooms = housing$total_rooms/housing$households

drops = c('total_bedrooms', 'total_rooms')

housing = housing[ , !(names(housing) %in% drops)]

head(housing)

# turn categoricals into booleans

categories = unique(housing$ocean_proximity)
#split the categories off
cat_housing = data.frame(ocean_proximity = housing$ocean_proximity)

for(cat in categories){
  cat_housing[,cat] = rep(0, times=nrow(cat_housing))
}
head(cat_housing)

for (i in 1:length(cat_housing$ocean_proximity)) {
  cat = as.character(cat_housing$ocean_proximity[i])
  cat_housing[,cat][i] = 1
}

head(cat_housing)

cat_columns = names(cat_housing)
keep_columns = cat_columns[cat_columns != 'ocean_proximity']
cat_housing = select(cat_housing, one_of(keep_columns))

tail(cat_housing)

colnames(housing)

drops = c('ocean_proximity', 'median_house_value')
housing_num = housing[ , !(names(housing) %in% drops)]
head(housing_num)

scaled_housing_num = scale(housing_num)

head(scaled_housing_num)

# merge the altered numerical and categorical dataframes

cleaned_housing = cbind(cat_housing, scaled_housing_num, median_house_value=housing$median_house_value)
head(cleaned_housing)

# create a test set of data

#set a random seed to be able to reproduce in the future
set.seed(1738)

sample = sample.int(n = nrow(cleaned_housing), size = floor(.8*nrow(cleaned_housing)), replace = F)
train = cleaned_housing[sample, ] #just the samples
test = cleaned_housing[-sample, ]  # all but the samples

# checking to make sure we are where we should be

head(train)

# see if all the dataframe lengths are equal

nrow(train) + nrow(test) == nrow(cleaned_housing)

library('boot')

?cv.glm 

glm_house = glm(median_house_value~median_income+mean_rooms+population, data=cleaned_housing)
k_fold_cv_error = cv.glm(cleaned_housing, glm_house, K=5)

k_fold_cv_error$delta

glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse

# what parts of the model are callable?
names(glm_house)

glm_house$coefficients

# Random forest Model

library('randomForest')
install.packages('randomForest')
library('randomForest')

?randomForest

names(train)

set.seed(1738)

train_y = train[, 'median_house_value']
train_x = train[, names(train) !='median_house_value']

head(train_x)
head(train_y)


rf_model = randomForest(train_x, y = train_y, ntree = 500, importance = TRUE)

names(rf_model)

rf_model$importance

# The out-of-bag error estimate

oob_prediction = predict(rf_model) 

train_mse = mean(as.numeric((oob_prediction - train_y)^2))
oob_rmse = sqrt(train_mse)
oob_rmse

# how well does the model predict on the test data?

test_y = test[, 'median_house_value']
test_x = test[, names(test) !='median_house_value']

y_pred = predict(rf_model , test_x)
test_mse = mean(((y_pred - test_y)^2))
test_rsme = sqrt(test_mse)
test_rmse
