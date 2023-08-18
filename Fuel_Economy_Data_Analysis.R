library(ggplot2)
library(tidyverse) 
library(data.table)
library(rstudioapi)
library(skimr)
library(inspectdf)
library(mice)
library(plotly)
library(highcharter)
library(recipes) 
library(caret) 
library(purrr) 
library(graphics) 
library(Hmisc) 
library(glue)
library(h2o)
#1. Add ggplot2::mpg dataset.
df("mpg", package = "ggplot2")
df<-mpg%>%view()

#2. Make data ready for analysis doing preprocessing techniques.
df %>% skimr::skim()
sapply(df, class)

#Step1: Inpute missing values
df %>% inspect_na()
#Step2:Variable encoding
df %>% glimpse()
#Step3:Multicollinearity Check
target <- 'cty'
features <- df %>%  select(displ,year,cyl) %>% names() 
f<-as.formula(paste(target,paste(features, collapse = "+"), sep = "~")) 

glm <- glm(f,data = df)
glm %>% summary()


glm %>% faraway::vif() %>% sort(decreasing = T) %>% names()->features
df<-df %>% select(target,features)

#Step4:Scaling
df %>% glimpse()
df<-df %>% scale() %>% as.data.frame()
df %>% glimpse


#3. Fit Generalized Linear Model using H2O in R.
h2o.init()

h2o_data<-df %>% as.h2o()

#Step1: Train/Test split
h2o_data<-h2o_data %>% h2o.splitFrame(ratios=0.8, seed =123)
train<-h2o_data[[1]]
test<-h2o_data[[2]]

#4. Run GLM using following modelling structure. cty ~ year + cyl + displ.
target <- 'cty'
features <- df %>%  select(displ,year,cyl) %>% names() 

#Step 2. fit the model
model<-h2o.glm(
  x=features, y= target,
  training_frame = train,
  validation_frame = test,
  nfolds=10, seed=123,
  lambda = 0, compute_p_values = T)

model@model$coefficients_table %>% 
  as.data.frame() %>% 
  select(names, p_value) %>% 
  mutate(p_value = round(p_value,3)) %>% 
  .[-1,] %>% 
  arrange(desc(p_value))

##Step3 Remove unsignificant values from data
while(model@model$coefficients_table %>% 
      as.data.frame() %>% 
      select(names, p_value) %>% 
      mutate(p_value = round(p_value,3)) %>% 
      .[-1,] %>% 
      arrange(desc(p_value)) %>% 
      .[1,2]> 0.05) {
  model@model$coefficients_table %>% 
    as.data.frame() %>%
    dplyr::select(names,p_value) %>% 
    mutate(p_value = round(p_value,3)) %>% 
    filter(! is.nan(p_value)) %>% 
    .[-1,] %>% 
    arrange(desc(p_value)) %>% 
    .[1,1] -> v
  features <- features[features!=v]
  train_h2o <- train %>% as.data.frame() %>%  select(target,features) %>% as.h2o()
  test_h2o <- test %>% as.data.frame() %>%  select(target,features) %>% as.h2o()
  
  model<-h2o.glm(
    x=features, y= target,
    training_frame = train,
    validation_frame = test,
    nfolds=10, seed=123,
    lambda = 0, compute_p_value = T)}  
# Check the update p_values
model@model$coefficients_table %>% 
  as.data.frame() %>%
  dplyr::select(names,p_value) %>% 
  mutate(p_value = round(p_value,3))

#Step 4 Predictions

y_pred<- model %>%  h2o.predict(newdata = test) %>%  as.data.frame()

#5. Print coefficients table and give interpretation of results.
#Evaluotion

test_set <- test %>% as.data.frame()
residuals = test_set$cty - y_pred$predict

#RMSE
RMSE = sqrt(mean(residuals^2))

#MAE
MAE = mean(abs(residuals))

#R Squared
y_test_mean = mean(test_set$cty)

#Total sum of squares
tss = sum((test_set$cty - y_test_mean)^2)

#Residual sum of squares
rss = sum(residuals^2)
R2 = 1- (rss/tss)
R2

#Adjusted R squared

n<-test_set %>% nrow()

#number of independent variables
k<-features %>%  length()
Adjusted_R2 = 1 - (1-R2)*((n-1)/(n-k-1))

#Comparison
tibble(RMSE =round(RMSE,1),
       R2, Adjusted_R2)

#Actual VS Predicted Plot(test)

my_data <- cbind(predicted=y_pred$predict,
                 observed = test_set$cty) %>% 
  as.data.frame()

g <- my_data %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color= "darkred")+
  geom_smooth(method=lm) +
  labs(x="Predicted CTY",
       y="Actual CTY",
       title=glue('Test:Adjusted R2={round(enexpr(Adjusted_R2),2)}')) +
  theme(plot.title = element_text(color='darkgreen',size=16,hjust=0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14))

g %>% ggplotly()

#Overfitting Check

y_pred_train <- model %>%  h2o.predict(newdata = train) %>%  as.data.frame()

train_set <- train %>%  as.data.frame()
residuals = train_set$cty - y_pred_train$predict

RMSE_train = sqrt(mean(residuals^2))

y_train_mean = mean(train_set$cty)

tss = sum((train_set$cty - y_train_mean)^2)
rss = sum(residuals^2)

R2_train = 1- (rss/tss);
R2_train

n<- train_set %>%  nrow()
k<- features %>%  length()

Adjusted_R2_train = 1 - (1-R2_train)*((n-1)/(n-k-1))

#Actual VS Predicted plot(training)

my_data_train <- cbind(predicted=y_pred_train$predict,
                       observed = train_set$cty) %>% 
  as.data.frame()

g_train <- my_data_train %>% 
  ggplot(aes(predicted, observed)) + 
  geom_point(color= "darkred")+
  geom_smooth(method=lm) +
  labs(x="Predicted CTY",
       y="Actual CTY",
       title=glue('Train:Adjusted R2={round(enexpr(Adjusted_R2_train),2)}')) +
  theme(plot.title = element_text(color='darkgreen',size=16,hjust=0.5),
        axis.text.y = element_text(size=12),
        axis.text.x = element_text(size=12),
        axis.title.y = element_text(size=14),
        axis.title.x = element_text(size=14))

g_train %>%  ggplotly()

#comparisson of plots
library(patchwork)
g_train + g

