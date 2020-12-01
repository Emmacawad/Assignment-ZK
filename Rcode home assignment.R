data_sample_1 = read.csv("https://tinyurl.com/ha-dataset1")
View(data_sample_1)
summary(data_sample_1)

## Models
model1 <- lm(pain ~ age + sex, data = data_sample_1_corr)
model2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_1_corr)

## Data exploration
describe(data_sample_1)

data_sample_1 %>% ggplot() +aes(x = age) +geom_histogram()

data_sample_1_corrected <- data_sample_1 %>%
  mutate(age = replace(age, age=="444", 44))

data_sample_1_corrected %>% ggplot() +aes(x = age) +geom_histogram()
data_sample_1_corrected %>% ggplot() +aes(x = STAI_trait) +geom_histogram()

## Correction of coding errors
data_sample_1_corr <- data_sample_1_corrected %>%
  mutate(STAI_trait = replace(STAI_trait, STAI_trait=="3.9", 39.0))

## Data exploration
data_sample_1_corr %>% ggplot() +
  aes(x = STAI_trait) + 
  geom_bar()

data_sample_1_corr %>% ggplot() +
  aes(x = pain_cat) + 
  geom_bar()

data_sample_1_corr %>% ggplot() +
  aes(x = mindfulness) + 
  geom_histogram()

data_sample_1_corr %>% ggplot() +
  aes(x = cortisol_serum) + 
  geom_histogram()

data_sample_1_corr %>% ggplot() +
  aes(x = cortisol_saliva) + 
  geom_histogram()

## Model diagnostics 
model2 %>% plot(which=5)
model2 %>% plot(which=4)

data_sample_1_corr %>% slice(c(96, 100, 114))

model2 %>% plot(which = 2)

residuals_model2 = enframe(residuals(model2)) 
residuals_model2 %>% ggplot() + aes(x = value) + geom_histogram()

describe(residuals(model2))

model2 %>% residualPlots()

model2 %>% plot(which = 3)

model2 %>% ncvTest()
model2 %>% bptest()

model2 %>% vif()

## Correction of multicollinearity
data_sample_1_corr = data_sample_1_corr %>% 
  mutate(cortisol_serum_centered = cortisol_serum - mean(cortisol_serum), cortisol_saliva_centered = cortisol_saliva - mean(cortisol_saliva))

model2_centered=lm(pain ~ age + sex + STAI_trait + pain_cat + cortisol_serum_centered * cortisol_saliva_centered, data = data_sample_1_corr)

model2_centered %>% vif()

model2 %>% vif()

summary(model1)
summary (model2)

model2=lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +cortisol_serum + cortisol_saliva, data = data_sample_1_corr)
model2%>% vif()

model2_centered=lm(pain ~ age + sex + STAI_trait + pain_cat + +mindfulness + cortisol_serum_centered * cortisol_saliva_centered, data = data_sample_1_corr)
model2_centered %>% vif()

model2=lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +cortisol_serum, data = data_sample_1_corr)
model2 %>% vif()

## Coefficients and p-values
summary(model1)
summary (model2)

## Standarized Coefficients and Confidences Intervals
confint(model2)
confint(model1)

lm.beta(model2)
lm.beta(model1)

## R^2
summary(model1)$adj.r.squared
summary(model2)$adj.r.squared

## Model Comparison 
AIC(model1)
AIC(model2)

anova(model1, model2)

##Data Exploration
data_sample_1_corr %>% ggplot() +
  aes(x = IQ) + 
  geom_histogram()

data_sample_1_corr %>% ggplot() +
  aes(x = weight) + 
  geom_histogram()

data_sample_1_corr %>% ggplot() +
  aes(x = household_income) + 
  geom_histogram()

## Data Correction
data_sample_1_corr1 <- data_sample_1_corr %>%
  mutate(household_income = replace(household_income, household_income=="-3732", NA))

## New Model and Data Exploration
newmodel<- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data=data_sample_1_corr1)

newmodel %>% plot(which=4)

newmodel %>% plot(which = 2)

residuals_newmodel = enframe(residuals(newmodel)) 
residuals_newmodel %>% ggplot() + aes(x = value) + geom_histogram()

newmodel %>% residualPlots()

newmodel %>% plot(which = 3)

newmodel %>% ncvTest()
newmodel %>% bptest()

newmodel %>% vif()

## Backward regression
backwardmodel = step(newmodel, direction = "backward")

backwardmode<- lm(pain ~ sex + household_income + mindfulness + age + pain_cat + cortisol_serum, data=data_sample_1_corr1)

## Model Comparison
AIC(newmodel)

summary(backwardmodel)

## New Regression Models
backwardmodel<- lm(pain ~ sex + household_income + mindfulness + age + pain_cat + cortisol_serum, data=data_sample_1_corr1)
theorymodel<- lm(pain ~ age + sex+ STAI_trait + pain_cat + mindfulness + cortisol_serum, data=data_sample_1_corr1)

## Model Comparison
AIC(backwardmodel)
AIC(theorymodel)

anova(backwardmodel, theorymodel)

## Applied to second data
data_sample_2=read.csv("https://tinyurl.com/ha-dataset2")
backwardmodel<- lm(pain ~ sex + household_income + mindfulness + age + pain_cat + cortisol_serum, data=data_sample_2)
theorymodel<- lm(pain ~ age + sex+ STAI_trait + pain_cat + mindfulness + cortisol_serum, data=data_sample_2)

## Coefficients, Standarized Coefficients and Confidence Intervals
summary(backwardmodel)
confint(backwardmodel)
lm.beta(backwardmodel)

## Prediction Performance
backwardmodel %>%
  sum((data_sample_2$outcome-predict(backwardmodel))^2)

sum((data_sample_2$outcome-predict(theorymodel))^2)

data_sample_3=read.csv("https://tinyurl.com/ha-dataset3")
view(data_sample_3)

## Random Intercept Model
modelrandomhospital= lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +cortisol_serum + (1|hospital), data = data_sample_3)

## Standarized Coefficients and Confidence Intervals
confint(modelrandomhospital)
summary(modelrandomhospital)
stdCoef.merMod(modelrandomhospital)

## R^2
r.squaredGLMM(modelrandomhospital)
r2beta(model2)

## Random Intercept Model applied to data set 4
data_sample_4=read.csv("https://tinyurl.com/ha-dataset4")
view(data_sample_4)
modelrandomhospital= lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +cortisol_serum + (1|hospital), data = data_sample_3)

modelrandomhospital

modelrandomhospital_prediction = predict(modelrandomhospital, data_sample_4, allow.new.level = TRUE)
modelrandomhospital_prediction

RSS= sum((data_sample_4$pain- predict(modelrandomhospital, data_sample_4, allow.new.level = TRUE))^2)
RSS

Modelrandomhospital_mean= lmer(pain ~ 1 + (1|hospital), data=data_sample_4)
Modelrandomhospital_mean

TSS= sum((data_sample_4$pain - predict(modelrandomhospital_mean, data_sample_4, allow.new.level = TRUE))^2)
TSS

r2 = 1- (RSS/TSS)
r2

## R^2
r.squaredGLMM(modelrandomhospital)
r.squaredGLMM(model2)

modelrandomhospital

##Sum Resduals

sum(residuals(modelrandomhospital)^2)
sum(residuals(model2)^2)

## Random Intercept and Slope Model
modelrandomhospital2 = lmer(pain ~ age + (age|hospital), data = data_sample_3)

## Graph of Fitted Regression Lines for Each Hospital
datasample_3 = data_sample_3
datasample_3$pred_int = predict(modelrandomhospital) 
datasample_3$pred_slope = predict(modelrandomhospital2)

ggplot(datasample_3, aes(y = pain, x = age, group = hospital))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_slope, x=age))+ 
  facet_wrap( ~ hospital, ncol = 5)

modelrandomhospital= lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness +cortisol_serum + (1|hospital), data = data_sample_3)

modelrandomhospital
