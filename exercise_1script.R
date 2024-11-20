#install necesarry packages
library(tidyverse)
library(broom)
library(yardstick)
library(ggfortify)
#1A LOAD, inspect, and prep data,
food_env<- read_csv("assignment1_dataset.csv")
summary(food_env)
food_env_full<- food_env %>%
  na.omit(food_env)%>%
  mutate(METRO13log=as.logical((food_env_full$METRO13)))
summary(food_env_full)

#1B filter/slice the data to answer specific questions
MICHPOVRATE<-food_env_full %>%
  filter(State == "MI")%>%
  filter(POVRATE15 <= 15.0)%>%
  arrange(desc(Pop2020))%>%
  select(County,State, POVRATE15, Pop2020)
print(MICHPOVRATE)
  
STATEPOV40<- food_env_full%>%
  filter(POVRATE15 > 40)%>%
  count(State)
print(STATEPOV40)

METRO<-food_env_full%>%
  group_by(State, METRO13log)%>%
  summarise(truemetro = n())
print(METRO)  

FOODINS<- food_env_full%>%
  group_by(State)%>%
  summarise(min = min(food_insecurity_2016), max = max(food_insecurity_2016),
                mean = mean(food_insecurity_2016), med = median(food_insecurity_2016))%>%
  arrange(med)
print(FOODINS)

#1C Create New columns to standardize count data

ggplot(food_env_full, aes(x = GROC16)) +geom_histogram()
food_env_full_10k<- food_env_full%>%
  mutate(GROC16_per10K = GROC16 /(Pop2020/10000), FMRKT_SNAP18per10k = FMRKT_SNAP18/(Pop2020/10000),SNAPS17_per10k = SNAPS17/(Pop2020/10000))%>%
  select(-c(GROC16, FMRKT_SNAP18, SNAPS17))
ggplot(food_env_full_10k, aes(x = GROC16_per10K)) + geom_histogram()

#1d Outliers
ggplot(food_env_full_10k, aes (x = PCT_LACCESS_HHNV15))+ geom_boxplot()
quan3<-quantile(food_env_full_10k$PCT_LACCESS_HHNV15, 0.75)
quan1<-quantile(food_env_full_10k$PCT_LACCESS_HHNV15, 0.25)
food_env_nooutlier<- food_env_full_10k%>%
  filter(PCT_LACCESS_HHNV15 <= quan3)%>%
  filter(PCT_LACCESS_HHNV15 >= quan1)
ggplot(food_env_nooutlier, aes (x = PCT_LACCESS_HHNV15))+ geom_boxplot()

#2a Simple linear regression
p1<- ggplot(food_env_full_10k, aes(x = SNAPS17_per10k, y = food_insecurity_2016)) + 
  geom_point() + geom_smooth(method="lm")
#2a Simple linear regression
p2<- ggplot(food_env_full_10k, aes(x = SNAPS17_per10k, y = food_insecurity_2016, col = METRO13log)) + 
  geom_point() + geom_smooth(method="lm")
print(p1)
print(p2)
snap_vs_foodsec_model <- lm(food_insecurity_2016 ~ SNAPS17_per10k, data = food_env_full_10k)

summary(snap_vs_foodsec_model)
pred<-data.frame(SNAPS17_per10k = 21.5)
pred_data<- pred%>%
  mutate(food_insecurity_2016 =predict(snap_vs_foodsec_model, newdata = pred))
print(pred_data)
p3<- ggplot(food_env_full_10k, aes(x = SNAPS17_per10k, y = food_insecurity_2016)) + 
  geom_point() + geom_smooth(method="lm") +geom_point(data = pred_data, color = "red")
print(p3)
explanatory<-data.frame(SNAPS17_per10k = food_env_full_10k$SNAPS17_per10k)
pred_data<- explanatory%>%
  mutate(food_insecurity_2016 =predict(snap_vs_foodsec_model, newdata = explanatory))
rmse<- sqrt(mean((food_env_full_10k$food_insecurity_2016 - pred_data$food_insecurity_2016)^2))
print(rmse)
autoplot(snap_vs_foodsec_model)
simple_model1<-broom::glance(snap_vs_foodsec_model)
#2b looks at cooks value and leverage
food_env_full_10k%>%
mutate(lev = hatvalues(snap_vs_foodsec_model))%>%
  arrange(desc(lev))%>%
  select(State, County, lev )

food_env_cooks<- food_env_full_10k%>%
mutate(cooks = cooks.distance(snap_vs_foodsec_model))


autoplot(snap_vs_foodsec_model, which = 4:6)

ggplot(food_env_cooks%>% filter (cooks <0.2), aes(x = SNAPS17_per10k, y = food_insecurity_2016)) + geom_point() +geom_smooth(method = "lm")

#2c 5 states model

food_env_5_states<- food_env_full_10k%>%
  filter(State %in% c("FL", "WV", "MI", "WA", "NJ"))
  
ggplot(food_env_5_states, aes( x = State, y = SNAPS17_per10k)) + geom_boxplot()
model2<- lm(food_insecurity_2016 ~ State + SNAPS17_per10k,data = food_env_5_states)
ggplot(food_env_5_states, aes( x = SNAPS17_per10k, y = food_insecurity_2016, col = State)) + geom_point() + geom_smooth(method = "lm")
simple_model2<- broom::glance(model2)

print(simple_model1)
print(simple_model2)

# model 2 is a much better model.

#3a logistic regresion-build and asses a simple logisitic regression model.
food_env_log<- food_env_full_10k %>%
  mutate(METRO13_int = as.integer(METRO13))

log_model<- glm(METRO13_int ~ GROC16_per10K, data = food_env_log, family = "binomial")
coef(log_model)
ggplot(food_env_log, aes ( x = GROC16_per10K, y = METRO13_int)) + 
  geom_point() + geom_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial"))

explanatory_log<- tibble(GROC16_per10K = c(1.0, 4,7, 12.1, 21.9))

pred_log<-explanatory_log %>%
  mutate(METRO13_int = predict(log_model, newdata = explanatory_log, type = "response"),
         outcome = round(METRO13_int))

ggplot(food_env_log, aes ( x = GROC16_per10K, y = METRO13_int)) + 
  geom_point() + geom_smooth(method = "glm", se = FALSE, method.args = list(family = "binomial")) + 
  geom_point(data = pred_log, x= pred_log$GROC16_per10K, y = pred_log$METRO13_int, color = "red")

#develop confusion matrix
exp_data_log<- tibble(GROC16_per10K= seq(-1,30, 0.5))
pred_data_log<-exp_data_log%>%
  mutate(METRO13_int = (predict(log_model, newdata= exp_data_log, type = "response")),
         outcome = round(METRO13_int),
         odds_ratio = METRO13_int/(1-METRO13_int),
         log_odds_ratio = log (odds_ratio),
         log_odds_ratio2= predict(log_model, newdata = exp_data_log))

actual_resp<- food_env_log$METRO13_int
pred_resp<-round(fitted(log_model))

outcomes<- table(pred_resp, actual_resp)
confusion<-yardstick::conf_mat((outcomes))
print(confusion)
autoplot(confusion)
summary(confusion, event_level = "second")

#4 multivariate model
multi_lin_mod<-lm(food_insecurity_2016 ~ State + POVRATE15 + METRO13 + GROC16_per10K + SNAPS17_per10k, data = food_env_5_states)
summary(multi_lin_mod)
multi_lin_mod_int<-lm(food_insecurity_2016 ~ State + POVRATE15 + METRO13 + GROC16_per10K + SNAPS17_per10k + SNAPS17_per10k*State , data = food_env_5_states)
summary(multi_lin_mod_int)
exp_mutli<- tibble(State = c("MI","NJ","WA","WV"), County = c("Oogabooga", "Joisey", "Forest", "Country Road"), POVRATE15 =c(1,2,3,4), METRO13 =c(1,1,0,0), GROC16_per10K = c(1, 3, 0.5, 5), SNAPS17_per10k = c(0.2, 3, 1, 2.2) )
pred_multi <- exp_mutli %>%
  mutate(food_insecurity_2016 = predict(multi_lin_mod, newdata = exp_mutli), food_insecurity_2016_interaction = predict(multi_lin_mod_int, newdata = exp_mutli))
print(pred_multi)