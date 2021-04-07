########introduction slide (number 7 in the presentation he put in mooodle)##########

#merge product and transactional data

t_data
product_data
df = merge(t_data, product_data, by = 'product_name')
df

#compute the % of transactions per price range for both snacks and drinks 

aux1 = df[price >= 1.5 & price < 2]
aux1
aux1_s = aux1[type_drink_snack == 'snack']
aux1_d = aux1[type_drink_snack == 'drink']
aux1_d
count_s = nrow(aux1_s)
count_d = nrow(aux1_d)

#in the range 1.5 <= x < 2 100% of transactions are snacks 

aux2 = df[price >= 2 & price < 2.5]
aux2
aux2_s = aux2[type_drink_snack == 'snack']
aux2_d = aux2[type_drink_snack == 'drink']
count_s = nrow(aux2_s)
count_d = nrow(aux2_d)
perc_snacks = (count_s/nrow(aux2)) *100 #51%
perc_drinks = (count_d/nrow(aux2)) *100 #49%


aux3 = df[price >= 2.5 & price < 3]
aux3
aux3_s = aux3[type_drink_snack == 'snack']
aux3_d = aux3[type_drink_snack == 'drink']
count_s = nrow(aux3_s)
count_d = nrow(aux3_d)
perc_snacks = (count_s/nrow(aux3)) *100 #65.5%
perc_drinks = (count_d/nrow(aux3)) *100 #34.5%

aux4 = df[price >= 3 & price < 3.5]
aux4
aux4_s = aux4[type_drink_snack == 'snack']
aux4_d = aux4[type_drink_snack == 'drink']
count_s = nrow(aux4_s)
count_d = nrow(aux4_d)
perc_snacks = (count_s/nrow(aux4)) *100 #67.5%
perc_drinks = (count_d/nrow(aux4)) *100 #38.4%%

aux5 = df[price >= 3.5]
aux5
aux5_s = aux5[type_drink_snack == 'snack']
aux5_d = aux5[type_drink_snack == 'drink']
count_s = nrow(aux5_s)
count_d = nrow(aux5_d)
perc_snacks = (count_s/nrow(aux5)) *100 #40.6%
perc_drinks = (count_d/nrow(aux5)) *100 #59.4%

#Obtain number of transactions done by the same customers over different lags

df = setorderv(df, cols= c('machine', 'timestamp'))

df = df[, last_vend:=shift(timestamp, n = 1L), by = machine]
df = df[, last_vend_2:=shift(timestamp, n = 2L), by = machine]
df = df[, last_vend_3:=shift(timestamp, n = 3L), by = machine]


df = df[, delta1:= difftime(timestamp, last_vend, units = 'auto'), by = machine]
df = df[, delta2:= difftime(timestamp, last_vend_2, units = 'auto'), by = machine]
df = df[, delta3:= difftime(timestamp, last_vend_3, units = 'auto'), by = machine]
df

aux1 = df[delta1 < 60]
nrow(aux1) #448889 differences below 60 seconds --> meaning there exist 448889 couple of transactions being
#made within 60 seconds to one another
aux2 = df[delta2 < 120]
nrow(aux2) #165863 differences below 120 seconds --> meaning there exist 165863 groups of 3 transactions
#made within 120 seconds from the first one to the last one
aux3 = df[delta3 < 180]
nrow(aux3) #100425 differences below 180 seconds --> meaning there exist 100425 groups of 4 transactions 
#made within 180 seconds from the first one to the last one

### all done up to here is to replicate the 2 graphs in slide 7###########3


############location################

#merge data to have transactions, products sold and machine specifics in the same data table

df = merge(df, machine_data, by = 'machine')
df
Tot_Active_Days <- df[, length(unique(date)), machine]
Tot_Active_Days #total active days per machine
Tot_N_Trans <- count(df, 'machine')
Tot_N_Trans #total number of transactions per machine
aux <- merge(Tot_N_Trans, Tot_Active_Days, by = 'machine')
df <- merge(df, aux, by = 'machine')
df <- df[, Avg_Daily_Items := freq/V1]
df

#train-test split

df <- df[, New_Var := ifelse(is.na(train_AvgDailyPassengers),1,0)]
df

#Using machine data for the regression to avoid repetitions of the same features for the same machines

df1 = merge(machine_data, aux, by = 'machine')
df1 = df1[, Avg_Daily_Items := freq/V1]
df1 = df1[,c('freq','V1'):=NULL]
df1 = df1[, Train_Daily_Pass := ifelse(is.na(train_AvgDailyPassengers),1,0)]
df1 = df1[, Train_WDay_Pass := ifelse(is.na(train_AvgWorkingDayPassengers),1,0)]
df1

machines = unique(df1$machine)
machines
m_train = sample(machines, round(0.7*length(machines),0), replace = F)
m_test = setdiff(machines, m_train)

df1 %>% filter(machine %in% m_train) -> train_s
train_s
df1 %>% filter(machine %in% m_test) -> test_s
test_s

#modelling part: I have tried many different models using the previous train-test split, consider only model
#for which I have also done cross-validation

model1 = lm(Avg_Daily_Items ~ location_type +
              num_vendex_nearby_300 +
              n_density_5km +
              income_average +
              total_number_of_routes_600 +
              num_hotels +
              num_hotels_45 +
              Train_Daily_Pass +
              Train_WDay_Pass, 
            data = train_s)
summary(model1)
summary(model1)$r.squared #0.21 (lower)


model2 = lm(Avg_Daily_Items ~ location_type +
              n_density_5km +
              income_average +
              total_number_of_routes_600 +
              num_hotels +
              Train_Daily_Pass +
              Train_WDay_Pass, 
            data = train_s)
summary(model2)
summary(model2)$r.squared #0.21 (lower)

model3 = lm(Avg_Daily_Items ~ location_type +
              n_density_5km +
              income_average +
              num_hotels +
              Train_Daily_Pass, 
            data = train_s)
summary(model3)
summary(model3)$r.squared #0.185 (lower)

model4 = lm(Avg_Daily_Items ~ location_type +
              n_density_5km +
              log(income_average) +
              log(total_number_of_routes_600) +
              num_hotels +
              Train_Daily_Pass +
              Train_WDay_Pass, 
            data = train_s)
summary(model4)
summary(model4)$r.squared #0.22 (lower)

model5 = lm(Avg_Daily_Items ~ small_machine +
              location_type +
              n_density_5km +
              log(income_average) +
              log(total_number_of_routes_600) +
              num_hotels +
              Train_Daily_Pass +
              Train_WDay_Pass, 
            data = train_s)
summary(model5)
summary(model5)$r.squared #0.24 (lower)

model6 = lm(Avg_Daily_Items ~ small_machine +
              location_type +
              log(income_average) +
              log(total_number_of_routes_600) +
              num_hotels +
              Train_Daily_Pass, 
            data = train_s)
summary(model6)
summary(model6)$r.squared #0.244 () ###highest so far

model7 <- lm(Avg_Daily_Items ~ small_machine +
                     log(total_number_of_routes_600) +
                     num_hotels_45 +
                     Train_Daily_Pass +
                     num_vendex_nearby_300, 
             data = train_s)
summary(model7)
summary(model7)$r.squared #0.199 (lower)

model8 = lm(Avg_Daily_Items ~ small_machine +
              location_type +
              log(income_average) +
              log(total_number_of_routes_600), 
            data = train_s)
summary(model8)
summary(model8)$r.squared #0.24 (lower)

model9 = lm(Avg_Daily_Items ~ location_type +
              log(income_average) +
              log(total_number_of_routes_600) +
              num_hotels +
              Train_Daily_Pass, 
            data = train_s)
summary(model9)
summary(model9)$r.squared #0.22 (lower)

# ========================================================================== # 
#  Cross-validation
# ========================================================================== #
# 5 fold : Train-test split
cfolds <- 5
index <- sample(1:cfolds, nrow(df1), replace = TRUE)
# To simplify, create a train with index 1:4, test with index 5
train <- df1[index %in% 1:4,]
test <- df1[index == 5,]


modelcv <- lm(Avg_Daily_Items ~ location_type +
                log(income_average) +
                log(total_number_of_routes_600) +
                num_hotels +
                Train_Daily_Pass, data = train)
summary(modelcv)


model10 = gbm(Avg_Daily_Items ~ factor(location_type) +
              log(income_average) +
              log(total_number_of_routes_600) +
              num_hotels +
              Train_Daily_Pass, 
            data = train_s,
            n.trees = 1000, 
            shrinkage = 0.02, 
            n.minobsinnode = 50)
summary(model10)

# ========================================================================== # 
#  Cross-validation
# ========================================================================== #
# 5 fold : Train-test split
cfolds <- 5
index <- sample(1:cfolds, nrow(df1), replace = TRUE)
# To simplify, create a train with index 1:4, test with index 5
train <- df1[index %in% 1:4,]
test <- df1[index == 5,]


modelcv <- gbm(Avg_Daily_Items ~ factor(location_type) +
                 log(income_average) +
                 log(total_number_of_routes_600) +
                 num_hotels +
                 Train_Daily_Pass, 
               data = train_s,
               n.trees = 1000, 
               shrinkage = 0.02, 
               n.minobsinnode = 50)
summary(modelcv)

#train models with the most important variables

#location type

model_loctype = lm(Avg_Daily_Items ~ 
                     location_type, 
                   data = train_s)
summary(model_loctype)
summary(model_loctype)$r.squared #0.15
preds1 = sort(model_loctype$fitted.values, decreasing = TRUE)
Twenty_Percent = round(length(preds1)*0.2)
Twenty_Percent
Top <- preds1[1:Twenty_Percent]
Bottom <- preds1[(length(preds1)-Twenty_Percent):length(preds1)]           
Agg_sales_Top <- sum(Top) #3581
Agg_sales_Bottom <- sum(Bottom) #2095
Ratio_Top_Bottom <- Agg_sales_Top/Agg_sales_Bottom #1.71 

#conclusion: machines closer to transportation tend to sell 3.7 on average compared to the ones that are 
#further away from this latter. The top 20% sells 71% more compared to the bottom 20%.

#income average 

model_incomeavg = lm(Avg_Daily_Items ~ 
                     log(income_average), 
                   data = train_s)
summary(model_incomeavg)
summary(model_incomeavg)$r.squared #0.02
preds2 = sort(model_incomeavg$fitted.values, decreasing = TRUE)
Twenty_Percent = round(length(preds2)*0.2)
Twenty_Percent
Top <- preds2[1:Twenty_Percent]
Bottom <- preds2[(length(preds2)-Twenty_Percent):length(preds2)]           
Agg_sales_Top <- sum(Top) #3080
Agg_sales_Bottom <- sum(Bottom) #2739
Ratio_Top_Bottom <- Agg_sales_Top/Agg_sales_Bottom #1.12
#conclusion: the top 20% in terms of income average tends to sell 1.12 more compared to the bottom 20%

#total number of routes 600

model_nr = lm(Avg_Daily_Items ~ 
                       log(total_number_of_routes_600), 
                     data = train_s)
summary(model_nr)
summary(model_nr)$r.squared #0.08
preds3 = sort(model_nr$fitted.values, decreasing = TRUE)
Twenty_Percent = round(length(preds3)*0.2)
Twenty_Percent
Top <- preds3[1:Twenty_Percent]
Bottom <- preds3[(length(preds3)-Twenty_Percent):length(preds3)]           
Agg_sales_Top <- sum(Top) #3527
Agg_sales_Bottom <- sum(Bottom) #2264
Ratio_Top_Bottom <- Agg_sales_Top/Agg_sales_Bottom #1.56
#conclusion: the top 20% in terms of number of routes 600m away from the machine tends to sell 1.56 more 
#compared to the bottom 20%

#number of hotels 

model_nh = lm(Avg_Daily_Items ~ 
                num_hotels, 
              data = train_s)
summary(model_nh)
summary(model_nh)$r.squared #0.03
preds4 = sort(model_nh$fitted.values, decreasing = TRUE)
Twenty_Percent = round(length(preds4)*0.2)
Twenty_Percent
Top <- preds4[1:Twenty_Percent]
Bottom <- preds4[(length(preds4)-Twenty_Percent):length(preds4)]           
Agg_sales_Top <- sum(Top) #3333
Agg_sales_Bottom <- sum(Bottom) #2922
Ratio_Top_Bottom <- Agg_sales_Top/Agg_sales_Bottom #1.14
#same conclusion as before but with number of hotels 



###### Influence of variables ######

# location_type == transport ------> 66.9%
# log(income_average) -------------> 19.8%
# log(total_number_of_routes_600) -> 12.4%
# num_hotels ----------------------> 0.90%

#Analysis of average daily sales by location

count(df1, 'location_type')

#

aux1 = df1[location_type == 'transport']       
aux2 = df1[location_type == 'petrol station'] 
aux3 = df1[location_type == 'others']         

#mean average daily sales for the different location types

mean(aux1$Avg_Daily_Items) #10.32 for transport
mean(aux2$Avg_Daily_Items) #5.57  for petrol station
mean(aux3$Avg_Daily_Items) #6.63  for others

#average daily sales by looking at the quintiles of the fitted values of the model

high = mean(first_q)              #11.97 
med_high = mean(second_q)         #10.22
medium = mean(third_q)            #9.17
med_low = mean(fourth_q)          #7.04
low = mean(fifth_q, na.rm = TRUE) #5.52

#Model used and quintiles

model9 = lm(Avg_Daily_Items ~ location_type +
              log(income_average) +
              log(total_number_of_routes_600) +
              num_hotels, 
            data = train_s)
summary(model9)
summary(model9)$r.squared #0.22 (lower)

predsx = model9$fitted.values
predsx = sort(predsx, decreasing = TRUE)
length(predsx)
Twenty_Percent1 = round(length(predsx)*0.2)
Twenty_Percent1

#Quintiles of predictions (the same ones used before to compute average daily sales per quintile)

first_q = predsx[1:Twenty_Percent1]
second_q = predsx[(Twenty_Percent1):(2*Twenty_Percent1)]
third_q = predsx[(2*Twenty_Percent1):(3*Twenty_Percent1)]
fourth_q = predsx[(3*Twenty_Percent1):(4*Twenty_Percent1)]
fifth_q = predsx[(4*Twenty_Percent1):(5*Twenty_Percent1)]

newdata = data.frame(location_type = 'transport',
                     income_average = 1000,
                     total_number_of_routes_600 = 5,
                     num_hotels = 1
                     )


predict(model9, newdata = newdata)


#these are the values each observation should display to stay within the average daily sales values
#for each different quintile from highest to lowest

#1st quantile features' values:
# location_type = 'transport
# income_average = 100000
# total_number_of_routes_600 = 800
# num_hotels = 40


#2nd quantile features' values:
# location_type = 'transport'
# income_average = 20000
# total_number_of_routes_600 = 75
# num_hotels = 10

#3rd quantile features' values:
# location_type = 'transport'
# income_average = 10000
# total_number_of_routes_600 = 35
# num_hotels = 10

#4th quantile features' values:
# location_type = 'transport'
# income_average = 5000
# total_number_of_routes_600 = 25
# num_hotels = 5

#5th quantile features' values:
# location_type = 'transport'
# income_average = 1000
# total_number_of_routes_600 = 5
# num_hotels = 1

auxx = merge(df1, aux, by = 'machine')
auxx = auxx[V1 > 72]
auxx = setorderv(auxx, cols = c('Avg_Daily_Items'))
auxx

#In this part we opt for a 'Conservative' strategy, that is bringing the worst 3 percent of the machines
#active for at least 80% of the time period under consideration (72 days) to the 2nd quintile, that is 
#adjusting for the conditions specified before

Worst_3perc = auxx[1:66]
Worst_3perc

mean(Worst_3perc$Avg_Daily_Items)
Total_Avg_Sales_W3 = sum(Worst_3perc$Avg_Daily_Items)
Total_Avg_Sales_W3
length(Worst_3perc$machine) #66

mean(second_q) #10.2

Upgraded_Sales = 10.2*66
Upgraded_Sales

Perc_Incr_Conservative = (((Upgraded_Sales*1.7) - (Total_Avg_Sales_W3*1.7))/(Total_Avg_Sales_W3*1.7))*100
Perc_Incr_Conservative
Perc_Incr_Conservative_P = (Upgraded_Sales*1.7)/(sum(df1$Avg_Daily_Items)*1.7)
Perc_Incr_Conservative_P #0.03% increase in total profits 



#brought to the same conditions necessary by the predictions of the model to reach the average 
#daily sales of the 1st top quntile 

Worst_5perc = auxx[1:110]
Worst_5perc

Total_Avg_Sales_W5 = sum(Worst_5perc$Avg_Daily_Items)
Total_Avg_Sales_W5
length(Worst_5perc$machine) #110

mean(first_q) #11.84

Upgraded_Sales1 = 11.84*110
Upgraded_Sales1

Perc_Incr_Aggressive = (((Upgraded_Sales1*1.7) - (Total_Avg_Sales_W5*1.7))/(Total_Avg_Sales_W5*1.7))*100
Perc_Incr_Aggressive
Perc_Incr_Aggressive_P = (Upgraded_Sales1*1.7)/(sum(df1$Avg_Daily_Items)*1.7)
Perc_Incr_Aggressive_P #0.06% increase in total profits

############################################################
#Additional lever: swap small machines for big machines
############################################################

# Auxliary function -------------------------------------------------------
tbr = function(a, b){
  top = mean(a[order(-b)][1:floor(0.2*length(a))])
  bot = mean(a[order(b)][1:floor(0.2*length(a))])
  return(top/bot)
}

# Library loading ---------------------------------------------------------
library(data.table)
library(ggplot2)
library(lubridate)
library(gbm)

# Data loading ------------------------------------------------------------
td = transactional_data
md = machine_data

# Model data creation -----------------------------------------------------
daily_sales_dt = td[,.(daily_sales = .N/uniqueN(date)),by=machine]
data = merge(daily_sales_dt,
             md,
             by='machine',all.x=T)

# Driver importance -------------------------------------------------------
vars = setdiff(names(data),c('machine','daily_sales','location_type'))

dt = data.table()
for (v in vars) {
  print(v)
  aux = data[complete.cases(eval(parse(text=v)))]
  dt = rbind(dt, data.table(var = v,
                            tbr = tbr(aux$daily_sales, aux[[v]]),
                            cor = cor(aux$daily_sales, aux[[v]])))
}

dt[, sign := ifelse(tbr<1, '-', '+')]
dt[tbr<1, tbr := 1/tbr]
dt[order(-tbr)]

# Treat variables ---------------------------------------------------------
data[,income_average:=as.numeric(income_average)]
data[,log_transport:=log10(total_number_of_routes_600)]
data[is.na(log_transport),
     log_transport:=mean(data$log_transport,na.rm=T)]
data[,isna_train_AvgDailyPassengers:=ifelse(is.na(train_AvgDailyPassengers),1,0)]
data[,no_income:=ifelse(is.na(income_average),1,0)]
data[is.na(income_average),income_average:=mean(data$income_average,na.rm=T)]

# Calculate the big machine boost with a GLM ---------------------------------
set.seed(1)
r = runif(nrow(data))
train = data[r>0.3]
test = data[r<=0.3]

model_vars = c('isna_train_AvgDailyPassengers',
               'num_hotels_45', 'log_transport',
               'num_vendex_nearby_300', 'income_average')

# We will model sales taking into account location factors but EXCLUDING small machine variable to see how do we overestimate or understimate sales because of that. The relative jump will be the boost in sales

model = glm(daily_sales~., data=train[,c(model_vars,'daily_sales'),with=F],family='gaussian')

data[,pred_sales:=predict(model,data)]
data[,.(ratio_sales_vs_benchmark = mean(daily_sales/pred_sales)),by=small_machine][,ratio_sales_vs_benchmark[small_machine==0] / ratio_sales_vs_benchmark[small_machine==1]] # Big machines sell ~20% more than small machines at a similar location

# Business case -----------------------------------------------------------
# We will calculate how much would each small machine sell if they were replaced by big machines and viceversa (how much less would big machines sell if they were replaced by a small machine). Then we will make the "swaps" pairing by impact

small_machines = data[small_machine==1]
small_machines[,sales_if_big:=daily_sales*1.2]

big_machines = data[small_machine==0]
big_machines[,sales_if_small:=daily_sales*(1/1.2)]

small_machines[,increase:=sales_if_big - daily_sales]
big_machines[,decrease:=daily_sales - sales_if_small]

# Paring small and big machines
small_machines[,.N] # 959
big_machines[,.N] # 1536
# we can make at most 959 pairings

a = big_machines[order(decrease)][,.(machine,decrease)][1:959] # ordered by machines loosing the least
b = small_machines[order(-increase)][,.(machine,increase)] # ordered by machines winning the most

c = cbind(a,b) # pairing
c[,profit:= increase - decrease]
c[,yearly_profit:=profit* (365*0.9)] # 365 days with 10% of inactive days

cost_of_moving = 500 # 500 euros of cost of both relocations
c[yearly_profit>cost_of_moving][,sum(yearly_profit)/1e3]




############assortment############

#look at the presence in transactions of specific products' categories to see which are the dominant ones

df = merge(transactional_data, product_data, by = 'product_name')
aux1 = df[type_drink_snack == 'snack']
nrow(aux1)
aux2 = df[type_drink_snack == 'drink']
nrow(aux2)

count_trans_snacks = count(aux1, 'category')
count_trans_snacks
#percentage of transactions per category:
#-Chocolate Based = 45.4%
#-Cookies, pastries and cereals = 8.5%
#-Salty = 23.4%
#-Sugar Candy = 22.7%

count_trans_drinks = count(aux2, 'category')
count_trans_drinks
#percentage of transactions per category:
#-Carbonates and energy drinks = 44.8%
#-Juice, tea and smoothies = 36.2%
#-Milk based = 9.9%
#-Water = 9.1%

#product presence --> machines with at least on transaction for each given category

df = setorderv(df, cols = c('machine', 'timestamp'))
aux = df[category == 'Chocolate based']
nrow(count(aux, 'machine')) #2488 machines
aux = df[category == 'Cookies, pastries and cereals']
nrow(count(aux, 'machine')) #2487 machines
aux = df[category == 'Salty']
nrow(count(aux, 'machine')) #2488 machines
aux = df[category == 'Sugar candy']
nrow(count(aux, 'machine')) #2488 machines
aux = df[category == 'Carbonates and energy drinks']
nrow(count(aux, 'machine')) #2490 machines
aux = df[category == 'Juice, tea and smoothies']
nrow(count(aux, 'machine')) #2495 machines
aux = df[category == 'Milk based']
nrow(count(aux, 'machine')) #2484 machines
aux = df[category == 'Water']
nrow(count(aux, 'machine')) #1664 machines

#Product efficiency --> daily sales of product category standardized by average daily sales of the type
#of product

Un_Days_Snacks <- df[type_drink_snack == 'snack', length(unique(date)), type_drink_snack]
Un_Days_Snacks
N_Trans_Snacks = count(aux1, 'type_drink_snack')
N_Trans_Snacks
aux_snacks = merge(Un_Days_Snacks, N_Trans_Snacks, by = 'type_drink_snack')
aux_snacks

Un_Days_Drinks <- df[type_drink_snack == 'drink', length(unique(date)), type_drink_snack]
Un_Days_Drinks
N_Trans_Drinks = count(aux2, 'type_drink_snack')
N_Trans_Drinks
aux_drinks = merge(Un_Days_Drinks, N_Trans_Drinks, by = 'type_drink_snack')
aux_drinks

df_snacks = merge(df, aux_snacks, by = 'type_drink_snack')
df_snacks = df_snacks[, Avg_Daily_Sales_Type:= freq/V1]
df_snacks = df_snacks[,c('V1','freq'):=NULL]
df_snacks
df_drinks = merge(df, aux_drinks, by = 'type_drink_snack')
df_dirnks = df_drinks[, Avg_Daily_Sales_Type:= freq/V1]
df_drinks = df_drinks[,c('V1','freq'):=NULL]
df_drinks

A_D_Snacks = df_snacks[, length(unique(date)), category]
A_D_Snacks
A_D_Drinks = df_drinks[, length(unique(date)), category]
A_D_Drinks
N_T_Snacks = count(df_snacks, 'category')
N_T_Snacks
N_T_Drinks = count(df_drinks, 'category')
N_T_Drinks

aux33 = merge(A_D_Snacks, N_T_Snacks, by = 'category')
aux44 = merge(A_D_Drinks, N_T_Drinks, by = 'category')
aux33 = aux33[, Avg_Daily_Sales_S_Cat:= freq/V1]
aux33 = aux33[, c('V1', 'freq'):=NULL]

aux44 = aux44[, Avg_Daily_Sales_D_Cat:= freq/V1]
aux44 = aux44[, c('V1', 'freq'):=NULL]
aux44

df_snacks = merge(df_snacks, aux33, by = 'category')
df_drinks = merge(df_drinks, aux44, by = 'category')
df_snacks
df_snacks = df_snacks[, xxx:= Avg_Daily_Sales_S_Cat/Avg_Daily_Sales_Type, category]
df_drinks = df_drinks[, xxx:= Avg_Daily_Sales_D_Cat/Avg_Daily_Sales_Type, category]
df_drinks[category == 'Water']

#Product efficiency per category for snacks:
#-Chocolate based: 45%
#-Salty: 23%
#-Sugar Candy: 23%
#-Cookies, pastries and cereals: 9%

#Product efficiency per category for drinks:
#-Carbonates and energy drinks: 45%
#-Juice, tea and smoothies: 36%
#-Milk based: 10%
#-Water: 9%

#strategy: for snacks --> "Chocolate based" and "Cookies, Pastries and Cereals" have the same product presence
#in terms of machines in which we can find these specific categories, nevertheless the product efficiency
#of the former is much higher than the latter (45% vs 9%). We could replace "Cookies, pastries and cereals" 
#entirely with "Chocolate based" products and, assuming these products' categories are completely independent
#to one another and that demand would keep up with the same distribution, we would increase our total average 
#daily sales for snacks by 36%, increasing the overall sales by 23% and the given profits by the same percentage.
#strategy: for drinks --> same by substituing "Water" entirely with "Carbonates and energy drinks".


nrow(transactional_data)

############pricing###############

#Here I wanted to see if, by increasing the price of average price of items sold per machine, I would
#have generated an increase in profits considering the decreased average daily sales. The machines 
#taken into consideration are only the ones in highly touristic areas, that is with at least 4 hotels 
#located in their areas

df = merge(t_data, product_data, by = 'product_name')
df = merge(df, machine_data, by = 'machine')
aux = df[, profit:= (price - cost), by = 'machine']
aux = df[, mean(profit), by ='machine']
df1 = merge(machine_data, aux, by = 'machine')
df1
setnames(df1, 'V1', 'Avg_Profit_Items_Sold')
df1

Active_Days <- df[, length(unique(date)), machine]
Active_Days
n_trans <- count(df, 'machine')
n_trans
aux1 <- merge(n_trans, Active_Days, by = 'machine')
df1 = merge(df1, aux1, by = "machine")
df1 = df1[, Avg_Daily_Items:=freq/V1]
df1

aux = df1[num_hotels > 3]
nrow(aux) #282 transactionsaux

machines = unique(aux$machine)
modified_a = sample(machines, round(0.5*length(machines),0), replace = F)
control_a = setdiff(machines, modified_a)

aux %>% filter(machine %in% modified_a) -> modified_a
modified_a
aux %>% filter(machine %in% control_a) -> control_a
control_a

mean(modified_a$Avg_Daily_Items) #12.2
mean(control_a$Avg_Daily_Items)  #11.22
mean(modified_a$Avg_Profit_Items_Sold) #1.94

modified_a = modified_a[, Incr_P:= 1.15*(Avg_Profit_Items_Sold)] #prices pumped up 15%
control_a                                      #control group --> no modification

mean(modified_a$Incr_P) #2.21

model_mod = lm(Avg_Daily_Items ~ location_type +
                 log(income_average) +
                 log(total_number_of_routes_600) +
                 Avg_Profit_Items_Sold,
                 data = control_a)
summary(model_mod)
mean(model_mod$fitted.values)
mean(predict(model_mod, newdata = modified_a), na.rm = TRUE) #11.82
previous_avg_profit = 1.94*12.2 #23.7
new_avg_profit = 2.23*11.8   #26.3


#by increasing the average prices at which the items are sold by 15% by each single machine in highly 
#touristic areas we generate a decrease of 3.2% in total average daily sales. Therefore, if we calculate
#the difference in average daily revenue generated per machine the previous one will be 23.7 while the 
#new one accounts for 26.3. Inidicating an increase in average profit equal to 11%. Considering that the
# sample taken into consideration (282 machines given by areas in which we have more than 3 hotels) is
# 11% of the total number of machines we can expect a total increase in profit of 1.2% taking into account
# the whole set of machines. 

########Operations####

#what we did for the alarm system, the following is the code for the previous assignment

#Import the relevant datasets 
library(tidyverse)
library(data.table)
library(dplyr)

t_data = fread(file = '~/Desktop/final_data/transactional_data.csv',
               sep = ',',
               header = TRUE)
t_data = setDT(t_data)
t_data
summary(t_data)

m_fail = fread(file = '~/Desktop/final_data/machine_failures.csv',
               sep = ',',
               header = TRUE)
m_fail = setDT(m_fail)
m_fail
summary(m_fail)



#Create the relevant variables for the model

#1

#Merge transactional data with machine failures

df = merge(t_data, m_fail, by = c('machine', 'timestamp'), all.x = T)
df
#Substitue NA values with "0"

df = df[is.na(failure), failure:=0]
df = subset(df, select = c('machine', 'timestamp','date', 'product_name', 'failure', 'column.x'))
df = setnames(df, 'column.x', 'column')

#2

#order transactions by machine and date

df = setorderv(df, cols= c('machine', 'date'))

#create variable "last_vend" (timestamp of last sale)

df = df[, last_vend:=shift(timestamp, n = 1L), by = machine]

#3

#create variable "deltahours" (for every transaction contains the hours which passed since the last sale)

df = df[, deltahours:= difftime(timestamp, last_vend, units = 'hours'), by = machine]

#4 

#create "machine_daily_average" (contains average daily sales per machine)

#count transactions per machine (unique)

trans_per_m = count(df,'machine')

#count unique days each machine has been active

days_active = df[, length(unique(date)), machine]

#set data table to merge to the original one with all the necessary transformations (aux1 = auxiliary data table)

aux1 = merge(trans_per_m, days_active, by = 'machine')
aux1 = setDT(aux1)
aux1 = aux1[, machine_daily_average:= freq/V1]
aux1 = subset(aux1, select = c('machine', 'machine_daily_average'))

#merge to original data table

df = merge(df, aux1, by = 'machine')

df = subset(df, select = c('machine', 'timestamp', 'date', 'product_name', 'failure', 
                           'column', 'last_vend', 'deltahours', 'machine_daily_average'))

#5

#create new variable called "delta" (normalized "deltahours")

df = df[, delta:= .(deltahours/(24/machine_daily_average))]


#Creating the model

#6 

#select machines for train and test sets

machines = unique(df$machine)
m_train = sample(machines, round(0.7*length(machines),0), replace = F)
m_test = setdiff(machines, m_train)

#create train and test sets

df %>% filter(machine %in% m_train) -> train_set
train_set
df %>% filter(machine %in% m_test) -> test_set
test_set

#model

m = glm(failure ~ delta, data = train_set, family = 'binomial')
summary(m)
m$coefficients
m_fitted = predict(m, newdata = train_set, type = 'response')

#Insight: the value for the intercept is equal to -6.91 while the value for the coefficient
#associated with delta is equal to 0.56

m_test = glm(failure ~ delta, data = test_set, family = 'binomial')
summary(m_test)
m_test$coefficients
m_fitted_test = predict(m, newdata = test_set, type = 'response')

#Insight: the value for the intercept is equal to -6.9 while the value for the coefficient 
#associated with delta is equal to 0.56

#questions

#a

roc(response=train_set$failure, predictor = m_fitted,
    auc = TRUE, plot = TRUE, col = 'red', 
    legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Positive Percentage")

roc(response=test_set$failure, predictor = m_fitted_test,
    auc = TRUE, plot = TRUE, col = 'purple',
    legacy.axes=TRUE, percent=TRUE,
    xlab="False Positive Percentage", ylab="True Positive Percentage")


#the value of the AUC for the model built on the train set is equal to 0.9191, while 
#the value for the AUC built on the test set is equal to 0.9219

#b 

#plot the function of probability of failure vs delta

m_int = m$coefficients['(Intercept)']
m_delta_coeff = m$coefficients['delta']

x = train_set$delta
curve(1/(1 + exp(-(m_int + m_delta_coeff*x))), from = 0, to = 30, xlab = 'delta', ylab = 'Logistic Function', col = 'red')

#c

#1

f = function(x){-0.6 + 1 / (1 + exp(-(m_int + m_delta_coeff*x)))}
delta_threshold_0.6 = uniroot(f, c(0,20))$root  # 13.04

f = function(x){-0.8 + 1 / (1 + exp(-(m_int + m_delta_coeff*x)))}
delta_threshold_0.8 = uniroot(f, c(0,20))$root  # 14.79

#the threshold for 60% is equal to 13.04 hours while the threshold for 80% is equal to 14.79 hours.

#2

#count number of unique days

length(unique(train_set$date)) #90
length(unique(test_set$date)) #90

#number of med-risk alarm per day

med_per_day = length(df[delta > delta_threshold_0.6]$delta)/90
med_per_day #41.18
high_per_day = length(df[delta > delta_threshold_0.8]$delta)/90
high_per_day #27.7

#3

#get observations for which we expect high-risk and med-risk alarm to be activated

high_risk_alarms = df[delta > delta_threshold_0.8]
med_risk_alarms = df[delta > delta_threshold_0.6]

#find % of false alarms per level of priority

High_False_Alarms = length(high_risk_alarms[failure == 0]$failure)
High_False_Alarms #222
High_False_Alarms_Perc = (High_False_Alarms/length(high_risk_alarms$column))*100
High_False_Alarms_Perc #the percentage of false alarms for the high-risk priority is equal to 8.9%

Med_False_Alarms = length(med_risk_alarms[failure == 0]$failure)
Med_False_Alarms #750
Med_False_Alarms_Perc = (Med_False_Alarms/length(med_risk_alarms$column))*100
Med_False_Alarms_Perc #the percentage of false alarms for the med-risk priority is equal to 20.23%

#d

#for med-risk alarms 

df1 = df[delta > delta_threshold_0.6]
df1 = df1[, threshold_hours:= (delta_threshold_0.6*(24/machine_daily_average))]
df1 = df1[, delta_fixed:= (threshold_hours+1.5)*(1/(24/machine_daily_average))]
df1 = df1[, won_sales:= failure*(delta - delta_fixed)]
df1 = df1[, additional_revenues:=won_sales*1.7]
times_operator_sent = length(df1[failure == 0]$failure)
times_operator_sent
total_cost = Med_False_Alarms*10
total_add_rev = sum(df1$additional_revenues)
profit_increase = total_add_rev - total_cost
profit_increase
perc_profit_med_risk = (profit_increase/(1.7*length(df$column)))*100
perc_profit_med_risk

#the percentage increase in profit by activating our system only on medium-risk alarms would be 
#equal to 1.96% with respect to the profit made on all transactions in the given period of 90 days

#for high-risk alarms

df2 = df[delta > high_risk_alarms]
df2 = df2[, threshold_hours:= (high_risk_alarms*(24/machine_daily_average))]
df2 = df2[, delta_fixed:= (threshold_hours+1.5)*(1/(24/machine_daily_average))]
df2 = df2[, won_sales:= failure*(delta - delta_fixed)]
df2 = df2[, additional_revenues:=won_sales*1.7]
df2
times_operator_sent = length(df2[failure == 0]$failure)
times_operator_sent
total_cost = High_False_Alarms*10
total_add_rev = sum(df2$additional_revenues)
profit_increase = total_add_rev - total_cost
profit_increase
perc_profit_high_risk = (profit_increase/(1.7*length(df$column)))*100
perc_profit_high_risk

#the percentage increase in profit by activating our system only on high-risk alarms would be 
#equal to 2.13% with respect to the profit made on all transactions in the given period of 90 days







