# assignment 3 predictive maintenance

library("dplyr")
library("xgboost")
library("randomForest")
require(caTools)

main_path = "C:\\Users\\micho\\OneDrive\\UNI\\HEC\\Fall Term 2021\\statistical learning MATH 60603\\assignments\\Assignment 3 predictive maintenance\\DataNew\\"

repairs = read.csv(paste(main_path,"repairs.csv",sep=""))
sensors_study = read.csv(paste(main_path,"sensors-study.csv",sep=""))
sensors_score = read.csv(paste(main_path,"sensors-score.csv",sep=""))

str(repairs)
str(sensors_study)
str(sensors_score)

head(repairs)
sensors_study[2000:2100,]
head(sensors_score)

# identify which pumps
unique(sensors_study$Month) # we have 12 months
unique(sensors_study$ID)

# some observations
barplot(prop.table(table(repairs %>% filter(is.na(Bill6))%>%select(Bill12))))

# uplift model on the yearly profits done with and without month 6 maintenance
# projected yearly profits if i maintain and if i did not based on data from the first 5 months
df = as.data.frame(matrix(nrow=200,ncol=1))
colnames(df) = c("ID")
df$ID = c(1:200)

for(mon in (1:12)){
  for(lable in c("Volume","Energy","PSD500","PSD1000","PSD1500","PSD2000","PSD2500","PSD3000")){
    df[[paste(mon,lable,sep="_")]] = sensors_study %>% select(Month,lable) %>% filter( Month==mon) %>% select(lable)
  }
}

df1 = as.data.frame(matrix(nrow=200,ncol=1))
colnames(df1) = c("ID")
df1$ID = c(1:200)
df1 = merge(df1,repairs %>% select(ID,Cost6,Cost12),by = "ID")
df1$Cost6 = ifelse(is.na(df1$Cost6),0,df1$Cost6)

df1[["Energy"]] = tapply(sensors_study$Energy,sensors_study$ID,FUN=sum)
df1[["Volume"]] = tapply(sensors_study$Volume,sensors_study$ID,FUN=sum)
#df1[["Yr_profits"]] = df1$Volume*0.03-(df1$Energy*0.1+df1$Cost6+df1$Cost12)
df1[["Yr_profits"]] = df1$Volume/df1$Energy


dframe = as.data.frame(matrix(nrow=200,ncol=length(df)))
colnames(dframe) = names(df)
dframe[["ID"]] = df[["ID"]]

for (col in c(2:length(names(dframe)))){
  dframe[[col]] = as.numeric(unlist(df[col]))
}


df=dframe    
    
df = df %>% select(names(df)[1:41])
df = merge(df,df1%>%select(ID,Yr_profits),by = "ID")
df[["Yr_profits"]] = as.numeric(unlist(df$Yr_profits))

df_repaired = df[1:100,]
df_notrepaired = df[101:200,]

ID_train_repaired = sample(df_repaired$ID,90)
ID_val_repaired = subset(df_repaired$ID, !(df_repaired$ID %in% ID_train_repaired))
ID_train_notrepaired = sample(df_notrepaired$ID,90)
ID_val_notrepaired = subset(df_notrepaired$ID, !(df_notrepaired$ID %in% ID_train_notrepaired))

df_repaired_train = df_repaired %>% filter(ID %in% ID_train_repaired)
df_repaired_val = df_repaired %>% filter(ID %in% ID_val_repaired)
df_notrepaired_train = df_notrepaired %>% filter(ID %in% ID_train_notrepaired)
df_notrepaired_val = df_notrepaired %>% filter(ID %in% ID_val_notrepaired)

#plot(cbind(1:12),unlist(sensors_study %>% filter(ID == 2) %>% select(Energy)),'l')

# fit models
# XGBOOST
m1_repaired =xgboost(
    data = as.matrix(df_repaired_train %>% select(-ID,-Yr_profits)),
    label = df_repaired_train$Yr_profits,
    nrounds = 1000,
    objective = "reg:squarederror",
    early_stopping_rounds = 3,
    max_depth = 6,
    eta = .25
  )   

m1_notrepaired = xgboost(
  data = as.matrix(df_notrepaired_train %>% select(-ID,-Yr_profits)),
  label = df_notrepaired_train$Yr_profits,
  nrounds = 1000,
  objective = "reg:squarederror",
  early_stopping_rounds = 3,
  max_depth = 6,
  eta = .25
)   

prediction_repaired = predict(m1_repaired,df_repaired_val %>% select(-ID,-Yr_profits)%>%as.matrix())
rmse_repaired_m1 = sqrt(mean((prediction_repaired - df_repaired_val$Yr_profits)^2))

prediction_notrepaired = predict(m1_notrepaired,df_notrepaired_val %>% select(-ID,-Yr_profits)%>%as.matrix())
rmse_notrepaired_m1 = sqrt(mean((prediction_notrepaired - df_notrepaired_val$Yr_profits)^2))

#xgb.importance(feature_names = names(df_notrepaired_train %>% select(-ID,-Yr_profits)),model = m1_repaired)

# stepwise model
#model_repaired = glm(Yr_profits~., family = "gaussian",data = df_repaired_train%>%select(-ID))
model_repaired = lm(Yr_profits~.,data = df_repaired_train%>%select(-ID))
step_model_repaired = step(model_repaired,trace=F)
prediction_repaired = predict(step_model_repaired,df_repaired_val %>% select(-ID,-Yr_profits))
rmse_repaired_m2 = sqrt(mean((prediction_repaired - df_repaired_val$Yr_profits)^2)) #this is better

model_notrepaired = lm(Yr_profits~.,data = df_notrepaired_train%>%select(-ID))
step_model_notrepaired = step(model_notrepaired,trace=F)
prediction_notrepaired = predict(step_model_notrepaired,df_notrepaired_val %>% select(-ID,-Yr_profits))
rmse_notrepaired_m2 = sqrt(mean((prediction_notrepaired - df_notrepaired_val$Yr_profits)^2)) #this is better

# random forest
x_repaired = df_repaired_train%>%select(-ID, -Yr_profits)
y_repaired = as.vector(df_repaired_train$Yr_profits)
rf_repaired = randomForest(x = x_repaired,y = y_repaired, ntree=100)
prediction_repaired = predict(rf_repaired,df_repaired_val %>% select(-ID,-Yr_profits))
rmse_repaired_m3 = sqrt(mean((prediction_repaired - df_repaired_val$Yr_profits)^2))

x_notrepaired = df_notrepaired_train%>%select(-ID, -Yr_profits)
y_notrepaired = as.vector(df_notrepaired_train$Yr_profits)
rf_notrepaired = randomForest(x = x_notrepaired, y=y_notrepaired, ntree=100)
prediction_notrepaired = predict(rf_notrepaired,df_notrepaired_val %>% select(-ID,-Yr_profits))
rmse_notrepaired_m3 = sqrt(mean((prediction_notrepaired - df_notrepaired_val$Yr_profits)^2))

 best_repaired = rf_repaired
 best_notrepaired = step_model_notrepaired

# treat the complete database
df_real = as.data.frame(matrix(nrow=length(unique(sensors_score$ID)),ncol=1))
colnames(df_real) = c("ID")
df_real$ID = unique(sensors_score$ID)

for(mon in unique(sensors_score$Month)){
  for(lable in c("Volume","Energy","PSD500","PSD1000","PSD1500","PSD2000","PSD2500","PSD3000")){
    df_real[[paste(mon,lable,sep="_")]] = sensors_score %>% select(Month,lable) %>% filter(Month==mon) %>% select(lable)
  }
}

dframe_real = as.data.frame(matrix(nrow=length(df_real$ID),ncol=length(df_real)))
colnames(dframe_real)=names(df_real)
dframe_real[['ID']]=df_real[["ID"]]
names = names(df_real %>%select(-ID))
for(col in c(2:length(names(dframe_real)))){
  dframe_real[[col]] = as.numeric(unlist(df_real[col]))
}
df_real = dframe_real

# Uplift
uplift = predict(best_repaired,df_real %>% select(-ID)) - predict(best_notrepaired,df_real %>% select(-ID))
df_real[["uplift"]] = uplift

df_real = df_real %>% arrange(desc(uplift))

# threshold
thr = 7000
ID_useful = df_real$ID[which(uplift > thr)]

# first 20k
ordered_uplift = df_real %>% arrange(desc(uplift))
ID_useful = ordered_uplift$ID[1:20001]

#length(ID_useful)
path_export = "C:\\Users\\micho\\OneDrive\\UNI\\HEC\\Fall Term 2021\\statistical learning MATH 60603\\assignments\\Assignment 3 predictive maintenance\\testf4.csv"
write.csv(ID_useful,path_export,row.names = FALSE)

#---------------------------------------------Classification---------------------------------------------------------------
id=60
y_rep = sensors_study %>% select(ID, Energy) %>% filter(ID==id)%>% select(Energy)
y_rep = as.numeric(unlist(y_rep))

y_not = sensors_study %>% select(ID, Energy) %>% filter(ID==id+100)%>% select(Energy)
y_not = as.numeric(unlist(y_not))

par(mfrow=c(2,1))
plot(c(1:12),y_rep,'l')
plot(c(1:12),y_not,'l')
hist(df$Yr_profits)
