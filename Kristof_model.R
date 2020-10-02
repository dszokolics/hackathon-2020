library(tidyverse)
library(ROCR)
library(ranger)
library(data.table)

setwd("G:/.shortcut-targets-by-id/0B23Ot7AW9q8dTHJCSU1MT1A2SzQ/Common/Homokozó/nlp-hackathon")


data <- readRDS("data/transformed/final_ABTs_2.RDS")
View(data)

df1 <- data.frame(data$`2020-06-01`)
df2 <- data.frame(data$`2020-07-01`)

View(df1)

df1$X <- NULL
df1$by <- NULL
df1$year_month <- NULL
df1$last_activity <- NULL
df1$time <- NULL

str(df1)
df1$days_from_last_activity <- as.numeric(df1$days_from_last_activity)
df1$days_from_last_comment <- as.numeric(df1$days_from_last_comment)
df1$days_from_last_post <- as.numeric(df1$days_from_last_post)


df2$X <- NULL
df2$by <- NULL
df2$year_month <- NULL
df2$last_activity <- NULL
df2$time <- NULL

str(df2)
df2$days_from_last_activity <- as.numeric(df2$days_from_last_activity)
df2$days_from_last_comment <- as.numeric(df2$days_from_last_comment)
df2$days_from_last_post <- as.numeric(df2$days_from_last_post)

glm(churn ~., data = df1)


library(caret)
library(e1071)
library(corrplot)

#df1[,!sapply(df1, function(x) { sd(x) == 0} )]

df1 <- df1[ - as.numeric(which(apply(df1, 2, var) == 0))]
df1 <- na.omit(df1)

df2 <- df2[ - as.numeric(which(apply(df2, 2, var) == 0))]
df2 <- na.omit(df2)
png('corrplot.png', width = 1500, height = 1000, res = 150)
corrplot(cor(df1), tl.cex = 0.5, method = 'color', tl.srt = 90, tl.col = 'black',
         type = 'full', diag = T, number.cex = 0.3, number.font = 1/10, addCoef.col = "black", cl.cex = 0.5,
         col=colorRampPalette(c("blue3","white","darkred"))(50), cl.pos = 'r') 
dev.off()


# model
df1$churn <- as.factor(as.numeric(df1$churn))
df2$churn <- as.factor(as.numeric(df2$churn))


logit <- train(churn ~ ., data = df1, 
               method="glm", 
               family = binomial(), 
               preProcess = c("center", "scale"))

logit$finalModel

# on train (df1) eval
pred_train <- prediction(predict(logit, df1, 'prob')[2], df1$churn)
perf_train <- performance(pred_train,"tnr","fnr")
plot(perf_train,col="red", main="ROC curve on our train and test datasets")
abline(a = 0, b = 1)
auc_ROCR_train <- performance(pred_train, measure = "auc")
auc_ROCR_train <- auc_ROCR_train@y.values[[1]]
auc_ROCR_train
text(x = 0.125, y = 0.9, labels = 'Train AUC: 0.7901', col = 'red')


# on test (df2) eval
pred <- prediction(predict(logit, df2, 'prob')[2], df2$churn)
perf <- performance(pred,"tnr","fnr")
par(new=TRUE)
plot(perf,col="blue")
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR 
text(x = 0.125, y = 0.82, labels = 'Test AUC: 0.7799', col = 'blue')


# conf matrix
?performance
confusionMatrix(factor(ifelse(predict(logit, df2, 'prob')[2] >= 0.5, 1, 0)),
                df2$churn)

?varImp

#############################################################################
############################# GEROLD BENCE INNEN ############################
#############################################################################
varimplog <- varImp(logit)$importance
varimplog$feature <- rownames(varimplog)

varimplog <- varimplog %>% arrange(desc(Overall)) 
varimplog %>% top_n(10, wt = Overall) %>% ggplot(aes(reorder(feature,-Overall),Overall)) +
  geom_col(color = 'black', fill = 'gray') +
  theme_bw() +
  labs(title = 'Logistic regression most imporant features',
       x = 'Feature', y = 'Importance') +
theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))














# RF

# my tuneGrid object:
tgrid <- expand.grid(
  .mtry = c(7, 9, 11, 13),
  .splitrule = 'gini',
  .min.node.size = c(10, 30, 40, 50)
)
df1$churn <- ifelse(df1$churn == 1, 'churn', 'stay')
df1$churn <- factor(df1$churn)
rf <- train(churn  ~ ., data = df1,
                     method = "ranger",
                     trControl = trainControl(method="cv", number = 5, verboseIter = T, classProbs = T),
                     tuneGrid = tgrid,
            
                     importance = 'impurity',
            num.trees = 100,
            preProcess = c("center", "scale")
)



rf$finalModel

pred <- prediction(predict(rf, df1, 'prob')[2], ifelse(as.numeric(df1$churn) == 2, 1, 0))
perf <- performance(pred,"tpr","fpr")
plot(perf,col="red", main="ROC Curve")
abline(a = 0, b = 1)
auc_ROCR <- performance(pred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
text(x = 0.05, y = 0.9, labels = 'AUC: 0.9083')
auc_ROCR 

varimplog <- varImp(rf)$importance
varimplog$feature <- rownames(varimplog)

varimplog <- varimplog %>% arrange(desc(Overall)) 
varimplog %>% top_n(10, wt = Overall) %>% ggplot(aes(reorder(feature,-Overall),Overall)) +
  geom_col(color = 'black', fill = 'gray') +
  theme_bw() +
  labs(title = 'Random forest most imporant features',
       x = 'Feature', y = 'Importance') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


table(df1$churn)
3241/nrow(df1)
