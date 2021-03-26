library(DataExplorer)
library(pscl)
library(dominanceanalysis)
library(glmnet)
library(randomForest)
library(xgboost)
library(gbm)
library(varImp)
library(caret)
library(doParallel)
library(RankAggreg)
library(grid)
library(gridExtra)
library(OpenMPController) # for Kaggle backend
library(dplyr)
library(ggsci)
#Load the data
sample_data<-read.csv("classfication.csv")

#Splite the data
inTraining <- createDataPartition(sample_data$Outcome, p = .8, list = FALSE)
training <- sample_data[ inTraining,]
testing  <- sample_data[-inTraining,]
# Set up parallel computation
omp_set_num_threads(6) # caret parallel processing threads

# Set up grid search
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,allowParallel = TRUE)

randomGrid <- expand.grid(.mtry=(1:8))

gbmFit1 <- train(Outcome ~ ., data = training, 
                 method='rf',
                 trControl = fitControl,
                 metric='Accuracy', 
                 
                 tuneGrid=randomGrid)
plot(gbmFit1)


# fit the best model
rf_final<-train(Outcome~., data = training, 
                method='rf', 
                metric='Accuracy', 
                tuneGrid=gbmFit1$bestTune)

# Feature Importance 
rf_imp <- varImp(rf_final, scale = FALSE)
imp_plot<-rf_imp$importance%>%as.data.frame()
imp_plot$Variables<-rownames(imp_plot) 
imp_plot<-imp_plot%>%arrange(desc(imp_plot$Overall))
imp_plot$Variables[1:5]<-c("Age", "Allergies", "Location(SEN)", "Manufacturer","Sex")
imp_plot<-imp_plot[1:5,]
colnames(imp_plot)[1]<-"Importance Measure"
imp_plot$Variables<-factor(imp_plot$Variables,levels = c("Sex","Manufacturer","Location(SEN)","Allergies","Age"))
p10<-ggplot(imp_plot, aes(x=Variables, y=`Importance Measure`,fill=Variables)) + 
  geom_bar(stat="identity",width = 0.5)+
  coord_flip() +
  theme(axis.text=element_text(size=30),axis.title=element_text(size=20))+
  scale_color_jama()+
  scale_fill_jama() +
  ggtitle("Importance Ranking of Random Forest(Top 5)")

