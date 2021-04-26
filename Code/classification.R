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
sample_data<-read.csv("classification_14.csv")[,-1]
sample_data<-sample_data[sample_data$Outcomes!="Death",]
# label_test<-read.csv("test_id.csv")
# label_train<-read.csv("train_id.csv")
# sample_data$Outcomes[sample_data$Outcomes=="Death" ]<-"Serious" 
sample_data[,4:ncol(sample_data)]<-lapply(sample_data[,4:ncol(sample_data)], factor) 
set.seed(1234)

#Splite the data
inTraining <- createDataPartition(sample_data$Outcome, p = .8, list = FALSE)
training <- sample_data[ inTraining,] [,-1]
testing  <- sample_data[-inTraining,] [,-1]

# label<-training%>%select(VAERS_ID)
# label$LABEL<-"Train"
# tep<-testing%>%select(VAERS_ID)
# tep$LABEL<-"Test"
# label<-rbind(label,tep)
# write.csv(label,"Train_test_label_14.csv")
# training <- sample_data[ sample_data$VAERS_ID%in%label_train$X9.400590000000000000e.05,][-1]
# testing  <- sample_data[sample_data$VAERS_ID%in%label_test$X1.088112000000000000e.06,][,-1]
set.seed(1234)
# Set up parallel computation
omp_set_num_threads(8) # caret parallel processing threads

# Set up grid search
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,allowParallel = TRUE)

randomGrid <- expand.grid(.mtry=(1:8))

gbmFit1 <- train(Outcomes ~ ., data = training, 
                 method='rf',
                 trControl = fitControl,
                 metric='Accuracy', 
                 
                 tuneGrid=randomGrid)
plot(gbmFit1)


# fit the best model
rf_final<-train(Outcomes~., data = training, 
                method='rf', 
                metric='Accuracy', 
                tuneGrid=gbmFit1$bestTune)

confusionMatrix(data = predict(rf_final, newdata = testing), reference = testing$Outcomes,positive ="Serious")
# Feature Importance 
rf_imp <- varImp(rf_final, scale = T)
plot(rf_imp)
imp_plot<-rf_imp$importance%>%as.data.frame()
imp_plot$Variables<-rownames(imp_plot) 
imp_plot<-imp_plot%>%arrange(desc(imp_plot$Overall))
imp_plot$Variables[1:5]<-c("Age", "Num_days", "Allergies","Sex(Male)", "Location(PVT)")
imp_plot<-imp_plot[1:5,]
colnames(imp_plot)[1]<-"Importance Measure"
imp_plot$Variables<-factor(imp_plot$Variables,levels = c("Location(PVT)","Sex(Male)","Allergies","Num_days","Age"))
p10<-ggplot(imp_plot, aes(x=Variables, y=`Importance Measure`,fill=Variables)) + 
  geom_bar(stat="identity",width = 0.5)+
  coord_flip() +
  #theme(axis.text=element_text(size=30),axis.title=element_text(size=30))+
  scale_color_jama()+
  scale_fill_jama() +
  ggtitle("Importance Ranking of Random Forest(Top 5)")


ggarrange(ggarrange(p10,p10, ncol = 2,labels = c("A","B")) ,ggarrange(p10,p10, ncol = 2,labels =  c("C","D")),
          ggarrange(p10,p10, ncol = 2,labels =  c("E","F")),
          nrow = 3
)






p10
write.csv(imp_plot,"imp_plot45.csv")
