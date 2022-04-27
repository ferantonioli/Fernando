df = read.csv("C:/Users/fero9/OneDrive/Desktop/Babson/Fall/Machine Learning for Business/Project/train.csv")
source('C:/Users/fero9/OneDrive/Desktop/Babson/Fall/Machine Learning for Business/Data/BabsonAnalytics.R')

df$Id = NULL

#################################################################################################################
#KNN Original and Crossval

#Libraries
library(gtable)
library(grid)
library(gridExtra)
library(caret)

#Manage Data
df_knn = df
df_knn$Cover_Type = as.factor(df_knn$Cover_Type)
df_knn$Soil_Type1 = NULL
df_knn$Soil_Type2 = NULL
df_knn$Soil_Type3 = NULL
df_knn$Soil_Type4 = NULL
df_knn$Soil_Type5 = NULL
df_knn$Soil_Type6 = NULL
df_knn$Soil_Type7 = NULL
df_knn$Soil_Type8 = NULL
df_knn$Soil_Type9 = NULL
df_knn$Soil_Type10 = NULL
df_knn$Soil_Type11 = NULL
df_knn$Soil_Type12 = NULL
df_knn$Soil_Type13 = NULL
df_knn$Soil_Type14 = NULL
df_knn$Soil_Type15 = NULL
df_knn$Soil_Type16 = NULL
df_knn$Soil_Type17 = NULL
df_knn$Soil_Type18 = NULL
df_knn$Soil_Type19 = NULL
df_knn$Soil_Type20 = NULL
df_knn$Soil_Type21 = NULL
df_knn$Soil_Type22 = NULL
df_knn$Soil_Type23 = NULL
df_knn$Soil_Type24 = NULL
df_knn$Soil_Type25 = NULL
df_knn$Soil_Type26 = NULL
df_knn$Soil_Type27 = NULL
df_knn$Soil_Type28 = NULL
df_knn$Soil_Type29 = NULL
df_knn$Soil_Type30 = NULL
df_knn$Soil_Type31 = NULL
df_knn$Soil_Type32 = NULL
df_knn$Soil_Type33 = NULL
df_knn$Soil_Type34 = NULL
df_knn$Soil_Type35 = NULL
df_knn$Soil_Type36 = NULL
df_knn$Soil_Type37 = NULL
df_knn$Soil_Type38 = NULL
df_knn$Soil_Type39 = NULL
df_knn$Soil_Type40 = NULL
df_knn$Wilderness_Area1 = NULL
df_knn$Wilderness_Area2 = NULL
df_knn$Wilderness_Area3 = NULL
df_knn$Wilderness_Area4 = NULL

#Partition Data
N = nrow(df_knn)
set.seed(1234)
trainingSize = round(N*0.6)
trainingSample = sample(N, trainingSize)
trainingTotal = df_knn[trainingSample, ]
testTotal = df_knn[-trainingSample, ]

#Build Model
model_knn = knn3(Cover_Type ~ ., data=trainingTotal)

#Prediction
prediction_knn = predict(model_knn, testTotal, type = "class")

#Table in R
observation = testTotal$Cover_Type
table(prediction_knn, observation)

#Table Pretty
a = table(prediction_knn, observation)

table = tableGrob(a)

title = textGrob("Observations",gp=gpar(fontsize=12))
footnote = textGrob("1 = Spruce/Fir
2 = Lodgepole Pine
3 = Ponderosa Pine
4 = Cottonwood/Willow
5 = Aspen
6 = Douglas-fir
7 = Krummholz", x=0, hjust=0,
                    gp=gpar( fontface="italic",fontsize = 8))

padding = unit(1,"line")
table = gtable_add_rows(table, 
                        heights = grobHeight(title) + padding,
                        pos = 0)
table = gtable_add_rows(table, 
                        heights = grobHeight(footnote)+ padding)
table = gtable_add_grob(table, list(title, footnote),
                        t=c(1, nrow(table)), l=c(1,2), 
                        r=ncol(table))

dev.new()
grid.newpage()
grid.draw(table)

draw.text.rot = function(t,just, i, j,rot,x=.45,y=.500) {
  grid.text(t, x=x, y=y, just=just,rot=90,gp=gpar(fontsize=12))
}

title2 = draw.text.rot("Predictions",c("left"),2,2,x=.22,y=.475)

#Error Original
error_knn = sum(prediction_knn != observation)/nrow(testTotal)

#Build Model Crossval
CrossVal = kNNCrossVal(Cover_Type ~., trainingTotal)
model_knn_crossval = knn3(Cover_Type ~ ., data=trainingTotal, k = CrossVal)

#Prediction Crossval
prediction_knn_crossval = predict(model_knn_crossval, testTotal, type = "class")

#Error Crossval
observation = testTotal$Cover_Type
error_knn_crossval = sum(prediction_knn_crossval != observation)/nrow(testTotal)
error_bench = benchmarkErrorRate(trainingTotal$Cover_Type, testTotal$Cover_Type)

#Table Crossval
b = table(prediction_knn_crossval, observation)

table = tableGrob(b)

title = textGrob("Observations",gp=gpar(fontsize=12))
footnote = textGrob("1 = Spruce/Fir
2 = Lodgepole Pine
3 = Ponderosa Pine
4 = Cottonwood/Willow
5 = Aspen
6 = Douglas-fir
7 = Krummholz", x=0, hjust=0,
                    gp=gpar( fontface="italic",fontsize = 8))

padding = unit(1,"line")
table = gtable_add_rows(table, 
                        heights = grobHeight(title) + padding,
                        pos = 0)
table = gtable_add_rows(table, 
                        heights = grobHeight(footnote)+ padding)
table = gtable_add_grob(table, list(title, footnote),
                        t=c(1, nrow(table)), l=c(1,2), 
                        r=ncol(table))

dev.new()
grid.newpage()
grid.draw(table)

draw.text.rot = function(t,just, i, j,rot,x=.45,y=.500) {
  grid.text(t, x=x, y=y, just=just,rot=90,gp=gpar(fontsize=12))
}

title2 = draw.text.rot("Predictions_CrossVal",c("left"),2,2,x=.23,y=.425)

#################################################################################################################
#KNN Standardizer

#Manage Data
df_standardized = df
df_standardized$Cover_Type = as.factor(df_standardized$Cover_Type)
df_standardized$Id = NULL
df_standardized$Soil_Type1 = NULL
df_standardized$Soil_Type2 = NULL
df_standardized$Soil_Type3 = NULL
df_standardized$Soil_Type4 = NULL
df_standardized$Soil_Type5 = NULL
df_standardized$Soil_Type6 = NULL
df_standardized$Soil_Type7 = NULL
df_standardized$Soil_Type8 = NULL
df_standardized$Soil_Type9 = NULL
df_standardized$Soil_Type10 = NULL
df_standardized$Soil_Type11 = NULL
df_standardized$Soil_Type12 = NULL
df_standardized$Soil_Type13 = NULL
df_standardized$Soil_Type14 = NULL
df_standardized$Soil_Type15 = NULL
df_standardized$Soil_Type16 = NULL
df_standardized$Soil_Type17 = NULL
df_standardized$Soil_Type18 = NULL
df_standardized$Soil_Type19 = NULL
df_standardized$Soil_Type20 = NULL
df_standardized$Soil_Type21 = NULL
df_standardized$Soil_Type22 = NULL
df_standardized$Soil_Type23 = NULL
df_standardized$Soil_Type24 = NULL
df_standardized$Soil_Type25 = NULL
df_standardized$Soil_Type26 = NULL
df_standardized$Soil_Type27 = NULL
df_standardized$Soil_Type28 = NULL
df_standardized$Soil_Type29 = NULL
df_standardized$Soil_Type30 = NULL
df_standardized$Soil_Type31 = NULL
df_standardized$Soil_Type32 = NULL
df_standardized$Soil_Type33 = NULL
df_standardized$Soil_Type34 = NULL
df_standardized$Soil_Type35 = NULL
df_standardized$Soil_Type36 = NULL
df_standardized$Soil_Type37 = NULL
df_standardized$Soil_Type38 = NULL
df_standardized$Soil_Type39 = NULL
df_standardized$Soil_Type40 = NULL
df_standardized$Wilderness_Area1 = NULL
df_standardized$Wilderness_Area2 = NULL
df_standardized$Wilderness_Area3 = NULL
df_standardized$Wilderness_Area4 = NULL

standardizer = preProcess(df_standardized, c("center", "scale"))
df_standardized = predict(standardizer, df_standardized)

#Partition Data
N = nrow(df_standardized)
set.seed(1234)
trainingSize = round(N*0.6)
trainingSample = sample(N, trainingSize)
trainingTotal = df_standardized[trainingSample, ]
testTotal = df_standardized[-trainingSample, ]

#Build Model
model_standardized = knn3(Cover_Type ~ ., data=trainingTotal)

#Make prediction
prediction_standardized = predict(model_standardized, testTotal, type = "class")

#Calculate error
observation = testTotal$Cover_Type
table(prediction_standardized, observation)
error_knn_standardized = sum(prediction_standardized != observation)/nrow(testTotal)

#Table Pretty
c = table(prediction_standardized, observation)

table = tableGrob(c)

title = textGrob("Observations",gp=gpar(fontsize=12))
footnote = textGrob("1 = Spruce/Fir
2 = Lodgepole Pine
3 = Ponderosa Pine
4 = Cottonwood/Willow
5 = Aspen
6 = Douglas-fir
7 = Krummholz", x=0, hjust=0,
                    gp=gpar( fontface="italic",fontsize = 8))

padding = unit(1,"line")
table = gtable_add_rows(table, 
                        heights = grobHeight(title) + padding,
                        pos = 0)
table = gtable_add_rows(table, 
                        heights = grobHeight(footnote)+ padding)
table = gtable_add_grob(table, list(title, footnote),
                        t=c(1, nrow(table)), l=c(1,2), 
                        r=ncol(table))

dev.new()
grid.newpage()
grid.draw(table)

draw.text.rot = function(t,just, i, j,rot,x=.45,y=.500) {
  grid.text(t, x=x, y=y, just=just,rot=90,gp=gpar(fontsize=12))
}

title2 = draw.text.rot("Predictions_Standardized",c("left"),2,2,x=.22,y=.375)

#################################################################################################################
#Classification Tree

#Libraries
library(rpart)
library(rpart.plot)

#Manage Data
df_ct = df
df_ct$Cover_Type = as.factor(df_ct$Cover_Type)
df_ct$Soil_Type1 = as.logical(df_ct$Soil_Type1)
df_ct$Soil_Type2 = as.logical(df_ct$Soil_Type2)
df_ct$Soil_Type3 = as.logical(df_ct$Soil_Type3)
df_ct$Soil_Type4 = as.logical(df_ct$Soil_Type4)
df_ct$Soil_Type5 = as.logical(df_ct$Soil_Type5)
df_ct$Soil_Type6 = as.logical(df_ct$Soil_Type6)
df_ct$Soil_Type7 = as.logical(df_ct$Soil_Type7)
df_ct$Soil_Type8 = as.logical(df_ct$Soil_Type8)
df_ct$Soil_Type9 = as.logical(df_ct$Soil_Type9)
df_ct$Soil_Type10 = as.logical(df_ct$Soil_Type10)
df_ct$Soil_Type11 = as.logical(df_ct$Soil_Type11)
df_ct$Soil_Type12 = as.logical(df_ct$Soil_Type12)
df_ct$Soil_Type13 = as.logical(df_ct$Soil_Type13)
df_ct$Soil_Type14 = as.logical(df_ct$Soil_Type14)
df_ct$Soil_Type15 = as.logical(df_ct$Soil_Type15)
df_ct$Soil_Type16 = as.logical(df_ct$Soil_Type16)
df_ct$Soil_Type17 = as.logical(df_ct$Soil_Type17)
df_ct$Soil_Type18 = as.logical(df_ct$Soil_Type18)
df_ct$Soil_Type19 = as.logical(df_ct$Soil_Type19)
df_ct$Soil_Type20 = as.logical(df_ct$Soil_Type20)
df_ct$Soil_Type21 = as.logical(df_ct$Soil_Type21)
df_ct$Soil_Type22 = as.logical(df_ct$Soil_Type22)
df_ct$Soil_Type23 = as.logical(df_ct$Soil_Type23)
df_ct$Soil_Type24 = as.logical(df_ct$Soil_Type24)
df_ct$Soil_Type25 = as.logical(df_ct$Soil_Type25)
df_ct$Soil_Type26 = as.logical(df_ct$Soil_Type26)
df_ct$Soil_Type27 = as.logical(df_ct$Soil_Type27)
df_ct$Soil_Type28 = as.logical(df_ct$Soil_Type28)
df_ct$Soil_Type29 = as.logical(df_ct$Soil_Type29)
df_ct$Soil_Type30 = as.logical(df_ct$Soil_Type30)
df_ct$Soil_Type31 = as.logical(df_ct$Soil_Type31)
df_ct$Soil_Type32 = as.logical(df_ct$Soil_Type32)
df_ct$Soil_Type33 = as.logical(df_ct$Soil_Type33)
df_ct$Soil_Type34 = as.logical(df_ct$Soil_Type34)
df_ct$Soil_Type35 = as.logical(df_ct$Soil_Type35)
df_ct$Soil_Type36 = as.logical(df_ct$Soil_Type36)
df_ct$Soil_Type37 = as.logical(df_ct$Soil_Type37)
df_ct$Soil_Type38 = as.logical(df_ct$Soil_Type38)
df_ct$Soil_Type39 = as.logical(df_ct$Soil_Type39)
df_ct$Soil_Type40 = as.logical(df_ct$Soil_Type40)
df_ct$Wilderness_Area1 = as.logical(df_ct$Wilderness_Area1)
df_ct$Wilderness_Area2 = as.logical(df_ct$Wilderness_Area2)
df_ct$Wilderness_Area3 = as.logical(df_ct$Wilderness_Area3)
df_ct$Wilderness_Area4 = as.logical(df_ct$Wilderness_Area4)

#Partition Data
N = nrow(df_ct)
set.seed(1234)
trainingSize = round(N*0.6)
trainingSample = sample(N, trainingSize)
trainingTotal = df_ct[trainingSample, ]
testTotal = df_ct[-trainingSample, ]

#Build Model
model_ct = rpart(Cover_Type ~ ., data=trainingTotal)

#Prediction
prediction_ct = predict(model_ct, testTotal, type="class")
rpart.plot(model_ct)

#Evaluate
observation = testTotal$Cover_Type

#Error
error_ct = sum(prediction_ct != observation)/nrow(testTotal)

#Stopping Rules
stopping_rules = rpart.control(minsplit = 2, minbucket = 1, cp = 0)
model_ct_sr = rpart(Cover_Type ~ ., data=trainingTotal, control=stopping_rules)

prediction_overfit = predict(model_ct_sr, testTotal, type="class")

error_ct_overfit = sum(prediction_overfit != observation)/nrow(testTotal)

#Pruning
model_ct_pruned = easyPrune(model_ct_sr)

prediction_ct_pruned = predict(model_ct_pruned, testTotal, type="class")

error_ct_pruned = sum(prediction_ct_pruned != observation)/nrow(testTotal)
#################################################################################################################
#Neural Networks

#Libraries
library(gtable)
library(grid)
library(gridExtra)
library(caret)
library(nnet)
library(NeuralNetTools)

#Manage Data
df_nnet = df
df_nnet$Cover_Type = as.factor(df_nnet$Cover_Type)
df_nnet$Soil_Type1 = as.logical(df_nnet$Soil_Type1)
df_nnet$Soil_Type2 = as.logical(df_nnet$Soil_Type2)
df_nnet$Soil_Type3 = as.logical(df_nnet$Soil_Type3)
df_nnet$Soil_Type4 = as.logical(df_nnet$Soil_Type4)
df_nnet$Soil_Type5 = as.logical(df_nnet$Soil_Type5)
df_nnet$Soil_Type6 = as.logical(df_nnet$Soil_Type6)
df_nnet$Soil_Type7 = as.logical(df_nnet$Soil_Type7)
df_nnet$Soil_Type8 = as.logical(df_nnet$Soil_Type8)
df_nnet$Soil_Type9 = as.logical(df_nnet$Soil_Type9)
df_nnet$Soil_Type10 = as.logical(df_nnet$Soil_Type10)
df_nnet$Soil_Type11 = as.logical(df_nnet$Soil_Type11)
df_nnet$Soil_Type12 = as.logical(df_nnet$Soil_Type12)
df_nnet$Soil_Type13 = as.logical(df_nnet$Soil_Type13)
df_nnet$Soil_Type14 = as.logical(df_nnet$Soil_Type14)
df_nnet$Soil_Type15 = as.logical(df_nnet$Soil_Type15)
df_nnet$Soil_Type16 = as.logical(df_nnet$Soil_Type16)
df_nnet$Soil_Type17 = as.logical(df_nnet$Soil_Type17)
df_nnet$Soil_Type18 = as.logical(df_nnet$Soil_Type18)
df_nnet$Soil_Type19 = as.logical(df_nnet$Soil_Type19)
df_nnet$Soil_Type20 = as.logical(df_nnet$Soil_Type20)
df_nnet$Soil_Type21 = as.logical(df_nnet$Soil_Type21)
df_nnet$Soil_Type22 = as.logical(df_nnet$Soil_Type22)
df_nnet$Soil_Type23 = as.logical(df_nnet$Soil_Type23)
df_nnet$Soil_Type24 = as.logical(df_nnet$Soil_Type24)
df_nnet$Soil_Type25 = as.logical(df_nnet$Soil_Type25)
df_nnet$Soil_Type26 = as.logical(df_nnet$Soil_Type26)
df_nnet$Soil_Type27 = as.logical(df_nnet$Soil_Type27)
df_nnet$Soil_Type28 = as.logical(df_nnet$Soil_Type28)
df_nnet$Soil_Type29 = as.logical(df_nnet$Soil_Type29)
df_nnet$Soil_Type30 = as.logical(df_nnet$Soil_Type30)
df_nnet$Soil_Type31 = as.logical(df_nnet$Soil_Type31)
df_nnet$Soil_Type32 = as.logical(df_nnet$Soil_Type32)
df_nnet$Soil_Type33 = as.logical(df_nnet$Soil_Type33)
df_nnet$Soil_Type34 = as.logical(df_nnet$Soil_Type34)
df_nnet$Soil_Type35 = as.logical(df_nnet$Soil_Type35)
df_nnet$Soil_Type36 = as.logical(df_nnet$Soil_Type36)
df_nnet$Soil_Type37 = as.logical(df_nnet$Soil_Type37)
df_nnet$Soil_Type38 = as.logical(df_nnet$Soil_Type38)
df_nnet$Soil_Type39 = as.logical(df_nnet$Soil_Type39)
df_nnet$Soil_Type40 = as.logical(df_nnet$Soil_Type40)
df_nnet$Wilderness_Area1 = as.logical(df_nnet$Wilderness_Area1)
df_nnet$Wilderness_Area2 = as.logical(df_nnet$Wilderness_Area2)
df_nnet$Wilderness_Area3 = as.logical(df_nnet$Wilderness_Area3)
df_nnet$Wilderness_Area4 = as.logical(df_nnet$Wilderness_Area4)

#Standardizer
standardizer = preProcess(df_nnet, c("center", "scale"))
df_nnet = predict(standardizer, df_nnet)

#Partition Data
N = nrow(df_nnet)
set.seed(1234)
trainingSize = round(N*0.6)
trainingCases = sample(N, trainingSize)
trainingTotal = df_nnet[trainingCases, ]
testTotal = df_nnet[-trainingCases, ]

#Prediction
model_nnet = nnet(Cover_Type ~ ., data = trainingTotal, size = 9, maxit = 300)

plotnet(model_nnet)

par(mar = numeric(4))
plotnet(model_nnet,pad_x = .5)

prediction_nnet = as.factor(predict(model_nnet, testTotal, type = "class"))

#Table Pretty
observation = testTotal$Cover_Type
table(prediction_nnet, observation)

d = table(prediction_nnet, observation)

table = tableGrob(d)

title = textGrob("Observations",gp=gpar(fontsize=12))
footnote = textGrob("1 = Spruce/Fir
2 = Lodgepole Pine
3 = Ponderosa Pine
4 = Cottonwood/Willow
5 = Aspen
6 = Douglas-fir
7 = Krummholz", x=0, hjust=0,
                    gp=gpar( fontface="italic",fontsize = 8))

padding = unit(1,"line")
table = gtable_add_rows(table, 
                        heights = grobHeight(title) + padding,
                        pos = 0)
table = gtable_add_rows(table, 
                        heights = grobHeight(footnote)+ padding)
table = gtable_add_grob(table, list(title, footnote),
                        t=c(1, nrow(table)), l=c(1,2), 
                        r=ncol(table))

dev.new()
grid.newpage()
grid.draw(table)

draw.text.rot = function(t,just, i, j,rot,x=.45,y=.500) {
  grid.text(t, x=x, y=y, just=just,rot=90,gp=gpar(fontsize=12))
}

title2 = draw.text.rot("Predictions Neural Nets",c("left"),2,2,x=.22,y=.475)

#Error
error_nnet = sum(prediction_nnet != observation) / nrow(testTotal)

#################################################################################################################
#Ensemble

#Libraries
library(e1071)

#Manage Data
df$Cover_Type = as.factor
df$Soil_Type1 = as.factor(df$Soil_Type1)
df$Soil_Type2 = as.factor(df$Soil_Type2)
df$Soil_Type3 = as.factor(df$Soil_Type3)
df$Soil_Type4 = as.factor(df$Soil_Type4)
df$Soil_Type5 = as.factor(df$Soil_Type5)
df$Soil_Type6 = as.factor(df$Soil_Type6)
df$Soil_Type7 = as.factor(df$Soil_Type7)
df$Soil_Type8 = as.factor(df$Soil_Type8)
df$Soil_Type9 = as.factor(df$Soil_Type9)
df$Soil_Type10 = as.factor(df$Soil_Type10)
df$Soil_Type11 = as.factor(df$Soil_Type11)
df$Soil_Type12 = as.factor(df$Soil_Type12)
df$Soil_Type13 = as.factor(df$Soil_Type13)
df$Soil_Type14 = as.factor(df$Soil_Type14)
df$Soil_Type15 = as.factor(df$Soil_Type15)
df$Soil_Type16 = as.factor(df$Soil_Type16)
df$Soil_Type17 = as.factor(df$Soil_Type17)
df$Soil_Type18 = as.factor(df$Soil_Type18)
df$Soil_Type19 = as.factor(df$Soil_Type19)
df$Soil_Type20 = as.factor(df$Soil_Type20)
df$Soil_Type21 = as.factor(df$Soil_Type21)
df$Soil_Type22 = as.factor(df$Soil_Type22)
df$Soil_Type23 = as.factor(df$Soil_Type23)
df$Soil_Type24 = as.factor(df$Soil_Type24)
df$Soil_Type25 = as.factor(df$Soil_Type25)
df$Soil_Type26 = as.factor(df$Soil_Type26)
df$Soil_Type27 = as.factor(df$Soil_Type27)
df$Soil_Type28 = as.factor(df$Soil_Type28)
df$Soil_Type29 = as.factor(df$Soil_Type29)
df$Soil_Type30 = as.factor(df$Soil_Type30)
df$Soil_Type31 = as.factor(df$Soil_Type31)
df$Soil_Type32 = as.factor(df$Soil_Type32)
df$Soil_Type33 = as.factor(df$Soil_Type33)
df$Soil_Type34 = as.factor(df$Soil_Type34)
df$Soil_Type35 = as.factor(df$Soil_Type35)
df$Soil_Type36 = as.factor(df$Soil_Type36)
df$Soil_Type37 = as.factor(df$Soil_Type37)
df$Soil_Type38 = as.factor(df$Soil_Type38)
df$Soil_Type39 = as.factor(df$Soil_Type39)
df$Soil_Type40 = as.factor(df$Soil_Type40)
df$Wilderness_Area1 = as.factor(df$Wilderness_Area1)
df$Wilderness_Area2 = as.factor(df$Wilderness_Area2)
df$Wilderness_Area3 = as.factor(df$Wilderness_Area3)
df$Wilderness_Area4 = as.factor(df$Wilderness_Area4)

#Prediction Helper Models
pred_knn_full = predict(model_knn_crossval,df_knn)
pred_ct_full = predict(model_ct_pruned, df_ct)
pred_nnet_full = predict(model_nnet, df_nnet)

colnames(pred_knn_full) = paste("kNN", colnames(pred_knn_full), sep = "_")
colnames(pred_ct_full) = paste("ct", colnames(pred_ct_full), sep = "_")
colnames(pred_nnet_full) = paste("nnet", colnames(pred_nnet_full), sep = "_")

df_stacked = cbind(df, pred_knn_full, pred_ct_full, pred_nnet_full)

#Partition Data
trainingTotal_stacked = df_stacked[trainingCases, ]
testTotal_stacked = df_stacked[-trainingCases, ]

#Build Model
stacked = naiveBayes(Cover_Type ~ ., data=trainingTotal_stacked)

#Prediction Manager Model
predictions_stacked = predict(stacked, testTotal_stacked)
observations = testTotal$Cover_Type

#Error
error_rate_stacked = sum(predictions_stacked != observations)/nrow(testTotal_stacked)
