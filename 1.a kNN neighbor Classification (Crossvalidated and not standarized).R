df = read.csv("C:/Users/fero9/OneDrive/Desktop/Babson/Fall/Machine Learning for Business/Project/train.csv")
source('C:/Users/fero9/OneDrive/Desktop/Babson/Fall/Machine Learning for Business/Data/BabsonAnalytics.R')
library(gtable)
library(grid)
library(gridExtra)
library(caret)

df$Id = NULL

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

#Table
observation = testTotal$Cover_Type
table(prediction_knn, observation)

#Table 
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

#Error
error_knn = sum(prediction_knn != observation)/nrow(testTotal)

#Build Model Crossval
CrossVal = kNNCrossVal(Cover_Type ~., trainingTotal)
model_knn_crossval = knn3(Cover_Type ~ ., data=trainingTotal, k = CrossVal)

#Prediction Crossval
prediction_knn_crossval = predict(model_knn_crossval, testTotal, type = "class")

#Error Crossval
observation = testTotal$Cover_Type
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

error_knn_crossval = sum(prediction_knn_crossval != observation)/nrow(testTotal)

error_bench = benchmarkErrorRate(trainingTotal$Cover_Type, testTotal$Cover_Type)
