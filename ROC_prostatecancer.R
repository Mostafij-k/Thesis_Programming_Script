# ROC curve tutorial

     # Install require packages

# install.packages('ROCR')
# install.packages('pROC')
# install.packages('sqldf')
# install.packages('gmodels')
# install.packages('caret')
# install.packages('lattice')

    # Call library
library(ROCR)
library(pROC)
library(sqldf)
library(gmodels)
library(caret)
library(lattice)

    # Set working directory
setwd("C:/Users/user/Desktop/Thesis work/new/ROC curve")

    # Read data
datos = read.table(file = 'psa2b.csv', sep = ',', header = T)
str(datos)
head(datos, n = 20)

datos.originales <- datos

datos <- sqldf("select id, d, min(t), fpsa, tpsa, age from 'datos.originales'
               group by id")

table(datos$d)
hist(datos$fpsa)
hist(datos$tpsa)

prf = caret::BoxCoxTrans(y =(datos$fpsa))
prf

datos$fpsatran <- predict(prf, datos$fpsa)
hist(datos$fpsatran)

prt = caret::BoxCoxTrans(y =(datos$tpsa))
prt
datos$tpsatran <- predict(prt, datos$tpsa)
hist(datos$tpsatran)

centerscale <- caret::preProcess(datos[,c(7,8)])
centerscale

datos2 <- cbind(datos,
                predict(centerscale, datos[,c(7,8)])
)
names(datos2)[9] <- "fpsatip"
names(datos2)[10] <- "tpsatip"


d1 <- densityplot( ~ fpsatip, 
                   data = datos2,
                   groups = d,
                   auto.key = TRUE)

d2 <- densityplot( ~ tpsatip, 
                   data = datos2,
                   groups = d,
                   auto.key = TRUE)

print(d1, position = c(0, .5, 1, 1), more = T)
print(d2, position = c(0, 0, 1, .5))


value <- 4

val.trans.1 <- predict(prt, value)     #prt is the Box-Cox transform for tpsa
val.trans.2 <- (val.trans.1 - mean(datos2$tpsatran)) / sd(datos2$tpsatran)

val.trans.2


datos2$scree <- "b-test neg"
datos2$scree[datos$tpsa >= val.trans.2] <- "a-test pos"
datos2$cond <- "a-cond pos"
datos2$cond[datos$d == 0] <- "b-cond neg"

CrossTable(datos2$scree, datos2$cond, 
           prop.c = TRUE,
           prop.chisq = FALSE,
           prop.r = FALSE,
           prop.t = FALSE)


plot(c(0, 1), c(0, 1), type= "n", xlab = "FPR", ylab = "TPR")
FPR <- .029
TPR <- .465
points(FPR, TPR)
segments(0, 0, FPR, TPR)
segments(FPR, TPR, 1, 1)


pred.z.01 <- prediction(datos2$id, datos2$d)

# uso: performance(prediction_object, "tpr", "fpr") creates the object with performance metrics
# TPR: True Positive Ratio
# FPR: False Positive Ratio

perf.z.01 <- performance(pred.z.01, "tpr", "fpr")

plot.new()
plot(perf.z.01, col = "green") 
abline(0, 1, 
       col = "grey")

auc.z.01 <- performance(pred.z.01, "auc")

legend("bottomright", 
       paste(round(as.numeric(auc.z.01@y.values), digits = 2)), 
       col = c("green"),
       pch = c(3))

pROC::plot.roc(datos2$d, datos2$tpsa,
               print.auc = TRUE)






