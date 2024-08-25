     ## Basic ROC curve
# install ROCR packages

install.packages('ROCR')
library(ROCR)

# create example data frame
df = data.frame(a=sample(1:25,400,replace = T),
                b=runif(400)*3,               
                c=sample(1:10,400,replace = T))
#runif() function generates random deviates of the uniform distribution and
#is written as runif(n, min = 0, max = 1) 

df = cbind(df,type=ifelse((df$a+df$b+df$c)>=20, "high", "low")) 
index = sample(1:nrow(df), size = .80 * nrow(df))
train = df[index, ]
test = df[-index, ]
model = glm(as.factor(type)~a+b,data=train, 
            family = binomial(link = "logit"))
pred = predict(model,test,type="response")

# Next, we'll use a 'prediction' and 'performance' functions of a 'ROCR' package to check the accuracy
pred = prediction(pred, test$type)
perf = performance(pred, "acc")
plot(perf)

# You can also check the other metrics with a 'performance' function and visualize them in a plot.
perf_cost = performance(pred, "cost")
perf_err = performance(pred, "err")
perf_tpr = performance(pred, "tpr")
perf_sn_sp = performance(pred, "sens", "spec")

plot(perf_cost)

# We can get maximum accuracy cutoff from accuracy performance.
max_ind = which.max(slot(perf, "y.values")[[1]] )
acc = slot(perf, "y.values")[[1]][max_ind]
cutoff = slot(perf, "x.values")[[1]][max_ind]
print(c(accuracy= acc, cutoff = cutoff))

# Next, we'll create a ROC curve.
roc = performance(pred,"tpr","fpr")
plot(roc, colorize = T, lwd = 2)
abline(a = 0, b = 1) 

      ## Important note: 
#A random guess is a diagonal line and the model does not make any sense. 
                         
#If the curve approaches closer to the top-left corner, model performance becomes much better.

#Any curve under the diagonal line is worst than a random guess. 
#We can set the cutoff threshold based on our requirement in terms of sensitivity and specificity importance.


      # AUC
# The AUC represents the area under the ROC curve.
# We can evaluate the model the performance by the value of AUC. 
# Higher than 0.5  shows a better model performance. 
# If the curve changes to rectangle it is perfect classifier with AUC value 1.

auc = performance(pred, measure = "auc")
print(auc@y.values)




