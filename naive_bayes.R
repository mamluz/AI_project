library(naivebayes)
library(rpart)

df = banknote

colnames(df) <- c("variance","skewness",
                  "curtosis","entropy","Authenticity")
df$Authenticity <- as.factor(df$Authenticity)
str(df)
#NAIVE BAYES

idx=sample(1:nrow(df),0.7*nrow(df)) #proporcja test-train

train = df[idx,]
test = df[-idx,]

model = naive_bayes(Authenticity ~ ., data=train)
plot(model)
#budowa confusion-matrix
p=predict(model,test)
tab1=table(p,test$Authenticity)

print(tab1)

#dobroc klasyfikatora
acc=function(x)
{sum(diag(x)/sum(x))} #funkcja wylicza dobroc klasyfikacji

result=acc(tab1)

print("Dobroc klasyfikatora:")
print(result)



