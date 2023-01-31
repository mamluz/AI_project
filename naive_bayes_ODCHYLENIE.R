library(naivebayes)
library(rpart)
library(class)
i=0


results<- c(1:30)

while (i <= 30)
  
{
  df = banknote
  
  colnames(df) <- c("variance","skewness",
                    "curtosis","entropy","Authenticity")
  df$Authenticity <- as.factor(df$Authenticity)
  str(df)
  #NAIVE_BAYES z ODCHYLENIEM
  
  idx=sample(1:nrow(df),0.8*nrow(df)) #proporcja test-train
  
  train = df[idx,]
  test = df[-idx,]
  
  model = naive_bayes(Authenticity ~ ., data=train)
  
  plot(model)
  #budowa confusion-matrix
  p=predict(model,test)
  tab1=table(p,test$Authenticity)
  
  acc=function(x)
  {sum(diag(x)/sum(x))} #funkcja wylicza dobroc klasyfikacji
  
  results[i]=acc(tab1)

  i=i+1
}
avg = mean(results)
sd_value = sd(results) #odchylenie standardowe

print("Srednia dobroc klasyfikatora:")
print(avg)
print("Odchylenie standardowe:")
print(sd_value)





