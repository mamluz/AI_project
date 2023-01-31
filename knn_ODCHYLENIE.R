library(naivebayes)
library(rpart)
library(class)
i=0

k_values<- c(1:30)
results<- c(1:30)

while (i <= 30)
  
{
  df = banknote
  
  colnames(df) <- c("variance","skewness",
                    "curtosis","entropy","Authenticity")
  df$Authenticity <- as.factor(df$Authenticity)
  str(df)
  #KNN z ODCHYLENIEM
  
  idx=sample(1:nrow(df),0.2*nrow(df)) #proporcja test-train
  
  cl=df$Authenticity
  
  cl_Train=cl[idx] 
  cl_Test=cl[-idx]
  
  data=df[,c(1,2,3,4)] #wyluskanie 4 kolumn 
  
  train=data[idx,]
  test=data[-idx,]
  
  model=knn(train,test,cl=cl_Train,k=i+1)
  
  plot(model)
  #budowa confusion-matrix
  tab1=table(model,cl_Test)

  acc=function(x)
  {sum(diag(x)/sum(x))} #funkcja wylicza dobroc klasyfikacji
  
  results[i]=acc(tab1)
  k_values[i]=i+1
  i=i+1
}
avg = mean(results)
sd_value = sd(results) #odchylenie standardowe

print("Srednia dobroc klasyfikatora:")
print(avg)
print("Odchylenie standardowe:")
print(sd_value)





