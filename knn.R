library(naivebayes)
library(rpart)
library(class)

df = banknote

colnames(df) <- c("variance","skewness","curtosis","entropy","Authenticity")
df$Authenticity <- as.factor(df$Authenticity)
str(df)
#KNN

idx=sample(1:nrow(df),0.2*nrow(df)) #proporcja test-train

cl=df$Authenticity

cl_Train=cl[idx] 
cl_Test=cl[-idx]

data=df[,c(1,2,3,4)] #wyluskanie 4 kolumn 

train=data[idx,]
test=data[-idx,]

model=knn(train,test,cl=cl_Train,k=5)

plot(model)
#budowa confusion-matrix
tab1=table(model,cl_Test)

print(tab1)

#dobroc klasyfikatora
acc=function(x)
{sum(diag(x)/sum(x))} #funkcja wylicza dobroc klasyfikacji

result=acc(tab1)
sd_value = sd(tab1) #odchylenie standardowe
print("Dobroc klasyfikatora:")
print(result)
print("Odchylenie standardowe:")
print(sd_value)





