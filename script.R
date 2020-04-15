##-------------------------------------------------------------------------
#!change working directory!
#Data Inladen
install.packages("ggfortify")
install.packages("ggplot2")
install.packages("Rcpp")
library(MASS)
library(class)
library(ggplot2)
library(ggfortify)


wijn <- read.csv("wijn.csv", header=FALSE, sep = ",")
attach(wijn)
summary(wijn)

#Data schalen
wijnScaled = scale(wijn[,2:14]);
#extra col met de druifsoorten
wijnScaledSp = wijn;wijnScaledSp[,2:14] = wijnScaled;


##-------------------------------------------------------------------------

#geschaalde PCA maken [waarschijnlijk best aangezien geen eenheden gegeven worden]
wijnScaled.pca = prcomp(wijnScaled,scale=TRUE);

#plotjes
plot(wijnScaled.pca)

autoplot(wijnScaled.pca, data = wijnScaledSp, x=1,y=2,colour = V1,label= TRUE)
autoplot(wijnScaled.pca, data = wijnScaledSp, x=1,y=3,colour = V1)
autoplot(wijnScaled.pca, data = wijnScaledSp, x=1,y=4,colour = V1)
autoplot(wijnScaled.pca, data = wijnScaledSp, x=1,y=5,colour = V1)

autoplot(wijnScaled.pca, data = wijnScaledSp, x=2,y=1,colour = V1)
autoplot(wijnScaled.pca, data = wijnScaledSp, x=2,y=3,colour = V1)
autoplot(wijnScaled.pca, data = wijnScaledSp, x=2,y=4,colour = V1)
autoplot(wijnScaled.pca, data = wijnScaledSp, x=2,y=5,colour = V1)

autoplot(wijnScaled.pca, data = wijnScaledSp, x=3,y=1,colour = V1)
autoplot(wijnScaled.pca, data = wijnScaledSp, x=3,y=2,colour = V1)
autoplot(wijnScaled.pca, data = wijnScaledSp, x=3,y=4,colour = V1)
autoplot(wijnScaled.pca, data = wijnScaledSp, x=3,y=5,colour = V1)

autoplot(wijnScaled.pca, data = wijnScaledSp, x=4,y=1,colour = V1)
autoplot(wijnScaled.pca, data = wijnScaledSp, x=4,y=2,colour = V1)
autoplot(wijnScaled.pca, data = wijnScaledSp, x=4,y=3,colour = V1)
autoplot(wijnScaled.pca, data = wijnScaledSp, x=4,y=5,colour = V1)

autoplot(wijnScaled.pca, data = wijnScaledSp, x=5,y=1,colour = V1)
autoplot(wijnScaled.pca, data = wijnScaledSp, x=5,y=2,colour = V1)
autoplot(wijnScaled.pca, data = wijnScaledSp, x=5,y=3,colour = V1)
autoplot(wijnScaled.pca, data = wijnScaledSp, x=5,y=4,colour = V1)

#numerieke info

summary(wijnScaled.pca)

scaledRot = wijnScaled.pca$rotation;
#PC1 verklaart 36,2% van de data en met een grote PC1 komt een lagere hoeveelheid flavonoiden,fenolen,proteines en proanrhocyanidine relatief t.o.v. de andere variabelen overeen. [ interpretatie : hoeveelheid smaak in wijn/hoe zuur is de wijn ]
#PC2 verklaart 19,2% van de data en met een grote PC2 komt een lagere tint en proteineconcentratie relatief t.o.v. de hogere kleurintensiteit en hoger suikergehalte van de druif overeen. [ interpretatie : ? ]
#PC3 verklaart 11,1% van de data en met een grote PC3 komt een lager suikergehalte van de druif relatief t.o.v. het hogere asgehalte en de hogere alkaliteit overeen. [ interpretatie : ? ]

scaledPred = predict(wijnScaled.pca);


##-------------------------------------------------------------------------
#Classificatie

#functie voor error rate
aer = function(y1,y2,conf=TRUE) {
  confusion = table(y1,y2)
  if (conf) {print(confusion)}
  observaties = sum(confusion)
  verkeerd = observaties-sum(diag(confusion))
  verkeerd/observaties
}

#Opdelen in train, validatie en test data

lda.ervec.val = c(1:50);
lda.ervec.test = c(1:50);
qda.ervec = c(1:50);

for (i in 1:50) {
  
indices = 1:178;
test = sample(178,50);
validate_train = indices[-test];
validate = sample(validate_train,50);
train = validate_train[-which(validate_train %in% validate)];

##-----------------------------------------------------------------------
#determinantmethode
#lineair

wijn.lda <- lda(V1 ~.,wijnScaledSp ,subset = train);
lda.pred.val <- predict(wijn.lda, wijnScaledSp[validate,]);
lda.pred.test <- predict(wijn.lda, wijnScaledSp[test,]);

lda.ervec.val[i] = aer(wijnScaledSp$V1[validate],lda.pred.val$class,conf=FALSE);
lda.ervec.test[i] = aer(wijnScaledSp$V1[test],lda.pred.test$class,conf=FALSE);

#quadratisch

wijn.qda <- qda(V1 ~.,wijnScaledSp ,subset = train);
qda.pred.val <- predict(wijn.qda, wijnScaledSp[validate,]);

qda.ervec[i] = aer(wijnScaledSp$V1[validate],qda.pred.val$class,conf=FALSE);

}
#gemiddelde error-rates van 50 opdelingen 
lda.er.val = mean(lda.ervec.val);lda.er.val
lda.er.test = mean(lda.ervec.test);lda.er.test

qda.er = mean(qda.ervec);qda.er

##----------------------------------------------------------------------
#k-nearest-neighboursmethode
#zoek k met kleinste error rate

max_k = 78;
resultaten = cbind(CV=NULL);
klijst = 1:max_k;

for (k in klijst) {
  resultaten = rbind(resultaten,cbind(
    CV=aer(V1[validate],knn(wijnScaledSp[train,],wijnScaledSp[validate,],V1[train],k),FALSE)
  ))
}

head(resultaten)

plot(c(0,78),c(0,1),main="Error rate in function of neighbours",xlab="Number of neigbours",ylab="Error rate",type="n"); abline(h=lda.er, col = "blueviolet"); abline(h=qda.er, col="chartreuse")
matplot(1:78,resultaten,type='l',add=TRUE)
legend(x=0, y=0.8, legend=c('knn','lda','qda'), fill=c("black","blueviolet","chartreuse"))

best_k = which.min(resultaten[,3])
resultaten


#lda geeft gemiddeld kleinere error rate dan qda en ligt in dezelfde grootteorde van knn

##----------------------------------------------------------------------
#testen t.o.v. testset

lda.er.test #al berekend in vorige sectie
CV.test.ervec = c(1:50)
for (j in 1:50) {
  CV.test.ervec[j]=aer(V1[test],knn(wijnScaledSp[train,],wijnScaledSp[test,],V1[train],best_k),FALSE);
}
#gemiddelde error rate van 50 nearest neighbour modellen met best_k
CV.er.test = mean(CV.test.ervec); CV.er.test

#lda geeft betere aer.