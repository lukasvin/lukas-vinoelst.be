##-------------------------------------------------------------------------
#!change working directory!
#Data Inladen

library(MASS)
library(class)

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

plot(wijnScaled.pca)
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
resultaten = cbind(APER=NULL,LOOV=NULL,CV=NULL);
klijst = 1:max_k;

for (k in klijst) {
  resultaten = rbind(resultaten,cbind(
    APER=aer(V1,knn(wijnScaledSp, wijnScaledSp, V1, k),FALSE),
    LOOV=aer(V1,knn.cv(wijnScaledSp,V1,k),FALSE),
    CV=aer(V1[validate],knn(wijnScaledSp[train,],wijnScaledSp[validate,],V1[train],k),FALSE)
  ))
}

head(resultaten)

plot(c(0,78),c(0,1),xlab="Number of neigbours",ylab="Error rate",type="n"); abline(h=lda.er, col = "blueviolet"); abline(h=qda.er, col="chartreuse")
matplot(1:78,resultaten,type='l',add=TRUE)

best_k = which.min(resultaten[,3])
resultaten

#APER geeft natuurlijk beste resultaten samen met LOOV maar denk dat er in de opdracht specifiek verwacht wordt dat we direct CV doen met de train, en validate/test sets


#lda geeft gemiddeld kleinere error rate dan qda en ligt in dezelfde grootteorde van knn

##----------------------------------------------------------------------
#testen t.o.v. testset

lda.er.test #al berekend in vorige sectie
CV=aer(V1[test],knn(wijnScaledSp[train,],wijnScaledSp[test,],V1[train],best_k),FALSE); CV

#nearest neighbours geeft betere aer.

