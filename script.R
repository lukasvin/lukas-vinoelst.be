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
#Opdelen in train, validatie en test data
indices = 1:178;
test = sample(178,50);
validate_train = indices[-test];
validate = sample(validate_train,50);
train = validate_train[-which(validate_train %in% validate)];

#dit is mogelijks niet nodig
wijn.train = wijn[train,]
wijn.validate = wijn[validate,]
wijn.test = wijn[test,]

#functie voor error rate
aer = function(y1,y2,conf=TRUE) {
  confusion = table(y1,y2)
  if (conf) {print(confusion)}
  observaties = sum(confusion)
  verkeerd = observaties-sum(diag(confusion))
  verkeerd/observaties
}

##-----------------------------------------------------------------------
#determinantmethode
#lineair

wijn.lda <- lda(V1 ~.,wijnScaledSp ,subset = train);
lda.pred.val <- predict(wijn.lda, wijnScaledSp[validate,]);

#quadratisch

wijn.qda <- qda(V1 ~.,wijnScaledSp ,subset = train);
qda.pred.val <- predict(wijn.qda, wijnScaledSp[validate,])

##----------------------------------------------------------------------
#k-nearest-neighboursmethode
#zoek k met kleinste error rate

max_k = 78;
resultaten = cbind(ER.validate=NULL)
klijst = 1:max_k

for (k in klijst) {
  resultaten = rbind(resultaten,cbind(
    ER.validate=aer(V1[validate],knn(wijnScaledSp[train,], wijnScaledSp[validate,], V1[train], k),FALSE) 
  ))
}

head(resultaten)

plot(c(0,78),c(0,1),xlab="Number of neigbours",ylab="Error rate",type="n");
matplot(1:78,resultaten,type='l',add=TRUE)
