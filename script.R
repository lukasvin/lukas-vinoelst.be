##-------------------------------------------------------------------------
#!change working directory!
#Data Inladen


wijn <- read.csv("wijn.csv", header=FALSE, sep = ",")
summary(wijn)

#Data schalen
wijnScaled = scale(wijn[,2:14]);

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
train = sample(178,50);
validate_test = indices[-train];
validate = sample(validate_test,50);
test = validate_test[-validate];

wijn.train = wijn[train,]
wijn.validate = wijn[validate,]
wijn.test = wijn[test,]

