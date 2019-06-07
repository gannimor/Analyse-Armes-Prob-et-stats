donn <- read.csv2("Data.csv")
pays=donn[,1]
pop=donn[,2]
arm=donn[,3]
mort=donn[,4]
annee=donn[,5]
plot(arm,mort,main="Morts selon le nombre d'armes à feu par pays",
     xlab="Armes a feu par 100 habitants",
     ylab="Morts par armes à feu par 100k habitants")
text(arm,mort,pays,cex=0.5,pos=3)
plot(mort,pop,main="Morts selon la population du pays",
     xlab="Morts par armes à feu par 100k habitants",
     ylab="Nombre d'habitants")
text(mort,pop,pays,cex=0.5,pos=3)
hist(annee,main="Histograme de l'année d'obtention des données",
     xlab="Année d'obtention des données",
     ylab="Fréquence des données")

Reglin <- lm(mort~arm)
plot(arm,mort,main="Morts selon le nombre d'armes à feu par pays",
     xlab="Armes a feu par 100 habitants",
     ylab="Morts par armes à feu par 100k habitants")
abline(Reglin)
summary(Reglin)
confint(Reglin)
anova(Reglin)
coefcorel1 <- cov(arm,mort)/sqrt(var(arm)*var(mort))
coefcorel1
plot(Reglin,1:2)

Reglin2 <- lm(pop~mort)
plot(mort,pop,main="Morts selon la population du pays",
     xlab="Morts par armes à feu par 100k habitants",
     ylab="Nombre d'habitants")
abline(Reglin2)
summary(Reglin2)
confint(Reglin2)
anova(Reglin2)
coefcorel2 <- cov(mort,pop)/sqrt(var(mort)*var(pop))
coefcorel2
plot(Reglin2,1:2)


