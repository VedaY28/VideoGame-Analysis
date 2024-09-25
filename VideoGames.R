#Veda Yakkali

install.packages("devtools")
devtools::install_github("janish-parikh/ZTest")
devtools::install_github("devanshagr/PermutationTestSecond")

game <- read.csv('Video_Games.csv')
colors<- c('red','blue','cyan','yellow','green') # Assigning different colors to bars
#-----------------------------------------------------------------------------------------------
#Testing:
summary(game)

table(game$Publisher)
table(game$Rating)
table(game$Genre)

barplot(table(game$Publisher), main='Frequency of Publisher', xlab='Publisher', ylab='Frequency', col=colors)
barplot(table(game$Rating), main='Frequency of Rating', xlab='Rating', ylab='Frequency', col=colors)
barplot(table(game$Genre), main='Frequency of Genre', xlab='Genre', ylab='Frequency', col=colors)

GP <- tapply(game$Global_Sales, game$Publisher, mean)
GR <- tapply(game$Global_Sales, game$Rating, mean)
GG <- tapply(game$Global_Sales, game$Genre, mean)

barplot(GP, main='Mean Global Sales of Publishers', xlab='Genre', ylab='Frequency', col=colors)
barplot(GR, main='Mean Global Sales of Rating', xlab='Genre', ylab='Frequency', col=colors)
barplot(GG, main='Mean Global Sales of Genre', xlab='Genre', ylab='Frequency', col=colors)


table(game$Publisher=="Sony Computer Entertainment")
table(game$Publisher=="Nintendo")

#-----------------------------------------------------------------------------------------------
#Hypothesis1:
gameS <- game[game$Publisher =='Sony Computer Entertainment' & game$Genre =='Platform',]
gameN <- game[game$Publisher =='Nintendo' & game$Genre =='Platform',]

sony <- tapply(gameS$Global_Sales, gameS$Publisher, mean)
nin <- tapply(gameN$Global_Sales, gameN$Publisher, mean)

sony
nin

tab1 <- c(sony, nin)
barplot(tab1 ,main='Mean Global Sales of Nintendo & Sony Platform Games', xlab='Publishers', ylab='Mean Global Sales', col = colors)


act <- subset(game, Genre = 'Platform')
publ <- tapply(act$Global_Sales, act$Publisher, mean)
publ
barplot(publ, main='Mean Score for Low Budget Movies by Content', xlab='Publisher', ylab='Mean Global Sales', col = colors)
PermutationTestSecond::Permutation(act, "Publisher", "Global_Sales", 1000, "Sony Computer Entertainment", "Nintendo")

#-----------------------------------------------------------------------------------------------
#Hypothesis2:
gameS2 <- game[game$Publisher =='Sony Computer Entertainment' & game$Rating =='M',]
gameN2 <- game[game$Publisher =='Nintendo' & game$Rating =='M',]

sonyr <- tapply(gameS2$Global_Sales, gameS2$Publisher, mean)
ninr <- tapply(gameN2$Global_Sales, gameN2$Publisher, mean)

sonyr
ninr

tab2 <- c(sonyr, ninr)
barplot(tab2 ,main='Mean Global Sales of Nintendo & Sony M-Rated Games', xlab='Publishers', ylab='Mean Global Sales', col = colors)


Ma <- subset(game, Rating = 'M')
pub <- tapply(Ma$Global_Sales, Ma$Publisher, mean)
pub
barplot(pub, main='Mean Score for Low Budget Movies by Content', xlab='Publisher', ylab='Mean Global Sales', col = colors)
PermutationTestSecond::Permutation(Ma, "Publisher", "Global_Sales", 1000, "Nintendo", "Sony Computer Entertainment")
