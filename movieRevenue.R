remove (list = ls())
library(beepr)
library(stringr)
# Algorytm lasu losowego
choose.cp <- function(tree) {
  
  n <- which.min(tree$cptable[, 4])
  n.min <- min(which(tree$cptable[, 4] < tree$cptable[n, 4] + tree$cptable[n, 5]))
  
  return(tree$cptable[n.min, 1])
}

do.rf <- function(data, newdata, n) {
  
  rf <- randomForest(class ~ ., data = data, ntree = n)
  rf.pred <- predict(rf, newdata = newdata)
  
  return(err.rate(newdata$class, rf.pred))
}
err.rate <- function(org.class, pred.class) {
  
  CM <- table(org.class, pred.class)
  
  return(1 - sum(diag(CM)) / sum(CM))
}

CM.large <- function(org.class, pred.class) {
  
  CM <- table(org.class, pred.class)
  
  # Skuteczność klasyfikatora
  ACC <- sum(diag(CM)) / sum(CM)
  
  # Wartości true positive i true negative
  # zakładamy, że klasa "2" jest "pozytywna"
  TP <- CM[2,2]
  TN <- CM[1,1]
  
  sums <- apply(CM, 1, sum)
  
  TPR <- TP / sums[2]
  FPR <- 1 - TN / sums[1]
  
  return(c(ACC = round(ACC,4), TP = TP, TN = TN, TPR = round(TPR, 4), FPR = round(FPR, 4), row.names = NULL))
}

CV <- function(data, K) {
  
  N <- nrow(data)
  
  # Dane przetasowane
  data.rnd <- data[sample(1:N),]
  
  # Tworzenie K pseudoprób
  sets <- sapply(1:K, function(i) ((i-1) * (N/K) + 1):(i * (N/K)))
  
  # Przypadek K = 1
  if(is.vector(sets)) sets <- t(as.matrix(sets))
  
  # Dla każdej pseudopróby wyznaczamy liczbę pomyłek
  res <- t(sapply(1:K, function(k) CV.main(data.rnd[-c(sets[,k]),], data.rnd[sets[,k],])))
  
  res
}

# Główna funkcja odpowiedzialna za CV
# przyjmuje PU (jedna z pseudoprób) oraz PT
CV.main <- function(learn, test) {
  
  learn.classifier <- lda(class ~ ., data = learn)
  test.pred <- predict(learn.classifier, newdata = test)
  
  # Macierz pomyłek
  CM <- table(test$class, test.pred$class)
  
  # Liczba błędów
  sum(CM) - sum(diag(CM))
  
  #large
  #CM.large(test$class, test.pred$class)
}



data <- read.csv(file = "~/workspace/R/LSED/MovieRevenuePrediction/train.csv", header = T)
#cleaning up data 
data <- data[data$budget != 0, ]

idx.of.movies.which.make.money = data$budget < data$revenue
data$revenue[idx.of.movies.which.make.money] <- 1
data$revenue[!idx.of.movies.which.make.money] <- 2
data$revenue <- factor(data$revenue)

#removing some cols
drops <- c("id","belongs_to_collection", "poster_path", "status",
           "Keywords", "cast", "crew", "imdb_id", "production_companies")
data <- data[ , !(names(data) %in% drops)]

#substituting txt data by numbers
data$title <- nchar(as.vector(data$title))
data$original_title <- nchar(as.vector(data$original_title))
data$overview <- nchar(as.vector(data$overview))
data$tagline <- nchar(as.vector(data$tagline))

#extrect the genre of the movie
pat = "([A-Z][a-z]{1,20})"
data$genres <- str_extract(string = data$genres, pattern = pat)
data$genres <- factor(data$genres)

#extract if movie has homepage
data$homepage <- as.character(data$homepage)
idx.of.domains <- data$homepage != ""
data$homepage[idx.of.domains] <- 1
data$homepage[!idx.of.domains] <- 2
data$homepage <- factor(data$homepage)

#extract production country
pat2 = "([A-Z][A-Z])"
data$production_countries <- str_extract(string = data$production_countries, pattern = pat2)
data$production_countries <- factor(data$production_countries)

pat3= "[']([a-z][a-z])[']"
data$spoken_languages <- gsub('\'', '',str_extract(string = data$spoken_languages, pattern = pat3))
data$spoken_languages <- factor(data$spoken_languages)

#extracting the date
pat4 <- "^([0-9]{0,2})[/]"
month <- gsub('/', '', str_extract(as.character(data$release_date),pattern = pat4))
pat5 <-  "[/]([0-9]{1,2})[/]"
day <- gsub('/', '', str_extract(as.character(data$release_date),pattern = pat5))
pat6 <- "[/]([0-9]{1,2})$"
year <- gsub('/', '', str_extract(as.character(data$release_date),pattern = pat6))
year <- as.numeric(year)
idx.of.years.XXI.century <- year < 20
year[idx.of.years.XXI.century] <- year[idx.of.years.XXI.century] +2000
year[!idx.of.years.XXI.century] <- year[!idx.of.years.XXI.century] +1900
year <- as.character(year) 
weekday.of.release <- weekdays(as.Date(paste(day, month, year, sep = '-'),'%d-%m-%Y'), abbreviate = T)
month <- as.numeric(month)
day <- as.numeric(day)
year <- as.numeric(year)
data$month <- month 
data$day <- day
data$year <- year
data$weekday.of.release <- weekday.of.release
data$weekday.of.release <- factor(data$weekday.of.release)
data <- data[ , !(names(data) %in% 'release_date')]
data$month <- month.name[data$month]
data$month <- factor(data$month)
data <- data[, c(1,2,3,4,5,6,7,8,9,10,11,12,14,15,16,17,13)]

names(data)[names(data) == "revenue"] <- "class"

#extrating info whether it's an american movie
idx.of.eng.movies <- data$spoken_languages == 'en'
data$spoken_languages <- as.character(data$spoken_languages)
data$spoken_languages[idx.of.eng.movies] <- 1
data$spoken_languages[!idx.of.eng.movies] <- 2
data$spoken_languages <- factor(data$spoken_languages)

idx.of.us.movies <- data$original_language == 'en'
data$original_language <- as.character(data$original_language)
data$original_language[idx.of.us.movies] <- 1
data$original_language[!idx.of.us.movies] <- 2
data$original_language <- factor(data$original_language)

idx.of.US.movies <- data$production_countries == 'US'
data$production_countries <- as.character(data$production_countries)
data$production_countries[idx.of.US.movies] <- 1
data$production_countries[!idx.of.US.movies] <- 2
data$production_countries <- factor(data$production_countries)


###drzewo decyzyjne
# Tworzenie pełnego drzewa
tree0 <- rpart(class ~ ., data, minsplit = 0, minbucket = 0, cp = 0)

# Przycinanie drzewa
tree <- prune(tree0, cp = choose.cp(tree0))
rpart.plot(tree, type = 1, extra = 1)


####TODO classification 
library(MASS)
library(e1071)
# Metoda LDA
data.lda <- lda(class ~ ., data)
data.qda <- qda(class ~ ., data)
data.nb <- naiveBayes(class ~ ., data)

# Przewidywanie klas za pomocą metody LDA
# korzystamy z tych samych danych co przy uczeniu
# czyli powtórne podstawienie
data.lda.pred <- predict(data.lda, data)
data.nb.pred <- predict(data.nb, data)
genres <- data$genres
data <- data[ , !(names(data) %in% c( 'genres')) ]

# Budowanie macierzy pomyłek
res.old <- CM.large(data$class, data.lda.pred$class)
res.old <- rbind(res.old, CM.large(data$class, data.nb.pred))
rownames(res.old) <- c("LDA",  "NB")
res.old



##kroswalidacja LDA
##
##
##
##
N <- nrow(data)
# Dzielniki N
div <- which(!(N %% 1:N))

# Wykonanie wielokrotnej CV dla różnych dzielników
mat <- sapply(div[-1], function(d) replicate(5, sum(CV(data, d)) / N))
sapply( c(1:100), function(x) beep())
# Wyznaczanie statystyk
cv.res <- as.data.frame(t(apply(mat, 2, function(x) c(mean(x), sd(x)))))
colnames(cv.res) <- c("mean", "sd")
cv.res$K <- div[-1]
library(Hmisc)

with(cv.res, errbar(K, mean, mean + sd, mean - sd, ylab = 'mean (LDA)'))

##random forest
##las losowy
##
##
##
##
library(randomForest)
library(rpart.plot)
library(rpart)
# 10 realizacji lasu losowego dla PU i PT
# Dane przetasowane
data.rnd <- data[sample(1:N),]
N <- nrow(data.rnd)
PT <- data.rnd[1:N/4,]
PU <- data.rnd[(N/4+1) : N,]
vals <- c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)

PU <- na.exclude(PU)
PT <- na.exclude(PT)
data.no.na <- na.exclude(data)
tab.rf <- sapply(vals, function(i) replicate(5, do.rf(data.no.na, data.no.na, i)))
tab.rf.new <- sapply(vals, function(i) replicate(5, do.rf(PU, PT, i)))
# sapply( c(1:100), function(x) beep())
# Wyznaczanie wartości średnich i odchylenia dla PU
tab.rf.m <- apply(tab.rf, 2, mean)
tab.rf.s <- apply(tab.rf, 2, sd)

# Wyznaczanie wartości średnich i odchylenia dla PT
tab.rf.new.m <- apply(tab.rf.new, 2, mean)
tab.rf.new.s <- apply(tab.rf.new, 2, sd)

# Tworzenie pełnego drzewa
tree0 <- rpart(class ~ ., PU, minsplit = 0, minbucket = 0, cp = 0)

# Przycinanie drzewa
tree <- prune(tree0, cp = choose.cp(tree0))
rpart.plot(tree)
err0 <- err.rate(PT$class, predict(tree0, PT, type = "class"))

err1 <- err.rate(PU$class, predict(tree, PU, type = "class"))
err2 <- err.rate(PT$class, predict(tree, PT, type = "class"))

# Tworzenie wykresów
errbar(vals, tab.rf.m, tab.rf.m + tab.rf.s, tab.rf.m - tab.rf.s, ylim = c(0, 0.6), xlab = "Liczba drzew", ylab = "Bład klasyfikacji", log="x")
errbar(vals, tab.rf.new.m, tab.rf.new.m + tab.rf.new.s, tab.rf.new.m - tab.rf.new.s, add = T, col = "red", errbar.col = "red")

abline(h = err0, lty = 2)
abline(h = err1)
abline(h = err2, col = "red")

title("Las losowy")



##bagging
##
##
##
##
library(adabag)

# Algorytm bagging
do.bag <- function(data, newdata, n) {
  
  bag <- bagging(class ~ ., data = data, mfinal = n)
  bag.pred <- predict.bagging(bag, newdata = newdata)
  
  return(bag.pred$error)
}
vals <- c(1, 5, 10, 20, 50)
# 2 realizacje bagging dla PU i PT
tab.bag <- sapply(vals, function(i) replicate(2, do.bag(data.rnd, data.rnd, i)))
tab.bag.new <- sapply(vals, function(i) replicate(2, do.bag(PU, PT, i)))
sapply( c(1:100), function(x) beep())

# Wyznaczanie wartości średnich i odchylenia dla PU
tab.bag.m <- apply(tab.bag, 2, mean)
tab.bag.s <- apply(tab.bag, 2, sd)

# Wyznaczanie wartości średnich i odchylenia dla PT
tab.bag.new.m <- apply(tab.bag.new, 2, mean)
tab.bag.new.s <- apply(tab.bag.new, 2, sd)

# Tworzenie wykresów
errbar(vals, tab.bag.m, tab.bag.m + tab.bag.s, tab.bag.m - tab.bag.s, ylim = c(0.2, 0.6), xlab = "Liczba drzew", ylab = "Bład klasyfikacji")
errbar(vals, tab.bag.new.m, tab.bag.new.m + tab.bag.new.s, tab.bag.new.m - tab.bag.new.s, add = T, col = "red", errbar.col = "red")

abline(h = err0, lty = 2)
#abline(h = err1)
abline(h = err2, col = "red")

title("Bagging")
##boosting
##boostowanie
##
##

# Algorytm boosting
do.boost <- function(data, newdata, n) {
  
  boost <- boosting(class ~ ., data = data, mfinal = n, control=rpart.control(maxdepth = 1))
  boost.pred <- predict.boosting(boost, newdata = newdata)
  
  return(boost.pred$error)
}
vals <- c(1, 2, 5, 10)
# boosting(class ~ ., data = data, mfinal = 10, control=rpart.control(maxdepth = 1))

# 2 realizacje boosting dla PU i PT
tab.boost <- sapply(vals, function(i) replicate(2, do.boost(data.rnd, data.rnd, i)))
tab.boost.new <- sapply(vals, function(i) replicate(2, do.boost(PU, PT, i)))
sapply( c(1:20), function(x) beep())
# Wyznaczanie wartości średnich i odchylenia dla PU
tab.boost.m <- apply(tab.boost, 2, mean)
tab.boost.s <- apply(tab.boost, 2, sd)

# Wyznaczanie wartości średnich i odchylenia dla PT
tab.boost.new.m <- apply(tab.boost.new, 2, mean)
tab.boost.new.s <- apply(tab.boost.new, 2, sd)

# Tworzenie wykresów
errbar(vals, tab.boost.m, tab.boost.m + tab.boost.s, tab.boost.m - tab.boost.s, ylim = c(0.2, 0.6), xlab = "Liczba drzew", ylab = "Bład klasyfikacji")
errbar(vals, tab.boost.new.m, tab.boost.new.m + tab.boost.new.s, tab.boost.new.m - tab.boost.new.s, add = T, col = "red", errbar.col = "red")

abline(h = err0, lty = 2)
#abline(h = err1)
abline(h = err2, col = "red")
title("boosting")

#pc

test.pc <- princomp(~., cor=T, data=data)
?princomp
