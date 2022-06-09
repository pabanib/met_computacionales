unzip(zipfile = 'Datos/socialneighbor.zip')
social <- read.csv('socialneighbor.csv')

set.seed(123)
noise.covars <- matrix(data = runif(nrow(social) * 13), 
                       nrow = nrow(social), ncol = 13)
noise.covars <- data.frame(noise.covars)
names(noise.covars) <- c("noise1", "noise2", "noise3", "noise4", "noise5", "noise6",
                         "noise7", "noise8", "noise9", "noise10", "noise11", "noise12","noise13")

working <- cbind(social, noise.covars)

set.seed(333)
working <-  working[sample(nrow(social), 20000), ]

# Pick a selection of covariates
# If we have a lot of data and computation power, it is suggested that
# we include all covariates and use regularization. This suggestion is
# based on the observation that it's much easier to fix the overfitting 
# problem than to fix the underfitting problem.
covariate.names <- c("yob", "hh_size", "sex", "city", "g2000","g2002", "p2000", "p2002", "p2004"
                     ,"totalpopulation_estimate","percent_male","median_age", "percent_62yearsandover"
                     ,"percent_white", "percent_black", "median_income",
                     "employ_20to64", "highschool", "bach_orhigher","percent_hispanicorlatino",
                     "noise1", "noise2", "noise3", "noise4", "noise5", "noise6",
                     "noise7", "noise8", "noise9", "noise10", "noise11", "noise12","noise13")


names(working)[names(working)=="outcome_voted"] <- "Y"
Y <- working[["Y"]]

names(working)[names(working)=="treat_neighbors"] <- "W"

W <- working[["W"]]
covariates <- working[covariate.names]

# some algorithms require our covariates be scaled
# scale, with default settings, will calculate the mean and standard deviation of the entire vector, 
# then "scale" each element by those values by subtracting the mean and dividing by the sd
covariates.scaled <- scale(covariates)
processed.unscaled <- data.frame(Y, W, covariates)
processed.scaled <- data.frame(Y, W, covariates.scaled)

# Creemos una función que nos devuelvo dos bd

train_test <- function(bd, porcentaje) {
    train <- sample(nrow(bd), round(nrow(bd)*porcentaje), replace = FALSE)
    train.bd <- bd[train,]
    test.bd <- bd[-train,]
    return(list(train.bd,test.bd))
    }

set.seed(45)
lista_train_test <- train_test(processed.scaled, 0.9)
y.train <- as.matrix(lista_train_test[[1]]$Y, ncol = 1)
y.test <- as.matrix(lista_train_test[[2]]$Y, ncol = 1)
X.train <- lista_train_test[[1]]
X.test <- lista_train_test[[2]]


sumx <- paste(covariate.names, collapse = " + ")  # "X1 + X2 + X3 + ..." for substitution later
interx <- paste(" (",sumx, ")^2", sep="")  # "(X1 + X2 + X3 + ...)^2" for substitution later
linear <- paste("Y", sumx, sep = "~")
linear <- as.formula(linear)
linear.inter <- (paste("Y", interx, sep = "~"))
linear.inter <- as.formula(linear.inter)