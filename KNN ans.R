install.packages("caret")
library(caret)
library(class)
library(lattice)
library(gmodels)

glass <- read.csv(file.choose())
View(glass)
table(glass$Type)
str(glass)
glass$type <- as.factor(glass$Type)

# table or proportions with more informative labels
round(prop.table(table(glass$Type)) * 100, digits = 1)

# summarize any three numeric features
summary(glass[c("RI", "Na", "Mg")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(1,2,3,4,5))
normalize(c(10, 20, 30, 40, 50))

# normalize the glass data
glass_n <- as.data.frame(lapply(glass[1:9], normalize))
View(glass_n)

# confirm that normalization worked
summary(glass_n[c("RI","Na","Mg")])
View(glass_n)

# create training and test data
set.seed(212)
ind<- sample(2,nrow(glass_n),replace = TRUE, prob = c(0.7,0.3))
glass_train <- glass_n[ind==1, ]
glass_test <- glass_n[ind==2, ]

# create labels for training and test data
set.seed(212)
ind1<- sample(2,nrow(glass_n),replace = TRUE, prob = c(0.7,0.3))
glass_train_labels <- glass[ind1==1,10]
glass_test_labels <- glass[ind1==2,10]


glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=4)
glass_test_pred
table(glass_test_pred,glass_test_labels)

mean(glass_test_pred==glass_test_labels)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = glass_test_labels, y = glass_test_pred,
           prop.chisq=FALSE)

glass_test_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=6)
glass_test_pred
table(glass_test_pred,glass_test_labels)



#################zoo data#####
set.seed(1)
Zoo <- read.csv(file.choose())
View(Zoo)
Zoo = read.table("Zoo", sep=",", header = FALSE)
Zoo = data.frame(Zoo)
names(Zoo) <- c("animal", "hair", "feathers", "eggs", "milk", "airborne",
                "aquatic", "predator", "toothed", "backbone", "breathes", "venomous",
                "fins", "legs", "tail", "domestic", "size", "type")

types <- table(Zoo$type)
Zoo_target <- Zoo[, 18]
Zoo_key <- Zoo[, 1]
Zoo$animal <- NULL

names(types) <- c("mammal", "bird", "reptile", "fish", "amphibian", "insect", "invertebrate")
types

summary(Zoo)

str(Zoo)
####for k=1
k = sqrt(17) +1
m1 <- knn.cv(Zoo, Zoo_target, k, prob = TRUE)
prediction <- m1

cmat <- table(Zoo_target,prediction)

acc <- (sum(diag(cmat)) / length(Zoo_target)) * 100
print(acc)
data.frame(types)

cmat
??cmat

acc
acc#90.09

for k=2; acc=89.10

