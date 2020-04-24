# Exercise:
# Use the bulti-in dataset iris
# a) Use the lda function to separate the Species using all other variables
ldamod <- lda(Species ~ ., data= iris)
ldamod

# b) Obtain the prediction vs. actual matrix
table(Predicted=predict(ldamod)$class, Actual = iris$Species)
# Accuracy:
mean(predict(ldamod)$class == iris$Species)

# c) Predict the species of a new observation with the following characteristics:
# Sepal.Length = 7.7, Sepal.Width = 3.2, Petal.Length = 5.1, Petal.Width = 2.4
xnew = data.frame(Sepal.Length = 7.7, Sepal.Width = 3.2, Petal.Length = 5.1, Petal.Width = 2.4)
predict(ldamod, xnew)
# Virginica

# d) Repeat with qda
qdamod = qda(Species ~ ., data= iris)
qdamod
table(Predicted=predict(qdamod)$class, Actual = iris$Species)
# Accuracy:
mean(predict(qdamod)$class == iris$Species)
predict(qdamod, xnew)
# Virginica