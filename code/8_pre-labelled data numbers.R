set.seed(1234)
numbers <- sample(1:100000, 1500, replace = FALSE)
write.csv(numbers, file = "numbers.csv", row.names = FALSE)
