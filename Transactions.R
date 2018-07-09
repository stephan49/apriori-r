source("Apriori.R")

# Create transaction data set
T <- data.frame(id=1:7, items=I(list(c("beef", "chicken", "milk"),
                                     c("beef", "cheese"),
                                     c("cheese", "boots"),
                                     c("beef", "chicken", "cheese"),
                                     c("beef", "chicken", "clothes", "cheese", "milk"),
                                     c("chicken", "clothes", "milk"),
                                     c("chicken", "milk", "clothes"))))

# Minimum support
minsup <- .30

# Output all frequent itemsets
(F <- apriori(T))