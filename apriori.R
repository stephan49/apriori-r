# Apriori algorithm
apriori <- function(T) {
  C1 <<- init.pass(T)  # First pass over T
  F1 <<- C1[c(which(C1$count/nrow(T) >= minsup)),]  # Determine frequent itemsets
  
  # Subsequent passes over T
  k <- 2
  Fk_1 <- paste0("F", k-1)
  
  while (nrow(get(Fk_1)) != 0) { 
    assign(paste0("C", k), candidate.gen(get(Fk_1)))  # Candidate generation function
    
    # Check if each candidate itemset is in transaction dataset
    Ck <- get(paste0("C", k))
    try({
      Ck$count <- 0
      l <- length(Ck$items[[1]])
    }, silent = TRUE)
    
    for (i in 1:nrow(T)) {
      for (j in 1:nrow(Ck)) {
        if (length(intersect(T$items[[i]], try(Ck$items[[j]], silent = TRUE))) == l) {
          Ck$count[j] <- Ck$count[j] + 1 
        }
      }
    }
    assign(paste0("C", k), Ck, envir = .GlobalEnv)
    
    # Determine frequent itemsets
    assign(paste0("F", k), Ck[c(which(Ck$count/nrow(T) >= minsup)),], envir = .GlobalEnv)
    
    k <- k + 1
    Fk_1 <- paste0("F", k-1)
  }
  
  # Output set F of all frequent itemsets
  F <- data.frame(items = character(), count = numeric(), stringsAsFactors = FALSE)
  F <- do.call(rbind, lapply( paste0("F", 1:(k-1)) , get))
  rownames(F) <- 1:nrow(F)
  return(F)
}


# Initial pass function
init.pass <- function(T) {
  cnt = 1
  C1 <- data.frame(items = character(), count = numeric(), stringsAsFactors = FALSE)
  
  # Count support of individual items
  for (i in T$items) {
    for (j in i) {
      if (j %in% C1$items) {
        C1[(j == C1$items),]$count <- C1[(j == C1$items),]$count + 1
      } else {
        C1[cnt,]$items <- j
        C1[cnt,]$count <- 1
        cnt = cnt + 1
      }
    }
  }
  
  C1 <- C1[order(C1$items),]  # Sort items lexicographically
  return(C1)
}


# Candidate generation function
candidate.gen <- function(Fk_1) {
  l <- length(Fk_1$items[[1]])
  cnt = 1
  Ck <- data.frame(items = I(list()), count = numeric(), stringsAsFactors = FALSE)
  
  for (i in 1:nrow(Fk_1)) {
    for (j in i:nrow(Fk_1)) {
      if (i != j) {
        if (all((Fk_1$items[[i]]==Fk_1$items[[j]])[-l]) == TRUE &
            (Fk_1$items[[i]]==Fk_1$items[[j]])[l] == FALSE) {    # Find pairs that differ only by last item
          Ck[cnt, 1] <- c("")
          Ck$items[[cnt]] <- c(Fk_1$items[[i]], Fk_1$items[[j]][l])  # Join two itemsets and add to candidates
          
          s <- combn(Ck$items[[cnt]], l, simplify = FALSE) 
          if (all(s %in% Fk_1$items) == FALSE) {
            Ck <- Ck[-cnt,]  # Delete candidate if a subset is not in k-1 frequent itemset
          } else {
            cnt = cnt + 1
          }
        }
      }
    }
  }
  return(Ck)
}
