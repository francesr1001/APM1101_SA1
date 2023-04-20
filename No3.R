
p <- 0.6

set.seed(123)
par(mfrow = c(1,2),bg = "#ccccff")

searches <- rgeom(10000, p) + 1
hist(searches, breaks = seq(0.5, max(searches) + 0.5, by = 1), 
     main = "Distribution of Searches Needed to Find Key Phrase", 
     xlab = "Number of Searches")

searches_3 <- searches[searches > 3]

hist(searches_3, breaks = 10, 
     main = "Simulated Conditional Distribution of Searches", 
     xlab = "Number of Searches Less Than 3 (Srch>3)")

mean_X <- mean(searches)
var_X <- var(searches)

cat("Mean of searches needed to find key phrase: ", mean_X, "\n")
cat("Variance of searches needed to find key phrase: ", var_X, "\n")

con_searches <- searches[searches > 3]
new_mean <- mean(con_searches)
new_var <- var(con_searches)

# CDF
cat("Mean of searches needed to find key phrase given 3 unsuccessful searches: ", new_mean, "\n")
cat("Variance of searches needed to find key phrase given 3 unsuccessful searches: ", new_var, "\n")

# a )
p_x4_given_xgt3 <- sum(searches == 4 & searches > 3) / sum(searches > 3)
p_x1 <- sum(searches == 1) / length(searches)

# result
cat("Estimate of P(X=4|X>3): ", p_x4_given_xgt3, "\n")
cat("Estimate of P(X=1): ", p_x1, "\n")


# b)
p_x5_given_xgt3 <- sum(searches == 5 & searches > 3) / sum(searches > 3)
p_x2 <- sum(searches == 2) / length(searches)

# result
cat("Estimate of P(X=4|X>3): ", p_x5_given_xgt3, "\n")
cat("Estimate of P(X=1): ", p_x2, "\n")


