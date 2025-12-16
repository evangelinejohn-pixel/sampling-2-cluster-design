# Sampling 2: Cluster Sampling for Population Mean

set.seed(123)

# 1. Create population with clusters
population <- data.frame(
  unit_id = 1:100,
  cluster_id = rep(1:10, each = 10),
  value = rnorm(100, mean = 50, sd = 10)
)

# 2. True population mean
true_mean <- mean(population$value)
M <- length(unique(population$cluster_id))  # total clusters

# 3. Cluster means
cluster_means <- aggregate(value ~ cluster_id, population, mean)
Sc2 <- var(cluster_means$value)  # variance of cluster means

# 4. Required number of clusters (precision-based)
z <- 1.96     # 95% confidence
d <- 2        # margin of error
m_required <- ceiling((z^2 * Sc2) / d^2)

# 5. Select clusters (SRSWOR)
sampled_clusters <- sample(1:M, m_required, replace = FALSE)
sample_data <- population[population$cluster_id %in% sampled_clusters, ]

# 6. Estimate population mean (cluster sampling estimator)
sample_mean <- mean(sample_data$value)

# 7. Sampling bias check
bias <- sample_mean - true_mean

# 8. Cost calculation
C0 <- 500     # fixed cost
C1 <- 200     # cost per cluster
C2 <- 10      # cost per unit
Nc <- 10      # units per cluster

total_cost <- C0 + m_required * C1 + m_required * Nc * C2

# 9. Time calculation
T1 <- 2       # hours per cluster
T2 <- 0.2     # hours per unit

total_time <- m_required * T1 + m_required * Nc * T2

# 10. Results
cat("True Population Mean:", round(true_mean, 2), "\n")
cat("Estimated Mean:", round(sample_mean, 2), "\n")
cat("Sampling Bias:", round(bias, 4), "\n")
cat("Required Clusters:", m_required, "\n")
cat("Expected Cost:", total_cost, "\n")
cat("Expected Time (hours):", total_time, "\n")

# 11. Plot experiment design
plot(1:M, rep(1, M), pch = 19,
     col = ifelse(1:M %in% sampled_clusters, "red", "grey"),
     xlab = "Cluster ID", ylab = "",
     main = "Cluster Sampling Design")
legend("topright",
       legend = c("Sampled Clusters", "Non-sampled Clusters"),
       col = c("red", "grey"), pch = 19)
