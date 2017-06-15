# simulate truncated gaussians, compare accuracy of different measures of correlation:
# 0. Known value of correlation coefficient (oracle)
# 1. correlation with full gaussian data
# 2. nonparanormal correlation with full guassian data
# 3. tetrachoric on full dichotomized data
# 4. regular correlation on upper-right quantile (non-truncated) data
# 5. nonparanormal on upper-right quantile (non-truncated) data
# 6. nonparanormal on randomly selected set of points from full distribution, 
#    with sample size equal to upper-right quantile
# 7. regular correlation on randomly selected set of points from full distribution, 
#    with sample size equal to upper-right quantile
# 8. Pearson correlation of all thresholded data
# 9. nonparanormal of all thresholded data
# 10. Pearson correlation of all thresholded, jittered data
# 11. nonparanormal correlation of all thresholded, jittered data

# Comparisons:
# 4 v. 7: How much does regular correlation suffer from truncation, 
#         independent of sample size?
# 5 v. 6: How much does nonparanormal correlation suffer from truncation, 
#         independent of sample size?
# 2 v. 3: Accuracy of tetrachoric v. nonparanormal in ideal conditions (full data)?
# 3 v. 5: Accuracy of tetrachoric v. nonparanormal in truncation conditions?
# 0 v. 5: bias of truncated nonparanormal
# 0 v. 4: bias of truncated regular correlation

#setwd("/Users/lizzie/dissertation_document/")

# libraries:
library(polycor)
library(huge)
library(MASS)
library(ggplot2)

# config:
normvar <- 1
cov_lower <- -0.9
cov_upper <- 0.9
mu <- c(0,0)
thresh_lower <- -1.5
thresh_upper <- 1.5

num_runs <- 1000
sample_size <- 100

trunc_sim <- function(normvar, 
                      cov_lower,
                      cov_upper,
                      mu,
                      thresh_lower,
                      thresh_upper,
                      num_runs,
                      sample_size) {
  
  resultmat <- matrix(nrow=num_runs, ncol=20)
  
  k = 0
  
  nas <- is.na(resultmat[,1])
  while(sum(nas) > 0){
    print(paste("NAs remaining:", sum(nas)))
    indices <- which(is.na(resultmat[,1]))
    for (j in seq_along(indices)){
      i <- indices[j]
      
      x_threshold <- runif(1, thresh_lower, thresh_upper)
      y_threshold <- runif(1, thresh_lower, thresh_upper)
      xy_cov <- runif(1, cov_lower, cov_upper)
      Sigma <- matrix(c(normvar, xy_cov, xy_cov, normvar), nrow=2)
      
      # data
      fulldata <- mvrnorm(n=sample_size, mu, Sigma)
      fulldata <- data.frame(fulldata)
      names(fulldata) <- c("x", "y")
      fulldata$x_binary <- (fulldata$x > x_threshold) * 1
      fulldata$y_binary <- (fulldata$y > y_threshold) * 1
      
      if (((sum(fulldata$y_binary)==0) | (sum(fulldata$x_binary)==0)) |
          ((sum(fulldata$y_binary)==sample_size) |
           (sum(fulldata$x_binary)==sample_size))) next
      contingencies <- table(fulldata$x_binary, fulldata$y_binary)
      if (min(contingencies) < 5) next
      lower_right <- contingencies[2,2]
      if (lower_right < 30) next
      
      k = k + 1
      print(paste("Round ", k, ", filling index: ", i, sep=""))
      
      upper_left <- contingencies[1,1]
      upper_right <- contingencies[1,2]
      lower_left <- contingencies[2,1]
      
      fulldata$x_thresholded <- pmax(fulldata$x, x_threshold)
      fulldata$y_thresholded <- pmax(fulldata$y, y_threshold)
      
      L1 <- qnorm(sum(1-fulldata$x_binary)/sample_size)
      L2 <- qnorm(sum(1-fulldata$y_binary)/sample_size)
      
      # correlations
      reg_cor_fulldata <- cor(fulldata[, c("x", "y")])[1,2]
      t_time <- system.time({ 
        tetchor <- polychor(contingencies, ML=TRUE)})[1]
      reg_cor_trunc <- cor(fulldata[,c("x_thresholded", "y_thresholded")])[1,2]
      kendall_time <- system.time({
        kendall_trunc <- sin((pi / 2) * cor(fulldata[, c("x_thresholded", 
                                                         "y_thresholded")], 
                                            method="kendall")[1,2])})[1]
      c_time_01 <- system.time({
        kendall_trunc_fixed_01 <- kendall.corrected(kendall_trunc, L1, L2, 
                                                    accuracy=0.01) })[1]
      c_time_001 <- system.time({
        kendall_trunc_fixed_001 <- kendall.corrected(kendall_trunc, L1, L2, 
                                                     accuracy=0.001) })[1]
      c_time_0001 <- system.time({
        kendall_trunc_fixed_0001 <- kendall.corrected(kendall_trunc, L1, L2, 
                                                      accuracy=0.0001) })[1]
      
      # results
      results <- c(oracle = xy_cov, 
                   pearson_fulldata = reg_cor_fulldata, 
                   tetrachoric = tetchor,
                   pearson_truncated = reg_cor_trunc,
                   kendall_trunc = kendall_trunc,
                   kendall_corrected_01 = kendall_trunc_fixed_01,
                   kendall_corrected_001 = kendall_trunc_fixed_001,
                   kendall_corrected_0001 = kendall_trunc_fixed_0001,
                   x_threshold = x_threshold,
                   y_threshold = y_threshold,
                   L1 = L1,
                   L2 = L2,
                   # trunc_sample_size = lower_right,
                   correction_time_01 = c_time_01,
                   correction_time_001 = c_time_001,
                   correction_time_0001 = c_time_0001,
                   tetrachoric_time = t_time,
                   upper_left = upper_left,
                   upper_right = upper_right,
                   lower_left = lower_left,
                   lower_right = lower_right) # = sample size of non-truncated points
      
      resultmat[i,] <- results
    }
    nas <- is.na(resultmat[,1])
  }
  
  colnames(resultmat) <- names(results)
  return(resultmat)
}

smallsample_resultmat <- trunc_sim(normvar, cov_lower, cov_upper, mu, 
                                   thresh_lower, thresh_upper, num_runs, 
                                   sample_size=100)

largesample_resultmat <- trunc_sim(normvar, cov_lower, cov_upper, mu, 
                                   thresh_lower, thresh_upper, num_runs, 
                                   sample_size=1000)

# results for large sample:

resultmat <- largesample_resultmat
error_mat <- resultmat 
for (i in 1:ncol(error_mat)){
  error_mat[,i] <- (error_mat[,i] - resultmat[,1]) * sign(resultmat[,1])
}

squared_error_mat <- (error_mat)^2

meansquarederrors <- colMeans(squared_error_mat[,c("pearson_fulldata",
                                                   "tetrachoric",
                                                   "pearson_truncated",
                                                   "kendall_trunc",
                                                   "kendall_corrected_01")])
meansquarederrors <- as.data.frame(meansquarederrors)
row.names(meansquarederrors) <- c("Lower bound", 
                                  "Tetrachoric Correlation",
                                  "Pearson Correlation",
                                  "Kendall's tau estimator", 
                                  "Truncation-corrected Kendall's tau")
meansquarederrors$cor_method <- factor(row.names(meansquarederrors), 
                                       levels=c("Lower bound", 
                                                "Tetrachoric Correlation",
                                                "Pearson Correlation",
                                                "Kendall's tau estimator", 
                                                "Truncation-corrected Kendall's tau"))

meanerrors <- colMeans(error_mat[,c("pearson_fulldata",
                                    "tetrachoric",
                                    "pearson_truncated",
                                    "kendall_trunc",
                                    "kendall_corrected_01")])
meanerrors <- as.data.frame(meanerrors)
row.names(meanerrors) <- c("Lower bound", 
                           "Tetrachoric Correlation",
                           "Pearson Correlation",
                           "Kendall's tau estimator", 
                           "Truncation-corrected Kendall's tau")
meanerrors$cor_method <- factor(row.names(meanerrors), 
                                levels=c("Lower bound", 
                                         "Tetrachoric Correlation",
                                         "Pearson Correlation",
                                         "Kendall's tau estimator", 
                                         "Truncation-corrected Kendall's tau"))

ggplot(data = meanerrors, aes(x=cor_method, y=meanerrors)) +
  geom_bar(stat="identity") + 
  theme_classic() + theme(axis.text.x=element_text(angle=35,hjust=1,vjust=1)) +
  labs(title = "Truncation biases estimates toward zero", 
       x = "Correlation Estimator", y = "Mean Absolute Error")
ggsave(filename="correlation_biased_low.png")


ggplot(data=meansquarederrors, aes(x=cor_method, y=meansquarederrors)) +
  geom_bar(stat="identity") +
  theme_classic() + theme(axis.text.x=element_text(angle=35,hjust=1,vjust=1)) + 
  labs(title = "Truncation increases mean squared error", 
       x = "Correlation Estimator", y = "Mean Squared Error")
ggsave(filename="correlation_squared_error.png")

# results for small sample:

resultmat <- smallsample_resultmat
error_mat <- resultmat 
for (i in 1:ncol(error_mat)){
  error_mat[,i] <- (error_mat[,i] - resultmat[,1]) * sign(resultmat[,1])
}

squared_error_mat <- (error_mat)^2

meansquarederrors <- colMeans(squared_error_mat[,c("pearson_fulldata",
                                                   "tetrachoric",
                                                   "pearson_truncated",
                                                   "kendall_trunc",
                                                   "kendall_corrected_01")])
meansquarederrors <- as.data.frame(meansquarederrors)
row.names(meansquarederrors) <- c("Lower bound", 
                                  "Tetrachoric Correlation",
                                  "Pearson Correlation",
                                  "Kendall's tau estimator", 
                                  "Truncation-corrected Kendall's tau")
meansquarederrors$cor_method <- factor(row.names(meansquarederrors), 
                                       levels=c("Lower bound", 
                                                "Tetrachoric Correlation",
                                                "Pearson Correlation",
                                                "Kendall's tau estimator", 
                                                "Truncation-corrected Kendall's tau"))

meanerrors <- colMeans(error_mat[,c("pearson_fulldata",
                                    "tetrachoric",
                                    "pearson_truncated",
                                    "kendall_trunc",
                                    "kendall_corrected_01")])
meanerrors <- as.data.frame(meanerrors)
row.names(meanerrors) <- c("Lower bound", 
                           "Tetrachoric Correlation",
                           "Pearson Correlation",
                           "Kendall's tau estimator", 
                           "Truncation-corrected Kendall's tau")
meanerrors$cor_method <- factor(row.names(meanerrors), 
                                levels=c("Lower bound", 
                                         "Tetrachoric Correlation",
                                         "Pearson Correlation",
                                         "Kendall's tau estimator", 
                                         "Truncation-corrected Kendall's tau"))

ggplot(data = meanerrors, aes(x=cor_method, y=meanerrors)) +
  geom_bar(stat="identity") + 
  theme_classic() + theme(axis.text.x=element_text(angle=35,hjust=1,vjust=1)) +
  labs(title = "Truncation biases estimates toward zero", 
       x = "Correlation Estimator", y = "Mean Absolute Error")
ggsave(filename="correlation_biased_low_smallsample.png")


ggplot(data=meansquarederrors, aes(x=cor_method, y=meansquarederrors)) +
  geom_bar(stat="identity") +
  theme_classic() + theme(axis.text.x=element_text(angle=35,hjust=1,vjust=1)) + 
  labs(title = "Truncation increases mean squared error", 
       x = "Correlation Estimator", y = "Mean Squared Error")
ggsave(filename="correlation_squared_error_smallsample.png")

# # scatterplot of error of kendall_corrected with sample size in quadrant;
# error_mat <- cbind(error_mat, resultmat[,7:17])
# error_mat <- as.data.frame(error_mat)
# error_mat$min_cell_freq <- pmin(error_mat$upper_left, error_mat$upper_right, 
#                                 error_mat$lower_left, error_mat$lower_right)
# 
# squared_error_dat <- cbind(squared_error_mat, resultmat[,7:17])
# squared_error_dat <- as.data.frame(squared_error_dat)
# squared_error_dat$min_cell_freq <- pmin(squared_error_dat$upper_left, 
#                                         squared_error_dat$upper_right, 
#                                         squared_error_dat$lower_left, 
#                                         squared_error_dat$lower_right)
# 
# max(pmin(squared_error_dat$upper_left, squared_error_dat$upper_right, 
#          squared_error_dat$lower_left, squared_error_dat$lower_right))
# 
# ggplot(error_mat, aes(trunc_sample_size, kendall_trunc_corrected)) + 
#   geom_point(alpha=0.1) + ylab("Error in corrected Kendall correlation") +
#   xlab("Number of points above cutoffs")
# ggsave(filename="Kendall_corrected_error_v_trunc_sample_size_smallsample.png")
# 
# ggplot(error_mat, aes(min_cell_freq, kendall_trunc_corrected)) + 
#   geom_point(alpha=0.1) + ylab("Error in corrected Kendall correlation") +
#   xlab("Number of points in smallest cell")
# ggsave(filename="Kendall_corrected_error_v_min_cell_size_smallsample.png")
# 
# # scatterplot of error of tetrachoric with sample size in quadrant 
# ggplot(error_mat, aes(min_cell_freq, tetrachoric)) + 
#   geom_point(alpha=0.1) + ylab("Error in tetrachoric correlation") +
#   xlab("Number of points in smallest cell")
# ggsave(filename="tetrachoric_error_v_min_cell_size_smallsample.png")
# 
# ggplot(error_mat, aes(trunc_sample_size, tetrachoric)) + 
#   geom_point(alpha=0.1) + ylab("Error in tetrachoric correlation") +
#   xlab("Number of points above cutoffs")
# ggsave(filename="tetrachoric_error_v_trunc_sample_size.png")
# 
# ggplot(squared_error_dat[,], 
#        aes(y=kendall_trunc_corrected, x=tetrachoric, color=min_cell_freq)) + 
#   geom_point(alpha=((min_cell_freq+100)/900)) + 
#   ylab("Squared error in corrected Kendall correlation") +
#   xlab("Squared error in tetrachoric correlation") 
# ggsave(filename="tetrachoric_error_v_kendall_error_smallsample.png")
# 
# # error of kendall_corrected with error of L1 and L2 
# # (compare with kendall_corrected_oracle)
# # 
# #write.csv(resultmat, file="first_results_of_kendall.csv")
# 
