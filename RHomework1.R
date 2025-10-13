# pick one of these: radius, texture, perimeter, area, smoothness,
#                    compactness, concavity, concavepoints, symmetry, fractaldim
measurement <- "radius"

col_mean  <- paste0("mean_",  measurement)
col_se    <- paste0("SE_",    measurement)
col_worst <- paste0("worst_", measurement)

# pull the three numeric vectors
v_mean  <- wpbc[[col_mean]]
v_se    <- wpbc[[col_se]]
v_worst <- wpbc[[col_worst]]

# drop NAs
v_mean  <- v_mean[!is.na(v_mean)]
v_se    <- v_se[!is.na(v_se)]
v_worst <- v_worst[!is.na(v_worst)]

# 3 histograms side-by-side
op <- par(mfrow = c(1,3), mar = c(4,4,2,1))
hist(v_mean,  breaks = 30, main = sprintf("Mean %s",  measurement), xlab = measurement)
hist(v_se,    breaks = 30, main = sprintf("SE %s",    measurement), xlab = measurement)
hist(v_worst, breaks = 30, main = sprintf("Worst %s", measurement), xlab = measurement)
par(op)

# make a copy so we don't mutate wpbc
df_small <- wpbc

# drop ID if it exists
if ("ID" %in% names(df_small)) df_small$ID <- NULL

# drop all SE_ and worst_ columns
drop_cols <- grepl("^SE_", names(df_small)) | grepl("^worst_", names(df_small))
df_small <- df_small[, !drop_cols, drop = FALSE]

# keep only numeric columns for pairs()
num_cols <- sapply(df_small, is.numeric)
df_small_num <- df_small[, num_cols, drop = FALSE]

# sanity check
str(df_small_num)

pairs(df_small_num,
      main = "WPBC: Scatterplot Matrix (mean features only)",
      pch = 19, cex = 0.5, col = rgb(0,0,0,0.4))

cor_mat <- cor(df_small_num, use = "pairwise.complete.obs")
# list top absolute correlations (excluding self-pairs)
top_pairs <- which(abs(cor_mat) > 0.85 & upper.tri(cor_mat), arr.ind = TRUE)
data.frame(
  var1 = rownames(cor_mat)[top_pairs[,1]],
  var2 = colnames(cor_mat)[top_pairs[,2]],
  r    = round(cor_mat[top_pairs], 3)
)[order(-abs(cor_mat[top_pairs])), ]

## 1) Pick variables and drop missing rows
vars <- c("mean_perimeter","mean_radius")
stopifnot(all(vars %in% names(wpbc)))
d <- wpbc[complete.cases(wpbc[ , vars]), vars]

## 2) Fit simple linear regression: perimeter ~ radius
fit <- lm(mean_perimeter ~ mean_radius, data = d)

## 3) Scatter plot + fitted line
plot(d$mean_radius, d$mean_perimeter,
     pch = 19, cex = 0.6, col = rgb(0,0,0,0.5),
     xlab = "Mean radius", ylab = "Mean perimeter",
     main = "Perimeter (mean) vs Radius (mean) with fitted line")
abline(fit, col = "red", lwd = 2)

## 4) Report fitted coefficients and R^2
coefs <- coef(fit)
cat("Intercept:", round(coefs[1], 4), "\n")
cat("Slope    :", round(coefs[2], 4), "\n")
cat("R^2      :", round(summary(fit)$r.squared, 4), "\n")



