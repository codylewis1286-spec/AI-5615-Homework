# =========================================================
# Problem 2 – Basic Analysis in R  (WPBC dataset)
# =========================================================

# -------------------------
# 2A – Preprocessing
# -------------------------
# Read wpbc.data (treat "?" as missing), name columns, fix types, and
# report complete summary statistics.

# Path to your data file (put the file in getwd() or give a full path)
fn <- "/Users/m298134/Desktop/AIHC 5615/data/wpbc.data"

# Read raw CSV-like file (no header), "?" -> NA
raw <- read.table(fn, header = FALSE, sep = ",",
                  na.strings = "?", stringsAsFactors = FALSE, strip.white = TRUE)

# Canonical column names (version WITHOUT an ID column)
wpbc_names_no_id <- c(
  "status","time",
  "mean_radius","mean_texture","mean_perimeter","mean_area","mean_smoothness",
  "mean_compactness","mean_concavity","mean_concavepoints","mean_symmetry","mean_fractaldim",
  "SE_radius","SE_texture","SE_perimeter","SE_area","SE_smoothness",
  "SE_compactness","SE_concavity","SE_concavepoints","SE_symmetry","SE_fractaldim",
  "worst_radius","worst_texture","worst_perimeter","worst_area","worst_smoothness",
  "worst_compactness","worst_concavity","worst_concavepoints","worst_symmetry","worst_fractaldim",
  "tsize","pnodes"
)

# Assign names, handling files that optionally include an ID as first column
if (ncol(raw) == length(wpbc_names_no_id)) {
  names(raw) <- wpbc_names_no_id
} else if (ncol(raw) == length(wpbc_names_no_id) + 1) {
  names(raw) <- c("ID", wpbc_names_no_id)
} else {
  stop(sprintf("Unexpected column count: %d (expected %d or %d).",
               ncol(raw), length(wpbc_names_no_id), length(wpbc_names_no_id)+1))
}

# Convert types: status as factor (N/R), everything else numeric (except optional ID)
wpbc <- raw
if (!"status" %in% names(wpbc)) stop("No 'status' column found.")
wpbc$status <- factor(wpbc$status, levels = c("N","R"))

num_cols <- setdiff(names(wpbc), c("status","ID"))
wpbc[num_cols] <- lapply(wpbc[num_cols], function(x) suppressWarnings(as.numeric(x)))

# Missing-value report (counts and percents)
na_counts <- sapply(wpbc, function(x) sum(is.na(x)))
na_perc   <- round(100 * sapply(wpbc, function(x) mean(is.na(x))), 1)
cat("---- Missing values per column ----\n")
print(data.frame(variable = names(wpbc), n_missing = na_counts, pct_missing = na_perc), row.names = FALSE)

# Complete summary statistics (base R)
cat("\n---- Summary statistics (base R) ----\n")
print(summary(wpbc))

# -------------------------
# 2B – Histograms for Mean / SE / Worst of one measurement
# -------------------------
# Pick ONE base measurement (radius, texture, perimeter, area, smoothness,
# compactness, concavity, concavepoints, symmetry, fractaldim) and plot histograms.

measurement <- "radius"  # <-- change if you want another family

col_mean  <- paste0("mean_",  measurement)
col_se    <- paste0("SE_",    measurement)
col_worst <- paste0("worst_", measurement)

stopifnot(all(c(col_mean, col_se, col_worst) %in% names(wpbc)))

v_mean  <- wpbc[[col_mean]]
v_se    <- wpbc[[col_se]]
v_worst <- wpbc[[col_worst]]

# Drop NAs
v_mean  <- v_mean[!is.na(v_mean)]
v_se    <- v_se[!is.na(v_se)]
v_worst <- v_worst[!is.na(v_worst)]

# Plot three histograms side-by-side
op <- par(mfrow = c(1,3), mar = c(4,4,2,1))
hist(v_mean,  breaks = 30, main = paste("Mean",  measurement),  xlab = measurement, col = "grey")
hist(v_se,    breaks = 30, main = paste("SE",    measurement),  xlab = measurement, col = "grey")
hist(v_worst, breaks = 30, main = paste("Worst", measurement),  xlab = measurement, col = "grey")
par(op)

# -------------------------
# 2C – Pairs plot on reduced dataset
# -------------------------
# Make a new data frame with all SE_* and worst_* columns removed, and remove ID if present.
# Use pairs() to scatterplot every pair of variables.
#Can I explain this? Yes, mean area and mean radius seem to be strongly related because as the radious of a circle increses, so does the area.

df_small <- wpbc
if ("ID" %in% names(df_small)) df_small$ID <- NULL

to_drop <- grepl("^SE_", names(df_small)) | grepl("^worst_", names(df_small))
df_small <- df_small[, !to_drop, drop = FALSE]

# Keep only numeric columns for pairs() (this drops 'status' automatically)
is_num <- sapply(df_small, is.numeric)
df_small_num <- df_small[, is_num, drop = FALSE]

# Scatterplot matrix
pairs(df_small_num,
      main = "WPBC: Scatterplot Matrix (mean features)",
      pch = 19, cex = 0.5, col = rgb(0,0,0,0.4))

# Optional: show strongest linear relationships (|r| > 0.85)
cor_mat <- cor(df_small_num, use = "pairwise.complete.obs")
idx <- which(abs(cor_mat) > 0.85 & upper.tri(cor_mat), arr.ind = TRUE)
if (nrow(idx)) {
  cat("\nTop strong correlations (|r|>0.85):\n")
  strong <- data.frame(
    var1 = rownames(cor_mat)[idx[,1]],
    var2 = colnames(cor_mat)[idx[,2]],
    r    = round(cor_mat[idx], 3)
  )
  strong <- strong[order(-abs(strong$r)), ]
  print(strong, row.names = FALSE)
}

# -------------------------
# 2D – Linear regression: mean_perimeter ~ mean_radius
# -------------------------
vars <- c("mean_perimeter","mean_radius")
stopifnot(all(vars %in% names(wpbc)))
d <- wpbc[complete.cases(wpbc[, vars]), vars]

fit <- lm(mean_perimeter ~ mean_radius, data = d)

# Scatter + fitted line
plot(d$mean_radius, d$mean_perimeter,
     pch = 19, cex = 0.6, col = rgb(0,0,0,0.5),
     xlab = "Mean radius", ylab = "Mean perimeter",
     main = "Perimeter (mean) vs Radius (mean) with fitted line")
abline(fit, col = "red", lwd = 2)

# Report coefficients and R^2
co <- coef(fit)
cat("\n--- Linear model: mean_perimeter ~ mean_radius ---\n")
cat(sprintf("Intercept: %.4f\n", co[1]))
cat(sprintf("Slope    : %.4f  (Δ perimeter per +1 radius)\n", co[2]))
cat(sprintf("R^2      : %.4f\n", summary(fit)$r.squared))

# Explanation (for your comments/markdown):
# • Slope: expected increase in mean perimeter for a 1-unit increase in mean radius.
#   For roughly circular objects, perimeter ≈ 2π·radius ≈ 6.283·radius, so the slope
#   should be in that neighborhood (nuclei aren’t perfect circles, so it won’t be exact).
# • Intercept: model’s predicted perimeter at radius = 0 (not physically meaningful here,
#   but needed to place the regression line).
# • R^2: typically high, because both variables are size measures (strong geometric link).
