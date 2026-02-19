suppressPackageStartupMessages({
  library(ggplot2)
  library(ggrepel)
})


# 1. load the results of burden -------------------------------------------

load("Data_for_Alert_Value.RData")

# Due to the size limitation of the uploaded files, we randomly selected a portion of our data as a subset. 
# The final result may not be consistent with the main text. 
# This code is only presented as an example, and the final result should be referred to the published one.

thresholds <- seq(75, 295, by = 1)
year_idx <- year - 1999
pop_vec <- pop[, year_idx]

# ====== Sanity check ======
stopifnot(
  nrow(pm_df) == length(pop_vec),
  all(dim(pm_df) == dim(Dspoint)),
  all(dim(pm_df) == dim(results[["lower"]])),
  all(dim(pm_df) == dim(results[["upper"]]))
)


# 2. function to calculate the alter value --------------------------------

compute_df <- function(pm_df, pop_vec, Ds_point, Ds_lower, Ds_upper, thresholds) {
  n_day  <- ncol(pm_df)
  n_grid <- length(pop_vec)
  
  pop_mat <- matrix(pop_vec, nrow = n_grid, ncol = n_day)
  
  ds_pop   <- numeric(length(thresholds))
  ds_death <- numeric(length(thresholds))
  ds_lo    <- numeric(length(thresholds))
  ds_hi    <- numeric(length(thresholds))
  
  for (i in seq_along(thresholds)) {
    mask <- pm_df > thresholds[i]
    ds_pop[i]   <- sum(ifelse(mask, pop_mat,  0), na.rm = TRUE)
    ds_death[i] <- sum(ifelse(mask, Ds_point, 0), na.rm = TRUE)
    ds_lo[i]    <- sum(ifelse(mask, Ds_lower, 0), na.rm = TRUE)
    ds_hi[i]    <- sum(ifelse(mask, Ds_upper, 0), na.rm = TRUE)
  }
  
  # append endpoint (300, 0) to match your original
  data.frame(
    Threshold    = c(thresholds, 300),
    ExposureProp = c(ds_pop   / max(ds_pop),   0),
    DeathProp    = c(ds_death / max(ds_death), 0),
    Lower        = c(ds_lo    / max(ds_lo),    0),
    Upper        = c(ds_hi    / max(ds_hi),    0)
  )
}

df <- compute_df(
  pm_df      = pm_df,
  pop_vec    = pop_vec,
  Ds_point   = Dspoint,
  Ds_lower   = results[["lower"]],
  Ds_upper   = results[["upper"]],
  thresholds = thresholds
)

# 3. main calculation -----------------------------------------------------
idx_best <- which.max(abs(df$DeathProp - df$ExposureProp) / sqrt(2))
df_best  <- df[idx_best, , drop = FALSE]
idx_max <- which.max(df$DeathProp)
df_max  <- df[idx_max, , drop = FALSE]

df_125 <- if (any(df$Threshold == 125)) df[df$Threshold == 125, , drop = FALSE] else NULL
df_150 <- if (any(df$Threshold == 150)) df[df$Threshold == 150, , drop = FALSE] else NULL

# setting color
opt_col <- if (exists("professional_colors") && !is.null(professional_colors[["optimal_point"]])) {
  professional_colors[["optimal_point"]]
} else "#D62728"

ci_col <- if (exists("pal") && !is.null(pal[["Gamma"]])) pal[["Gamma"]] else "#3C7FB1"

# plot
p <- ggplot(df, aes(x = ExposureProp, y = DeathProp)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray70", linewidth = 0.8) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper),
              fill = "pink", alpha = 1, color = "pink", linewidth = 2.5) +
  geom_path(aes(color = ExposureProp), linewidth = 1.2) +
  scale_color_gradient(low = "blue", high = "red") +
  coord_fixed() +
  labs(
    subtitle = "Percentage of person-days interfered by\nan alert exposure level (%)",
    x = NULL, y = NULL
  ) +
  scale_x_continuous(labels = function(x) paste0(round(x * 100))) +
  scale_y_continuous(
    labels = function(y) paste0(round(y * 100)),
    position = "left",
    sec.axis = dup_axis(
      name   = "Percentage of avoided premature deaths\nby an alert exposure level (%)",
      labels = NULL
    )
  ) +
  theme_minimal() +
  theme(
    axis.title.y.left   = element_blank(),
    axis.text           = element_text(size = 14),
    axis.title.x        = element_text(size = 20),
    plot.subtitle       = element_text(size = 30, hjust = 0.5),
    plot.margin         = margin(5.5, 60, 5.5, 5.5),
    legend.position     = "none",
    axis.text.y.right   = element_blank(),
    axis.ticks.y.right  = element_blank(),
    axis.title.y.right  = element_text(size = 30, angle = 90, vjust = 0.5)
  ) +
  # best point projections
  geom_segment(
    data = df_best,
    aes(x = ExposureProp, xend = ExposureProp, y = 0, yend = DeathProp),
    linetype = "dashed", color = opt_col, linewidth = 0.8, alpha = 0.7
  ) +
  geom_segment(
    data = df_best,
    aes(x = 0, xend = ExposureProp, y = DeathProp, yend = DeathProp),
    linetype = "dashed", color = opt_col, linewidth = 0.8, alpha = 0.7
  ) +
  geom_point(data = df_best, color = "red", size = 3) +
  geom_label(
    data = df_best,
    aes(x = ExposureProp, y = -0.02, label = paste0(round(ExposureProp * 100, 1), "%")),
    color = "red", fill = "white", size = 3, label.size = 0.5,
    label.padding = unit(0.2, "lines"), fontface = "bold"
  ) +
  geom_label(
    data = df_best,
    aes(x = -0.04, y = DeathProp, label = paste0(round(DeathProp * 100, 1), "%")),
    color = "red", fill = "white", size = 3, label.size = 0.5,
    label.padding = unit(0.2, "lines"), fontface = "bold"
  ) +
  geom_text_repel(
    data = df_best,
    aes(label = paste0(df_best$Threshold[1], " (Optimal value)")),
    color = "red", size = 5, fontface = "bold",
    nudge_x = 0.148, nudge_y = 0, segment.color = NA
  ) +
  # reference points
  geom_point(data = df_max, color = "blue", size = 3) +
  geom_text_repel(
    data = df_max, aes(label = "75 (WHO AQG IT-1)"),
    color = "blue", size = 5, fontface = "bold",
    nudge_x = 0.055, nudge_y = 0.024, segment.color = NA
  )

if (!is.null(df_125)) {
  p <- p +
    geom_point(data = df_125, color = "purple", size = 3) +
    geom_text_repel(
      data = df_125, aes(label = "125 (Alert value in U.S.)"),
      color = "purple", size = 5, fontface = "bold",
      nudge_x = 0.13, nudge_y = -0.01, segment.color = NA
    )
}
if (!is.null(df_150)) {
  p <- p +
    geom_point(data = df_150, color = "orange", size = 3) +
    geom_text_repel(
      data = df_150, aes(label = "(Alert value in China) 150"),
      color = "orange", size = 5, fontface = "bold",
      nudge_x = -0.31, nudge_y = 0.01, segment.color = NA
    )
}

# optional 
if (exists("dens_df")) {
  p <- p + geom_area(
    data = dens_df, inherit.aes = FALSE,
    aes(x = x_prop, y = y_scaled, fill = dist),
    alpha = 0.18, color = NA
  )
}

# optional
if (exists("ci_x") && exists("band_h")) {
  p <- p +
    geom_segment(inherit.aes = FALSE,
                 aes(x = ci_x[1], xend = ci_x[1], y = 0, yend = band_h * 0.92),
                 linetype = "dashed", linewidth = 0.7, color = ci_col) +
    geom_segment(inherit.aes = FALSE,
                 aes(x = ci_x[2], xend = ci_x[2], y = 0, yend = band_h * 0.92),
                 linetype = "dashed", linewidth = 0.7, color = ci_col)
}

p
