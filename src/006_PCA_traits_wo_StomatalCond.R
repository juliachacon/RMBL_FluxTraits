# ==============================================================================
# Principal Components Analysis on Plant Traits
# Based on AFIT R Guide: https://afit-r.github.io/pca
# ==============================================================================
# ---- Load Packages ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)

# source table to community and mytheme to PCA plots
source(here("src", "tbl_to_com.R"), echo=TRUE)
source(here("src", "mitema.PCA.R"), echo=TRUE)
load(here("data/processed", "cfluxes_night_clean.rds"))

# ---- Prepare Data ----

# Create composite ID for each row
cfluxes_night$elev_year_plot <- paste(
  cfluxes_night$elevation_m, 
  cfluxes_night$year, 
  cfluxes_night$plot, 
  sep = "_"
)

# Select relevant columns
cfluxes_night_sbs <- cfluxes_night %>%
  select(
    site, year, plot, elevation_m, site_year, unique_block, elev_year_plot,
    height_log, delta13C, NP_ratio, LDMC_log,
    percent_N, percent_P, SLA
  )

# Pivot to long format
cfluxes_night_sbsL <- cfluxes_night_sbs %>%
  pivot_longer(
    cols = c(height_log, delta13C, NP_ratio, LDMC_log, percent_N, percent_P, SLA),
    names_to = "clim",
    values_to = "value"
  )

# Create community matrix (samples × traits)
df_traits <- tbl_to_comm(cfluxes_night_sbsL, elev_year_plot, clim, value)

# Remove known outlier
df_traits <- df_traits[!rownames(df_traits) == "2710_2012_5", ]

# ---- Perform PCA ----
df_traits_pca <- prcomp(df_traits, center = TRUE, scale. = TRUE)

# ---- Variance Explained ----
VE <- df_traits_pca$sdev^2
PVE <- VE / sum(VE)
percentage_labels <- paste0("(", round(PVE * 100, 2), "%)")

# ---- Flip PC2 Axis (for interpretability) ----
df_traits_pca$rotation[, 2] <- -df_traits_pca$rotation[, 2]
df_traits_pca$x[, 2] <- -df_traits_pca$x[, 2]

# ---- Extract PCA Outputs ----
df_out <- as.data.frame(df_traits_pca$x)
df_out$ID <- rownames(df_out)
df_out$Elevation <- as.factor(sapply(df_out$ID, function(x) strsplit(x, "_")[[1]][1]))

# Extract loadings
PCAloadings <- data.frame(
  Variables = rownames(df_traits_pca$rotation),
  df_traits_pca$rotation
)

# ---- Plot PCA ----
pca_traits_plot <- ggplot(df_out, aes(x = PC1, y = PC2, color = Elevation)) +
  geom_point(size = 3, alpha = 0.5) +
  scale_color_manual(values = colorRampPalette(c("turquoise", "darkmagenta", "orange"))(5)) +
  geom_segment(
    data = PCAloadings,
    aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5),
    arrow = arrow(length = unit(0.5, "picas")),
    color = "black",
    size = 0.8
  ) +
  annotate(
    "text",
    x = PCAloadings$PC1 * 5,
    y = PCAloadings$PC2 * 5,
    label = PCAloadings$Variables,
    size = 4,
    color = "black"
  ) +
  xlab(paste("PC1", percentage_labels[1])) +
  ylab(paste("PC2", percentage_labels[2])) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

# Display PCA plot
print(pca_traits_plot)

# ---- Save PCA Scores ----
pcscores_traits <- df_out[, c("PC1", "PC2")] %>%
  rename(PC1trait = PC1, PC2trait = PC2)

# ---- Notes for Interpretation ----

# PC1 (Fast-Slow Plant Strategies):
# Positive loadings: SLA, percent_N, Height, delta13C, percent_P
# Negative loading: LDMC
# High elevations show higher P and faster traits (possibly soil or season-driven)

# PC2 (Chemical Trait Axis):
# NP ratio has strongest influence (negative), hence axis flipped
# Higher NP vs. higher P and delta13C (opposing ends)