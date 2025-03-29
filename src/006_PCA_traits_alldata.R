# Principal Components Analysis of Functional Traits (Clean Version 29th March by Julia)
# Source: AFIT Data Science Lab PCA guide

# ---- Load libraries ----
library(tidyverse)
library(ggplot2)

# ---- Source helper functions ----
source(here("src", "tbl_to_com.R"), echo=TRUE)
source(here("src", "mitema.PCA.R"), echo=TRUE)
cfluxes_night <- readRDS("~/GitHub/2025/RMBL_FluxTraits/data/processed/cfluxes_night_clean.rds")

# ---- Prepare identifiers ----
cfluxes_night$elev_year_plot <- paste(cfluxes_night$elevation_m, cfluxes_night$year, cfluxes_night$plot, sep = "_")

# ---- Select and reshape trait data ----
cfluxes_night_sbs <- cfluxes_night %>% 
  select(site, year, plot, elevation_m, site_year, unique_block, elev_year_plot, 
         height_log, delta13C, NP_ratio, LDMC_log, percent_N, percent_P, SLA, stomatal_cond)

cfluxes_night_sbsL <- cfluxes_night_sbs %>% 
  pivot_longer(cols = c(height_log, delta13C, NP_ratio, LDMC_log, percent_N, percent_P, SLA, stomatal_cond),
               names_to = "clim", values_to = "value")

# ---- Transform to community matrix ----
df_traits <- tbl_to_comm(cfluxes_night_sbsL, elev_year_plot, clim, value)

# ---- Remove outlier ----
df_traits <- df_traits[!rownames(df_traits) == "2710_2012_5", ]

# ---- PCA ----
df_traits_pca <- prcomp(df_traits, center = TRUE, scale. = TRUE)

# ---- Variance explained ----
VE <- df_traits_pca$sdev^2
PVE <- VE / sum(VE)

# ---- Scree plot ----
PVEplot <- qplot(1:length(PVE), PVE) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Proportion of Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# ---- Cumulative PVE plot ----
cumPVE <- qplot(1:length(PVE), cumsum(PVE)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Cumulative Variance Explained") + 
  ggtitle("Cumulative Scree Plot") +
  ylim(0, 1)

# ---- Flip PC2 sign for easier interpretation ----
df_traits_pca$rotation[, 2] <- -df_traits_pca$rotation[, 2]
df_traits_pca$x[, 2] <- -df_traits_pca$x[, 2]

# ---- Extract scores and loadings ----
df_out <- as.data.frame(df_traits_pca$x)
PCAloadings <- data.frame(Variables = rownames(df_traits_pca$rotation), df_traits_pca$rotation)

# ---- Format axis labels ----
percentage <- round(PVE * 100, 2)
axis_labels <- paste0("PC", 1:length(percentage), " (", percentage, "%)")

# ---- Add ID and elevation ----
df_out$ID <- rownames(df_out)
df_out$Elevation <- factor(sapply(df_out$ID, function(x) strsplit(x, "_")[[1]][1]))
rownames(df_out) <- df_out$ID

# ---- PCA biplot ----
pca_traits_plot <- ggplot(df_out, aes(x = PC1, y = PC2, col = Elevation)) +
  geom_point(size = 3, alpha = 0.5) +
  scale_color_manual(values = colorRampPalette(c("turquoise", "darkmagenta", "orange"))(5)) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5),
               arrow = arrow(length = unit(0.5, "picas")), color = "black", size = 0.8) +
  annotate("text", x = PCAloadings$PC1 * 5, y = PCAloadings$PC2 * 5,
           label = PCAloadings$Variables, color = "black", size = 4) +
  xlab(axis_labels[1]) + ylab(axis_labels[2]) +
  theme_classic() +
  theme(axis.title = element_text(size = 12, color = "black"),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 10))

print(PVEplot)
print(cumPVE)
print(pca_traits_plot)

# Note: I flipped the PC2 axis for interpretability

# ---- Rename PC scores ----
pcscores_traits <- df_out[, 1:2] %>%
  rename(PC1trait = PC1, PC2trait = PC2)

# ---- Interpretation Notes ----
# PC1: Fast-slow plant economics spectrum. High SLA, N, Height, delta13C, and percent_P indicate fast strategy; LDMC is conservative.
# PC2: Chemical trait axis. NP ratio (high influence) vs. P content and delta13C. Sign was flipped for interpretability.

# eje 1: fast-slow continuum. SLA, N, Height positive (más gastadoras, faster go positive vs LDMC, more conservative than go negative). Also delta 13 C varia en positivo, peor eficiencia en el uso del agua. Percent P go positive. Higher altitudes have higher P content. Higher altitudes here have a shorter growwing season (Korner); and a faster strategy. Maybe there is also something related to the soils that influence P. Anyway. P content covaries with the fast spectrum.
# eje 2: chemical traits. NP ratio axis covaries Negative. It has the higher score so I have change the signal tu make it positive and easier to interpret. On the other side P content and delta 13C. NP ratio higher vs higher P and Delta 13.
# mirar esto para delta 15
# https://link.springer.com/article/10.1007/s11104-015-2542-1
# NP ratio variation with climate
# NP link with photosynthesis
# mirar paper de Brian/Reich