# PCA of Climate Variables for RMBL Sites
# clean version 29th March 2025 by Julia

# ---- Load custom functions ----
source(here("src", "tbl_to_com.R"), echo=TRUE)
source(here("src", "mitema.PCA.R"), echo=TRUE)
cfluxes_night <- readRDS("~/GitHub/2025/RMBL_FluxTraits/data/processed/cfluxes_night_clean.rds")


# ---- Prepare identifiers ----
cfluxes_night$elev_year_plot <- paste(
  cfluxes_night$elevation_m, cfluxes_night$year, cfluxes_night$plot,
  sep = "_"
)

# ---- Select and pivot climate variables ----
cfluxes_night_sbs <- cfluxes_night %>%
  select(
    site, year, plot, elevation_m, site_year, unique_block, elev_year_plot,
    tmean_c_gs, vpdmin_hPa_gs, ppt_mm_gs, ppt_mm,
    tmean_c, vpdmin_hPa,
    spei_s03_m6, spei_s01_m6, spei_s03_m7, spei_s04_m7
  )

cfluxes_night_sbsL <- cfluxes_night_sbs %>%
  pivot_longer(
    cols = c(tmean_c_gs, vpdmin_hPa_gs, ppt_mm_gs, ppt_mm, tmean_c, vpdmin_hPa,
             spei_s03_m6, spei_s01_m6, spei_s03_m7, spei_s04_m7),
    names_to = "clim",
    values_to = "value"
  )

# ---- Convert to community format ----
df_clim <- tbl_to_comm(cfluxes_night_sbsL, elev_year_plot, clim, value)

# ---- Remove outlier ----
df_clim <- df_clim[!rownames(df_clim) == "2710_2012_5", ]

# ---- Run PCA ----
df_clim_pca <- prcomp(df_clim, center = TRUE, scale. = TRUE)

# ---- Extract scores and loadings ----
df_out <- as.data.frame(df_clim_pca$x)
PCAloadings <- data.frame(Variables = rownames(df_clim_pca$rotation), df_clim_pca$rotation)

# ---- Variance explained ----
VE <- df_clim_pca$sdev^2
PVE <- VE / sum(VE)
percentage <- round(PVE * 100, 2)
axis_labels <- paste0("PC", 1:length(percentage), " (", percentage, "%)")

# ---- Add metadata ----
df_out$ID <- rownames(df_out)
df_out$Elevation <- factor(sapply(df_out$ID, function(x) strsplit(x, "_")[[1]][1]))
rownames(df_out) <- df_out$ID

# ---- PCA biplot ----
pca_clim_plot <- ggplot(df_out, aes(x = PC1, y = PC2, col = Elevation)) +
  geom_point(size = 3, alpha = 0.5) +
  scale_color_manual(values = colorRampPalette(c("turquoise", "darkmagenta", "orange"))(5)) +
  geom_segment(data = PCAloadings,
               aes(x = 0, y = 0, xend = PC1 * 5, yend = PC2 * 5),
               arrow = arrow(length = unit(0.5, "picas")),
               color = "black", size = 0.8) +
  annotate("text",
           x = PCAloadings$PC1 * 5,
           y = PCAloadings$PC2 * 5,
           label = PCAloadings$Variables,
           color = "black", size = 4) +
  xlab(axis_labels[1]) +
  ylab(axis_labels[2]) +
  theme_classic() +
  theme(
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 10),
    legend.text = element_text(size = 10)
  )

print(pca_clim_plot)

# ---- Prepare PCA scores for downstream use ----
pcscores_clim <- df_out[, 1:2] %>%
  rename(PC1clim = PC1, PC2clim = PC2)


# usar PCAs ejes para clima y traits
# usar elevation como factor fijo y año como aleatorio
# los años no son independientes entre sí
# elevacion como factor fijo: porque podría estar difiriendo entre ellos por cosas que no estén considerados en el clima.
# luego ver colinearidad en los predictores. Si la altitud tiene mucha collinearidad, la quito. Si no, la dejo. Porque puede ser colineal con las variables climaticas.
