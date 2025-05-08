library(ggplot2)
library(dplyr)

# Extract PCA variable coordinates (loadings)
var_coords <- as.data.frame(pca_unhappy$var$coord) %>%
  mutate(Variable = rownames(.))

# Plot like MCA-style
ggplot(var_coords, aes(x = Dim.1, y = Dim.2)) +
  geom_point(color = "blue", shape = 17, size = 3) +  # triangle point
  geom_text(aes(label = Variable), hjust = -0.2, vjust = 0.2, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Variables - PCA unhappy",
    x = paste0("Dim1 (", round(pca_happy$eig[1, 2], 1), "%)"),
    y = paste0("Dim2 (", round(pca_happy$eig[2, 2], 1), "%)")
  ) +
  theme_minimal()

# ------------colors by var type
# Combine into one lookup table
var_types <- data.frame(
  Variable = c(socio_econ_vars, wellbeing_vars, personal_values_vars),
  Type = c(rep("Socioeconomic", length(socio_econ_vars)),
           rep("Wellbeing", length(wellbeing_vars)),
           rep("Personal Values", length(personal_values_vars)))
)

# Extract PCA coordinates for unhappy group (or happy)
var_coords <- as.data.frame(pca_happy$var$coord) %>%
  mutate(Variable = rownames(.)) %>%
  left_join(var_types, by = "Variable")

# Plot
ggplot(var_coords, aes(x = Dim.1, y = Dim.2, color = Type)) +
  geom_point(shape = 17, size = 3) +
  geom_text(aes(label = Variable), hjust = -0.2, vjust = 0.2, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Variables - PCA (Happy Group)",
    x = paste0("Dim1 (", round(pca_unhappy$eig[1, 2], 1), "%)"),
    y = paste0("Dim2 (", round(pca_unhappy$eig[2, 2], 1), "%)")
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Socioeconomic" = "#1f77b4",
                                "Wellbeing" = "#2ca02c",
                                "Personal Values" = "#d62728"))





# by value archetype
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(ggrepel)

# ---- 1. Extract variable coordinates (loadings) ----
loadings <- as.data.frame(pca_unhappy$var$coord[, 1:2])
colnames(loadings) <- c("PC1", "PC2")
loadings$Variable <- rownames(loadings)

# ---- 2. Define value archetypes ----
value_archetypes <- list(
  "Self-Direction"   = c("ipcrtiv", "impfree", "ipudrst"),
  "Stimulation"      = c("ipadvnt", "ipgdtim"),
  "Hedonism"         = c("impfun", "ipgdtim"),
  "Achievement"      = c("ipsuces", "ipstrgv"),
  "Power"            = c("imprich", "ipbhprp"),
  "Security"         = c("impsafe", "ipmodst", "ipfrule"),
  "Conformity"       = c("ipmodst", "ipbhprp"),
  "Tradition"        = c("imptrad", "ipmodst", "ipbhprp"),
  "Benevolence"      = c("iphlppl", "iprspot"),
  "Universalism"     = c("iplylfr", "impenv", "ipeqopt")
)

value_labels <- stack(value_archetypes)
colnames(value_labels) <- c("Variable", "Archetype")

# ---- 3. Merge with loadings ----
loadings <- left_join(loadings, value_labels, by = "Variable")

# ---- 4. Tag variable types ----
loadings$Type <- ifelse(!is.na(loadings$Archetype), "Value", "Other")
loadings$Type[loadings$Variable %in% socio_econ_vars] <- "Socioeconomic"
loadings$Type[loadings$Variable %in% wellbeing_vars] <- "Wellbeing"

# ---- 5. Plot ----
ggplot(loadings, aes(x = PC1, y = PC2, color = Archetype, shape = Type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_point(size = 3) +
  geom_text_repel(aes(label = Variable), size = 3, max.overlaps = 100) +
  scale_shape_manual(values = c(Value = 17, Socioeconomic = 15, Wellbeing = 18, Other = 3)) +
  labs(title = "Variable Contributions - PCA (Unappy Group)",
       x = "PC1", y = "PC2", color = "Value Archetype") +
  theme_minimal()




