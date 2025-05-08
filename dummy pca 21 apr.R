library(tidyverse)
library(FactoMineR)
library(factoextra)

# Define happy/unhappy groups
ess_clean <- clean_data %>%
  mutate(happiness_group = case_when(
    happy <= 4 ~ "unhappy",
    happy >= 7 ~ "happy",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(happiness_group)) %>%
  select(-happy)

# Split into groups
happy_data <- ess_clean %>% filter(happiness_group == "happy") %>% select(-happiness_group)
unhappy_data <- ess_clean %>% filter(happiness_group == "unhappy") %>% select(-happiness_group)

# Run PCA
pca_happy <- PCA(happy_data, scale.unit = TRUE, graph = FALSE)
pca_unhappy <- PCA(unhappy_data, scale.unit = TRUE, graph = FALSE)

# Scree plots
fviz_screeplot(pca_happy, title = "Scree Plot - Happy Group", addlabels = TRUE)
fviz_screeplot(pca_unhappy, title = "Scree Plot - Unhappy Group", addlabels = TRUE)

# -------------------joint scree plot
library(FactoMineR)
library(ggplot2)
library(dplyr)
library(tibble)

eig_happy <- as_tibble(pca_happy$eig[, 2]) %>%
  rename(variance = value) %>%
  mutate(Dimension = 1:n(), Group = "Happy")

eig_unhappy <- as_tibble(pca_unhappy$eig[, 2]) %>%
  rename(variance = value) %>%
  mutate(Dimension = 1:n(), Group = "Unhappy")

eig_combined <- bind_rows(eig_happy, eig_unhappy)

# Plot
ggplot(eig_combined, aes(x = Dimension, y = variance, color = Group)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Scree Plot Comparison: Happy vs Unhappy",
    x = "Principal Component",
    y = "Percentage of Variance Explained"
  ) +
  theme_minimal()



# ------------------- Variance explained
happy_var <- pca_happy$eig
unhappy_var <- pca_unhappy$eig

happy_components_80 <- min(which(cumsum(happy_var[, 2]) >= 80))
unhappy_components_80 <- min(which(cumsum(unhappy_var[, 2]) >= 80))

cat("Components to reach 80% variance:\n")
cat("Happy:", happy_components_80, "\nUnhappy:", unhappy_components_80, "\n")

# Dispersion in PC1-PC2 space
get_spread <- function(pca_obj) {
  scores <- as.data.frame(pca_obj$ind$coord)[, 1:2]
  centroid <- colMeans(scores)
  mean_dist <- mean(sqrt(rowSums((scores - centroid)^2)))
  return(mean_dist)
}

spread_happy <- get_spread(pca_happy)
spread_unhappy <- get_spread(pca_unhappy)

cat("Mean spread in PC1–PC2:\n")
cat("Happy:", round(spread_happy, 3), "\nUnhappy:", round(spread_unhappy, 3), "\n")


# ---- most important variables in pc 1 to 10
library(dplyr)
library(tibble)
library(tidyr)

# Extract loadings
loadings <- as.data.frame(pca_unhappy$var$coord)
loadings$Variable <- rownames(loadings)

# Reshape to long format and get absolute value
top_vars <- loadings %>%
  pivot_longer(cols = starts_with("Dim."), names_to = "PC", values_to = "Loading") %>%
  mutate(AbsLoading = abs(Loading)) %>%
  filter(PC %in% paste0("Dim.", 1:10)) %>%
  group_by(PC) %>%
  slice_max(order_by = AbsLoading, n = 4) %>%
  arrange(PC, desc(AbsLoading)) %>%
  select(PC, Variable, Loading)

print(top_vars)



