# PURPOSE: Run through hierarchical clustering examples
library(tidyverse)

# Load clean NBA data ------------

nba_filtered_stats <-
  read_csv("nba_filtered_stats.csv")

nba_pos_stats <- 
  read_csv("http://www.stat.cmu.edu/cmsac/sure/2021/materials/data/clustering/nba_2021_player_per_pos_stats.csv")
# Find rows for players indicating a full season worth of stats
tot_players <- nba_pos_stats %>% filter(tm == "TOT")
# Stack this dataset with players that played on just one team
nba_player_stats <- nba_pos_stats %>% filter(!(player %in% tot_players$player)) %>% 
  bind_rows(tot_players)
# Filter to only players with at least 250 minutes played
nba_filtered_stats <- nba_player_stats %>% filter(mp >= 250)
head(nba_filtered_stats)

nba_filtered_stats %>%
  ggplot(aes(x = x3pa, y = trb)) +
  geom_point() + 
  ggthemes::scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "bottom")

# Compute distance matrix ---------------

player_dist <- 
  dist(dplyr::select(nba_filtered_stats, x3pa, trb))

player_dist_matrix <- as.matrix(player_dist)
rownames(player_dist_matrix) <- nba_filtered_stats$player
colnames(player_dist_matrix) <- nba_filtered_stats$player
player_dist_matrix[1:3, 1:3]

# Plot distamce

long_dist_matrix <- 
  as_tibble(player_dist_matrix) %>%
  mutate(player1 = rownames(player_dist_matrix)) %>%
  pivot_longer(cols = -player1,
               names_to = "player2", values_to = "distance")

long_dist_matrix %>%
  ggplot(aes(x = player1, y = player2,
             fill = distance)) +
  geom_tile() +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  scale_fill_gradient(low = "darkorange", high = "darkblue")

library(seriation)


player_names_order <- 
  nba_filtered_stats$player[player_order]
player_dist_seriate <- seriate(player_dist)
player_order <- get_order(player_dist_seriate)

long_dist_matrix %>%
  mutate(player1 = fct_relevel(player1,
                               player_names_order),
         player2 = fct_relevel(player2,
                               player_names_order)) %>%
  ggplot(aes(x = player1, y = player2,
             fill = distance)) +
  geom_tile() +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom") +
  scale_fill_gradient(low = "darkorange", high = "darkblue")

# Start performing hclust -------------------------------

# Complete linage example
nba_complete_hclust <-
  hclust(player_dist, method = "complete")

nba_filtered_stats %>%
  mutate(player_clusters = 
           as.factor(cutree(nba_complete_hclust, k = 4))) %>%
  ggplot(aes(x = x3pa, y = trb,
             color = player_clusters)) +
  geom_point(alpha = 0.75) + 
  theme_bw() + ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom")


plot(nba_complete_hclust)

library(ggdendro)

ggdendrogram(nba_complete_hclust, theme_dendro = FALSE,
             labels = FALSE, leaf_labels = FALSE)  +
  labs(y = "Dissimilarity between clusters") +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank()) +
  geom_hline(yintercept = 20, linetype = "dashed",
             color = "darkred") +
  geom_hline(yintercept = 16, linetype = "dashed", 
             color = "darkblue")

nba_filtered_stats %>%
  mutate(player_clusters = 
           as.factor(cutree(nba_complete_hclust, h = 16))) %>%
  ggplot(aes(x = x3pa, y = trb,
             color = player_clusters)) +
  geom_point(alpha = 0.75) + 
  theme_bw() + ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom")

nba_single_hclust <- 
  hclust(player_dist, method = "single")

ggdendrogram(nba_single_hclust, theme_dendro = FALSE,
             labels = FALSE, leaf_labels = FALSE)  +
  labs(y = "Dissimilarity between clusters") +
  theme_bw() + 
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank()) 

nba_filtered_stats %>%
  mutate(player_clusters = 
           as.factor(cutree(nba_single_hclust, k = 4))) %>%
  ggplot(aes(x = x3pa, y = trb,
             color = player_clusters)) +
  geom_point(alpha = 0.75) + 
  theme_bw() + ggthemes::scale_color_colorblind() +
  theme(legend.position = "bottom")

# Average linkage
nba_average_hclust <-
  hclust





