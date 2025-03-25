library(dplyr)

# How many total ponds does each individual visit?
df_visits = tort_sp_stats
pond_usage_counts <- df_visits %>%
  group_by(id) %>%
  summarize(
    ponds_visited = n_distinct(pond_name),
    total_visits  = n()    # total lines of "visits" in your df
  )

# Classify them as single-pond vs multiple-pond
pond_usage_counts <- pond_usage_counts %>%
  mutate(visit_category = ifelse(ponds_visited > 1, "multiple ponds", "single pond"))

pond_usage_counts

pond_type_summary <- df_visits %>%
  group_by(pond_type) %>%
  summarize(n_visits = n(),
            tot_time  = sum(timeInside, na.rm = TRUE),
            .groups = 'drop')

pond_type_summary

library(dplyr)

# Summarize total number of visits for each Tortoise-Pond pair
edge_list <- df_visits %>%
  group_by(id, pond_name, pond_type) %>% 
  summarize(
    n_visits = n(),
    tot_hours = sum(timeInside, na.rm = TRUE),
    .groups = 'drop'
  )

head(edge_list)

library(igraph)
library(tidyr)

# We want a bipartite graph: Tortoises and Ponds are two types of nodes.
# A common approach is to create a unique node label for tortoises vs. ponds
# Then edges link them.

# Let’s define a new column that merges pond_name with its type
# (helpful if you want color for artificial vs natural)
edge_list <- edge_list %>%
  mutate(pond_label = paste(pond_name, "(", pond_type, ")", sep=""))

# The actual edge list is from `id` to `pond_label`.
# We'll let 'n_visits' be the edge weight
g_edges <- edge_list %>%
  select(id, pond_label, n_visits)

# Create an igraph object from a data frame
g <- graph_from_data_frame(d = g_edges,
                           directed = FALSE)  # Typically undirected for visitation



library(ggraph)
library(ggplot2)

# Add a node attribute "type" so we can color Tortoises vs. Ponds:
# (If a node name is in the 'id' column, it’s a Tortoise; otherwise it's a Pond.)
# You can refine this logic as needed.

V(g)$node_type <- ifelse(V(g)$name %in% edge_list$id, "Tortoise", "Pond")

# If we want to highlight which ponds are natural vs artificial:
# We stored the combined label in 'pond_label', so let's parse out the pond_type:
V(g)$pond_type <- NA
pond_nodes_idx <- which(V(g)$node_type == "Pond")
V(g)$pond_type[pond_nodes_idx] <-
  ifelse(grepl("(Natural)", V(g)$name[pond_nodes_idx], fixed = TRUE),
         "Natural", "Artificial")

# Now let's do a basic layout (e.g., "fr" for Fruchterman-Reingold)
p <- ggraph(g, layout = "fr") + 
  # edges:
  geom_edge_link(aes(width = n_visits), 
                 color = "gray50", 
                 alpha = 0.8,
                 show.legend = TRUE) +
  scale_edge_width(name = "Number of visits", range = c(0.5, 3)) +
  # nodes:
  geom_node_point(aes(
    color = node_type,      # Tortoise vs. Pond
    shape = node_type,
    size  = node_type
  ), 
  alpha = 0.9) +
  # optional: label ponds with their name, or label tortoises
  geom_node_text(
    data = . %>% dplyr::filter(node_type == "Pond"), # Only label ponds
    aes(label = name), 
    size = 3, 
    repel = TRUE,
    color = "black"
  ) +
  # a simple color scale
  scale_color_manual(values = c("Tortoise"="tomato", "Pond"="steelblue")) +
  scale_shape_manual(values = c("Tortoise"=17, "Pond"=19)) +
  scale_size_manual(values = c("Tortoise"=4, "Pond"=5)) +
  theme_void() +
  labs(title = "Tortoise ↔ Pond Bipartite Network")

print(p)

# Build a pond–pond adjacency from your `df_visits`
library(dplyr)
library(tidyr)

pond_pond_edges <- df_visits %>%
  select(id, pond_name) %>%
  group_by(id) %>%
  # get all pairs of ponds visited by the same tortoise
  summarize(pond_pairs = combn(pond_name, 2, simplify = FALSE), .groups = 'drop') %>%
  unnest(cols = pond_pairs) %>%
  # 'pond_pairs' is a list of pairs, e.g. c("PondA", "PondB")
  # Turn each pair into a row
  mutate(pond1 = map_chr(pond_pairs, 1),
         pond2 = map_chr(pond_pairs, 2)) %>%
  # Summarize frequency
  group_by(pond1, pond2) %>%
  summarize(n_tortoises = n_distinct(id), .groups = 'drop')

head(pond_pond_edges)

g_pp <- igraph::graph_from_data_frame(pond_pond_edges, directed = FALSE)







# Install required packages if needed:
# install.packages("ggplot2")
# install.packages("patchwork")
# install.packages("dplyr")

library(ggplot2)
library(patchwork)
library(dplyr)



# 1. Create the data frame
df <- data.frame(
  Site = c("Steve_Devine", "Rancho_Chato", "Primicias_2", "Primicias_6", 
           "Manzanillo", "Cerro_Mesa", "Pikaia", "Laguna_Chato", 
           "Chato_Parque_1", "Chato_Parque_2", "Caseta_1", "Caseta_2"),
  richness = c(13, 12, 12, 16, 15, 19, 13, 17, 12, 6, 12, 16),
  shannon  = c(1.50835974, 1.390039789, 1.773149278, 1.379478902, 
               1.181500288, 1.744302663, 0.554681541, 1.885582011, 
               0.579022919, 0.435516094, 1.67562238, 1.924395483),
  pielou   = c(0.58806609, 0.559393162, 0.713567762, 0.497541843, 
               0.436291871, 0.592405778, 0.216254383, 0.665527718, 
               0.233015964, 0.24306616, 0.674320051, 0.694078955),
  tipo     = c("Rancho", "Rancho", "Rancho", "Rancho", "Rancho", 
               "Rancho", "Rancho", "Parque", "Parque", "Parque", 
               "Parque", "Parque")
)

# 2. Create individual plots

palette <- c("Rancho" = "goldenrod1", "Parque" = "steelblue")
library(ggplot2)
library(patchwork)  # for combining plots

palette <- c("Rancho" = "steelblue", "Parque" = "goldenrod1")

# Define a common theme
custom_theme <- theme_minimal(base_size = 18) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 18)
  )

# Plot 1: Richness by Site and Tipo
p1 <- ggplot(df, aes(x = Site, y = richness, fill = tipo)) +
  geom_col() +
  scale_fill_manual(values = palette) +
  labs(title = "Richness by Tipo",
       x = "Site",
       y = "Richness") +
  custom_theme

# Plot 2: Shannon Index by Tipo
p2 <- ggplot(df, aes(x = tipo, y = shannon, fill = tipo)) +
  geom_boxplot() +
  scale_fill_manual(values = palette) +
  labs(title = "Shannon Index by Tipo",
       x = "Tipo",
       y = "Shannon Index") +
  custom_theme

# Plot 3: Pielou's Evenness by Tipo
p3 <- ggplot(df, aes(x = tipo, y = pielou, fill = tipo)) +
  geom_boxplot() +
  scale_fill_manual(values = palette) +
  labs(title = "Pielou's Evenness by Tipo",
       x = "Tipo",
       y = "Pielou's Evenness") +
  custom_theme

# Plot 4: Richness vs Shannon with Site Labels
p4 <- ggplot(df, aes(x = richness, y = shannon, color = tipo, label = Site)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5, hjust = 0.5, size = 4) +  # Adjust label text size here
  scale_color_manual(values = palette) +
  labs(title = "Richness vs. Shannon Index",
       x = "Richness",
       y = "Shannon Index") +
  custom_theme

# Combine plots vertically
combined_plot <- p1 / p2 / p3 / p4

# Print combined plot
print(combined_plot)

ggsave("Outdir/intvertebrates.png", plot = combined_plot,
       width = 20, height = 40, units = "in", dpi = 300)
