library(igraph)
library(ggraph)

library(tidyverse)
library(ggforce)

df <- read_tsv('/projects/ezhao_prj/analyses/oncotree/oncotree_edge_list.txt')
pog <- read_tsv('/projects/ezhao_prj/analyses/oncotree/POG_oncotree_mapping_mrj20171012.txt')

graph <- df %>%
  rename(
    from = parent,
    to = child
  ) %>%
  graph_from_data_frame() 

graph_vertices <- names(graph[1])

pog_filtered <- pog %>%
  group_by(pog_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  filter(
    oncotree_type %in% graph_vertices
  )

pog_vertices <- which(graph_vertices %in% c('Tissue', pog_filtered$oncotree_type)) %>%
  sapply(function(vert_index) {
  subcomponent(graph, vert_index, mode = c('in')) %>% as.numeric
}) %>%
  reduce(c) %>%
  unique()

graph_in_pog <- graph %>%
  induced_subgraph(pog_vertices)

df_in_pog <- tibble(
  name = names(V(graph_in_pog))
) %>%
  mutate(index = row_number())

pog_counts <- df_in_pog %>%
  inner_join(pog_filtered, by = c('name' = 'oncotree_type')) %>%
  group_by(index, name) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  right_join(df_in_pog, by = c('name', 'index')) %>%
  replace_na(list(count = 0))

vertex_attr(graph_in_pog, 'pog_count', pog_counts$index) <- pog_counts$count

total_counts <- sapply(1:vcount(graph_in_pog), function(vertex_id) {
  induced_subgraph(
    graph_in_pog, 
    subcomponent(graph = graph_in_pog, v = vertex_id, mode = 'out') %>% as.numeric()
  ) %>% 
    vertex_attr('pog_count') %>% 
    sum
})

vertex_attr(graph_in_pog, 'total_count') <- total_counts
vertex_attr(graph_in_pog, 'abbreviated_name') <- gsub('.*?\\((.*?)\\)', '\\1', vertex_attr(graph_in_pog, 'name'))
  
plot_pog_dendrogram <- function(pog_graph) {
  pog_graph %>%
    ggraph(layout = 'dendrogram', circular = TRUE) +
    geom_edge_diagonal() +
    geom_node_point(size = 5) +
    geom_node_text(
      aes(label = total_count),
      colour = 'white', 
      size = 2.5
    ) +
    geom_node_text(
      aes(
        x = x * 1.04,
        y = y * 1.04,
        filter = leaf,
        angle = -((-node_angle(x, y) + 90) %% 180) + 90, 
        label = abbreviated_name
      ), 
      size = 2.5, 
      hjust = 'outward'
    ) +
    theme_no_axes() +
    scale_x_continuous(expand = c(0.1, 0.1)) +
    scale_y_continuous(expand = c(0.1, 0.1))
}

plot_pog_dendrogram(graph_in_pog)

leaves <- V(graph_in_pog)[degree(graph_in_pog, mode = 'out') == 0] %>% as.numeric()
plot_pog_dendrogram(delete_vertices(graph_in_pog, leaves))
