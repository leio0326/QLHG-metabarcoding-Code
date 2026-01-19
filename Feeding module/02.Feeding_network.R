#Fig4e Feeding network dynamics
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)

df <- read.csv("Kappa_irr_Results_Sep_perfectmatch.csv") # Predicted insect-plant feeding pairs for each sampling month
insect_order <- read.csv("insorder2.csv") # Insect taxonomy mapping file
plant_order <- read.csv("feedorder2.csv") # Feeding-plant taxonomy mapping file

df <- df %>%
  left_join(insect_order, by = c("Insect" = "Accession")) %>%
  rename(Insect_Order = Taxonomy) %>%
  left_join(plant_order, by = c("Plant" = "Accession")) %>%
  rename(Plant_Order = Taxonomy) %>%
  filter(!is.na(Insect_Order) & !is.na(Plant_Order))

df_count <- df %>%
  group_by(Insect_Order, Plant_Order) %>%
  summarise(Connection_Count = n(), .groups = "drop")

insect_totals <- df_count %>%
  group_by(Insect_Order) %>%
  summarise(Total_Connections = sum(Connection_Count), .groups = "drop") %>%
  arrange(desc(Total_Connections))

insect_order_levels <- insect_totals$Insect_Order
plant_orders_sorted <- sort(unique(df_count$Plant_Order))

insect_data <- insect_totals %>%
  arrange(desc(Total_Connections)) %>%
  mutate(
    Insect_Order = factor(Insect_Order, levels = Insect_Order),
    total_all = sum(Total_Connections),
    cum_prop_top = cumsum(Total_Connections) / total_all,
    y_end_rev = cum_prop_top,
    y_start_rev = c(0, cum_prop_top[-n()]),
    y_start = 1 - y_end_rev,
    y_end = 1 - y_start_rev,
    height = y_end - y_start,
    y_center = (y_start + y_end) / 2
  )

plant_data <- df_count %>%
  group_by(Plant_Order) %>%
  summarise(Total_Connections = sum(Connection_Count), .groups = "drop") %>%
  arrange(Plant_Order) %>%
  mutate(
    Plant_Order = factor(Plant_Order, levels = Plant_Order),
    total_all = sum(Total_Connections),
    cum_prop = cumsum(Total_Connections) / total_all,
    y_start = c(0, cum_prop[-n()]),
    y_end = cum_prop,
    height = y_end - y_start,
    y_center = (y_start + y_end) / 2
  )

order_colors <- c(
  "Diptera" = "#8FC3E8",
  "Hymenoptera" = "#FFE066",
  "Coleoptera" = "#D9B8FF",
  "Hemiptera" = "#FF9F80",
  "Lepidoptera" = "#A3D9A5",
  "Orthoptera" = "#FFB3C1",
  "Psocoptera" = "#9d6969",
  "Plecoptera" = "#9a9a9a",
  "Megaloptera" = "#A6D8D9"
)

remaining_insects <- setdiff(unique(df_count$Insect_Order), names(order_colors))

if (length(remaining_insects) > 0) {
  paired_colors <- brewer.pal(12, "Paired")
  set3_colors <- brewer.pal(12, "Set3")
  all_colors <- c(paired_colors, set3_colors)

  remaining_colors <- all_colors[seq_along(remaining_insects)]
  names(remaining_colors) <- remaining_insects

  insect_colors <- c(order_colors, remaining_colors)
} else {
  insect_colors <- order_colors
}

plant_colors <- brewer.pal(8, "Set2")
if (length(plant_orders_sorted) > 8) {
  plant_colors <- colorRampPalette(plant_colors)(length(plant_orders_sorted))
}
names(plant_colors) <- plant_orders_sorted

insect_data$color <- insect_colors[as.character(insect_data$Insect_Order)]
plant_data$color <- plant_colors[as.character(plant_data$Plant_Order)]

connections_corrected <- df_count %>%
  left_join(
    insect_data %>%
      select(Insect_Order,
             insect_y_start = y_start,
             insect_y_end = y_end,
             insect_height = height,
             insect_total = Total_Connections),
    by = "Insect_Order"
  ) %>%
  left_join(
    plant_data %>%
      select(Plant_Order,
             plant_y_start = y_start,
             plant_y_end = y_end,
             plant_height = height,
             plant_total = Total_Connections),
    by = "Plant_Order"
  ) %>%
  uncount(Connection_Count) %>%
  group_by(Insect_Order) %>%
  mutate(insect_conn_id = row_number()) %>%
  ungroup() %>%
  group_by(Plant_Order) %>%
  mutate(plant_conn_id = row_number()) %>%
  ungroup() %>%
  mutate(
    insect_y = insect_y_start +
      (insect_conn_id - 0.5) / insect_total * insect_height,
    plant_y = plant_y_start +
      (plant_conn_id - 0.5) / plant_total * plant_height,
    x1 = 0.1,
    x2 = 0.9
  )

p <- ggplot() +
  geom_segment(
    data = connections_corrected,
    aes(x = x1, xend = x2, y = insect_y, yend = plant_y),
    color = "grey80",
    alpha = 0.6,
    linewidth = 0.1,
    lineend = "round"
  ) +
  geom_rect(
    data = insect_data,
    aes(xmin = 0, xmax = 0.1, ymin = y_start, ymax = y_end, fill = Insect_Order),
    color = "white",
    linewidth = 0.5
  ) +
  geom_rect(
    data = plant_data,
    aes(xmin = 0.9, xmax = 1.0, ymin = y_start, ymax = y_end, fill = Plant_Order),
    color = "white",
    linewidth = 0.5
  ) +
  scale_fill_manual(values = c(insect_colors, plant_colors)) +
  scale_x_continuous(limits = c(-0.05, 1.05), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0.02, 0.02))) +
  theme_void() +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )

ggsave(
  "Sep_Sankey.tiff",
  plot = p,
  width = 3,
  height = 8,
  dpi = 600,
  device = "tiff",
  compression = "lzw"
)
