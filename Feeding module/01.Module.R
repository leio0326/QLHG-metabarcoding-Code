# Fig4b Module succession
library(igraph)
library(ggplot2)
library(dplyr)
library(ggalluvial)
library(RColorBrewer)
library(tidyr)

set.seed(123)

output_dir <- "Output"
dir.create(output_dir, showWarnings = FALSE)

setwd("your workpath")

month_files <- c("May", "June", "July", "Aug", "Sep", "Oct")
month_labels <- c("May", "June", "July", "August", "September", "October")

mat_list <- lapply(paste0(month_files, "_perfectmatch.rds"), readRDS)

all_predators <- unique(unlist(lapply(mat_list, rownames)))

presence <- matrix(0, length(all_predators), 6,
                   dimnames = list(all_predators, month_labels))
connections <- presence

for (i in seq_along(mat_list)) {
  m <- mat_list[[i]]
  idx <- intersect(rownames(m), all_predators)
  presence[idx, i] <- 1
  connections[idx, i] <- rowSums(m[idx, , drop = FALSE])
}

species_summary <- data.frame(
  Species = all_predators,
  Months_Present = rowSums(presence),
  Total_Connections = rowSums(connections),
  stringsAsFactors = FALSE
) %>%
  filter(Months_Present >= 3) %>%
  arrange(desc(Total_Connections))

write.csv(
  species_summary,
  file.path(output_dir, "Filtered_Species_Min3Months.csv"),
  row.names = FALSE
)

top_n <- 15
top_species <- head(species_summary$Species, top_n)

write.csv(
  data.frame(
    Rank = seq_len(top_n),
    M_Label = paste0("M", seq_len(top_n)),
    Species = top_species,
    Total_Connections = head(species_summary$Total_Connections, top_n),
    Months_Present = head(species_summary$Months_Present, top_n)
  ),
  file.path(output_dir, "Top15_Species_Details.csv"),
  row.names = FALSE
)

df_all <- list()

for (i in seq_along(mat_list)) {

  mat <- mat_list[[i]]
  m <- nrow(mat)
  n <- ncol(mat)

  adj <- rbind(
    cbind(matrix(0, m, m), mat),
    cbind(t(mat), matrix(0, n, n))
  )

  sp <- c(rownames(mat), colnames(mat))
  dimnames(adj) <- list(sp, sp)

  g <- graph_from_adjacency_matrix(adj, mode = "max", weighted = TRUE)
  mem <- membership(cluster_louvain(g))

  total <- sum(adj)
  count <- rowSums(adj)

  df_all[[i]] <- data.frame(
    Month = month_labels[i],
    Species = sp,
    Count.relative = if (total > 0) count / total else 0,
    Module_raw = paste0("X", formatC(mem, width = 2, flag = "0")),
    stringsAsFactors = FALSE
  )
}

df <- bind_rows(df_all)

df <- df %>%
  mutate(
    Month = factor(Month, levels = month_labels),
    Module_simple = Module_raw
  )

for (i in seq_along(top_species)) {
  sp <- top_species[i]
  label <- paste0("M", i)
  idx <- which(df$Species == sp)
  if (length(idx) > 0) {
    raw <- df$Module_raw[idx[1]]
    df$Module_simple[df$Module_raw == raw] <- label
  }
}

df$Module_final <- ifelse(
  df$Module_simple %in% paste0("M", seq_len(top_n)),
  df$Module_simple,
  "Others"
)

df$Module_final <- factor(
  df$Module_final,
  levels = c(paste0("M", seq_len(top_n)), "Others")
)

cols <- brewer.pal(12, "Set3")
if (top_n > 12) {
  cols <- colorRampPalette(cols)(top_n)
}
names(cols) <- paste0("M", seq_len(top_n))
cols["Others"] <- "grey70"

pdf(file.path(output_dir, "Alluvial_Top15_Species_with_Legend.pdf"),
    width = 16, height = 5)

ggplot(
  df,
  aes(
    x = Month,
    stratum = Module_final,
    alluvium = Species,
    y = Count.relative,
    fill = Module_final
  )
) +
  geom_flow(stat = "alluvium", alpha = 0.8) +
  geom_stratum(alpha = 0.8) +
  scale_fill_manual(values = cols, name = "Modules") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 11, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8)
  )

dev.off()

saveRDS(df, file.path(output_dir, "Alluvial_Data_Top15_Species.rds"))
write.csv(df, file.path(output_dir, "Alluvial_Data_Top15_Species.csv"),
          row.names = FALSE)
