#Fig6 ternary plot 
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(grid)

setwd("your workpath")
# load JSDM result for 6 month
load("may.rdata")
an <- readRDS("may_model_an_QLHG.RDS")
model <- readRDS("may_model_QLHG.RDS")

type <- "R2_McFadden"

species_data <- data.frame(
  name   = model$species,
  sp     = model$species,
  env    = pmax(an$species[[type]]$F_A, 0),
  spa    = pmax(an$species[[type]]$F_S, 0),
  codist = pmax(an$species[[type]]$F_B, 0),
  r2     = pmax(an$species[[type]]$Full, 0),
  stringsAsFactors = FALSE
) %>%
  mutate(order = sapply(strsplit(name, "_"), `[`, 1))

R2_stats <- species_data %>%
  select(name, order, env, spa, codist, r2) %>%
  mutate(across(c(env, spa, codist, r2), round, 4)) %>%
  arrange(order, desc(r2))

write.csv(
  R2_stats,
  "may_species_R2_stats_McFadden.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

min_point_size <- 1.8
max_point_size <- 4.8
point_stroke   <- 0.3

order_colors <- c(
  Diptera       = "#8FC3E8",
  Hymenoptera   = "#FFE066",
  Coleoptera    = "#D9B8FF",
  Hemiptera     = "#FF9F80",
  Lepidoptera   = "#A3D9A5",
  Orthoptera    = "#FFB3C1",
  Psocoptera    = "#9d6969",
  Thysanoptera  = "#9a9a9a",
  Megaloptera   = "#A6D8D9"
)

create_ternary_plot <- function(dat) {

  dat <- dat %>%
    mutate(
      total = ifelse(env + spa + codist == 0, 1, env + spa + codist),
      e = env / total,
      s = spa / total,
      c = codist / total,
      x = s + 0.5 * e,
      y = sqrt(3) * e / 2
    )

  triangle <- data.frame(
    x = c(0, 0.5, 1, 0),
    y = c(0, sqrt(3)/2, 0, 0)
  )

  grid <- expand.grid(i = seq(0.2, 0.8, 0.2), t = c("C","E","S")) %>%
    rowwise() %>%
    do({
      i <- .$i
      t <- .$t
      if (t == "C") data.frame(x = c(i/2, 1-i/2), y = c(sqrt(3)*i/2, sqrt(3)*i/2))
      else if (t == "E") data.frame(x = c(i, 0.5*(1+i)), y = c(0, sqrt(3)*(1-i)/2))
      else data.frame(x = c(1-i, 0.5*(1-i)), y = c(0, sqrt(3)*(1-i)/2))
    })

  dat$order <- factor(dat$order, levels = sort(intersect(names(order_colors), unique(dat$order))))

  ggplot() +
    geom_path(data = grid, aes(x, y, group = interaction(x, y)),
              color = "grey80", linewidth = 0.15, linetype = "dashed") +
    geom_path(data = triangle, aes(x, y), linewidth = 0.5) +
    geom_point(
      data = dat,
      aes(x, y, size = r2, fill = order),
      shape = 21,
      color = "black",
      stroke = point_stroke,
      alpha = 0.8
    ) +
    scale_fill_manual(values = order_colors, guide = "none") +
    scale_size_continuous(range = c(min_point_size, max_point_size), guide = "none") +
    coord_equal() +
    theme_void() +
    theme(plot.margin = margin(2, 2, 2, 2))
}

create_r2_legend <- function(dat) {

  r2_breaks <- seq(0.2, 0.8, 0.2)
  r2_range  <- range(dat$r2, na.rm = TRUE)
  if (diff(r2_range) == 0) r2_range <- c(0, 1)

  size_map <- function(x)
    min_point_size + (max_point_size - min_point_size) *
    (x - r2_range[1]) / diff(r2_range)

  legend_data <- data.frame(
    x = seq_along(r2_breaks),
    y = 1,
    r2 = r2_breaks,
    size = size_map(r2_breaks),
    lab = r2_breaks
  )

  ggplot(legend_data, aes(x, y)) +
    geom_point(aes(size = size), shape = 21, fill = "grey70",
               color = "black", stroke = point_stroke) +
    geom_text(aes(label = lab), vjust = -2, size = 2.5) +
    annotate("text", x = mean(legend_data$x), y = 1.5,
             label = "R²", size = 3, fontface = "bold") +
    scale_size_identity() +
    coord_cartesian(xlim = c(0.5, max(legend_data$x) + 0.5), ylim = c(0.5, 2)) +
    theme_void()
}

p_main <- create_ternary_plot(species_data)
p_r2   <- create_r2_legend(species_data)

ggsave("may_ternary_plot_5cm.pdf", p_main, width = 5, height = 5, units = "cm", dpi = 600)
ggsave("may_r2_legend_5cm.pdf", p_r2, width = 10, height = 4, units = "cm", dpi = 600)
