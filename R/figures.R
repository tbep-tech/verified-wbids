library(tidyverse)
library(sf)
library(tbeptools)
library(ggspatial)
library(here)
library(hrbrthemes)
library(showtext)

# get font
font_add_google("Roboto", "roboto")#, regular = 'C:/Windows/Fonts/Roboto.ttf')
fml <- "roboto"

showtext_auto()
showtext_opts(dpi = 400)

thm <- theme_ipsum(base_family = fml, plot_margin = margin(10, 10, 10, 10)) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x = element_text(hjust = 0.5, size = 12),
    axis.title.y = element_text(hjust = 0.5, size = 12),
    legend.position = 'top'
  )

cols <-  c('#00806E', '#427355', '#5C4A42', '#958984', '#004F7E')
names(cols) <- c("Bacteria", "Biology", "Dissolved Oxygen", "Metals", "Nutrients")

load(file = here('data/tbvwbid.RData'))

data(tbsegshed)
tbsegshed <- st_union(tbsegshed)

totareami2 <- st_area(tbsegshed) %>%
  set_units('mi2') %>%
  as.numeric()

# map by parameter --------------------------------------------------------

tomap <- tbvwbid

p <- ggplot() +
  annotation_map_tile(zoom = 9, type = 'cartolight') +
  geom_sf(data = tomap, fill = 'red', col = 'red', alpha = 0.6) +
  geom_sf(data = tbsegshed, fill = NA, color = 'black') +
  facet_wrap(~parameter) +
  theme(
    strip.background = element_blank(),
    axis.text = element_text(size = 5)
  ) +
  labs(
    title = 'Parameter listing by waterbody ID',
    subtitle = 'Tampa Bay watershed as outline',
    caption = 'source: FDEP, plot created by TBEP'
  )

jpeg(here('figures/mapparm.jpg'), family = fml, height = 9, width = 9, res = 400, units = 'in')
print(p)
dev.off()

# map by parameter group --------------------------------------------------

tomap <- tbvwbid

p <- ggplot() +
  annotation_map_tile(zoom = 9, type = 'cartolight') +
  geom_sf(data = tomap, fill = 'red', col = 'red', alpha = 0.6) +
  geom_sf(data = tbsegshed, fill = NA, color = 'black') +
  facet_wrap(~parameter_group) +
  theme(
    strip.background = element_blank(),
    axis.text = element_text(size = 5)
  ) +
  labs(
    title = 'Parameter group listing by waterbody ID',
    subtitle = 'Tampa Bay watershed as outline',
    caption = 'source: FDEP, plot created by TBEP'
  )

jpeg(here('figures/mapparmgrp.jpg'), family = fml, height = 6, width = 6, res = 400, units = 'in')
print(p)
dev.off()

# summary by parameter and area -------------------------------------------

toplo <- tbvwbid %>%
  st_set_geometry(NULL) %>%
  group_by(parameter, parameter_group) %>%
  summarise(areami2 = sum(areami2), .groups = 'drop') %>%
  arrange(areami2) %>%
  mutate(
    wshedper = paste(round(100 * (areami2 / totareami2), 1), '%'),
    parameter = factor(parameter, levels = .$parameter)
  )

p <- ggplot(toplo, aes(y = parameter, x = areami2)) +
  geom_bar(stat = 'identity', alpha = 0.7, aes(color = parameter_group, fill = parameter_group), size = 0.7, width = 0.7) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  geom_text(aes(label = wshedper), nudge_x = 10, hjust = 0) +
  scale_x_continuous(limits = c(0, max(toplo$areami2) * 1.1)) +
  thm +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line()
  ) +
  labs(
    y = NULL,
    x = 'Square miles',
    color = NULL,
    fill = NULL,
    title = 'Total area for listings by parameter',
    subtitle = '% is for the total watershed area',
    caption = 'source: FDEP, plot created by TBEP'
  )

jpeg(here('figures/areaparm.jpg'), height = 9, width = 10, family = fml, units = 'in', res = 400)
print(p)
dev.off()

# summary by parameter group and area -------------------------------------

toplo <- tbvwbid %>%
  st_set_geometry(NULL) %>%
  group_by(parameter_group) %>%
  summarise(areami2 = sum(areami2), .groups = 'drop') %>%
  arrange(areami2) %>%
  mutate(
    wshedper = paste(round(100 * (areami2 / totareami2), 1), '%'),
    parameter_group = factor(parameter_group, levels = .$parameter_group)
  )

p <- ggplot(toplo, aes(y = parameter_group, x = areami2)) +
  geom_bar(stat = 'identity', alpha = 0.7, aes(color = parameter_group, fill = parameter_group), size = 0.7, width = 0.7) +
  scale_color_manual(values = cols, guide = 'none') +
  scale_fill_manual(values = cols, guide = 'none') +
  geom_text(aes(label = wshedper), nudge_x = 10, hjust = 0) +
  scale_x_continuous(limits = c(0, max(toplo$areami2) * 1.1)) +
  thm +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line()
  ) +
  labs(
    y = NULL,
    x = 'Square miles',
    title = 'Total area for listings by parameter group',
    subtitle = '% is for the total watershed area',
    caption = 'source: FDEP, plot created by TBEP'
  )

jpeg(here('figures/areaparmgrp.jpg'), height = 5, width = 8, family = fml, units = 'in', res = 400)
print(p)
dev.off()


# summary by parameter and count ------------------------------------------

toplo <- tbvwbid %>%
  st_set_geometry(NULL) %>%
  group_by(parameter, parameter_group) %>%
  summarise(cnt = n(), .groups = 'drop') %>%
  arrange(cnt) %>%
  mutate(
    per = paste(round(100 * (cnt / sum(cnt)), 1), '%'),
    parameter = factor(parameter, levels = .$parameter)
  )

p <- ggplot(toplo, aes(y = parameter, x = cnt)) +
  geom_bar(stat = 'identity', alpha = 0.7, aes(color = parameter_group, fill = parameter_group), size = 0.7, width = 0.7) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  geom_text(aes(label = per), nudge_x = 2, hjust = 0) +
  scale_x_continuous(limits = c(0, max(toplo$cnt) * 1.1)) +
  thm +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line()
  ) +
  labs(
    y = NULL,
    x = 'Square miles',
    color = NULL,
    fill = NULL,
    title = 'Counts of listings by parameter',
    subtitle = '% is for the total count',
    caption = 'source: FDEP, plot created by TBEP'
  )

jpeg(here('figures/countparm.jpg'), height = 9, width = 10, family = fml, units = 'in', res = 400)
print(p)
dev.off()

# summary by parameter group and count ------------------------------------

toplo <- tbvwbid %>%
  st_set_geometry(NULL) %>%
  group_by(parameter_group) %>%
  summarise(cnt = n(), .groups = 'drop') %>%
  arrange(cnt) %>%
  mutate(
    per = paste(round(100 * (cnt / sum(cnt)), 1), '%'),
    parameter_group = factor(parameter_group, levels = .$parameter_group)
  )

p <- ggplot(toplo, aes(y = parameter_group, x = cnt)) +
  geom_bar(stat = 'identity', alpha = 0.7, aes(color = parameter_group, fill = parameter_group), size = 0.7, width = 0.7) +
  scale_color_manual(values = cols, guide = 'none') +
  scale_fill_manual(values = cols, guide = 'none') +
  geom_text(aes(label = per), nudge_x = 5, hjust = 0) +
  scale_x_continuous(limits = c(0, max(toplo$cnt) * 1.1)) +
  thm +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(),
    panel.grid.minor.x = element_line()
  ) +
  labs(
    y = NULL,
    x = 'Count',
    title = 'Count of listings by parameter group',
    subtitle = '% is for the total count',
    caption = 'source: FDEP, plot created by TBEP'
  )

jpeg(here('figures/countparmgrp.jpg'), height = 5, width = 8, family = fml, units = 'in', res = 400)
print(p)
dev.off()
