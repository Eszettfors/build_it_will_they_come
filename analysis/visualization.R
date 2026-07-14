library(tidyverse)
library(DescTools)
library(ggrepel)
library(rnaturalearth)
library(sf)
library(corrplot)
library(pheatmap)
library(GGally)
library(patchwork)
library(plotly)

panel = read_csv("data/processed/finland_panel.csv")
df_geo = ne_countries(scale = "medium")

theme_set(theme_bw())

# we want to look at how a change in page view is determined by a change in native speaker numbers and change in the utility
# of the wikipedia proxied by articlces

# safe log
safe_log = function(x) log(x + 1)

# look at what languages are included
df_geo = df_geo %>%
  st_transform(crs = 8857)

plot_data = panel %>%
  filter(year == 2024) %>%
  distinct(language_name, latitude, longitude, family, articles, speakers, page_views, income_grp) %>%
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>%
  st_transform(crs = 8857)

df_geo %>%
  ggplot() + 
  geom_sf(data = df_geo, aes(fill = region_wb)) + 
  geom_sf(data = plot_data, aes(size = safe_log(articles), alpha = 0.9))


speaker_map = df_geo %>%
  filter(iso_a2_eh != "GL") %>%
  ggplot() + 
  geom_sf(data = df_geo) + 
  geom_sf(data = plot_data, aes(size = log10(speakers), colour = income_grp)) + 
  theme_minimal(base_size = 14) +
  theme(panel.border = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.17, 0.4)) + 
  guides(fill = "none",
         alpha = "none",
         colour = guide_legend(title = "Income Group (World Bank)"),
         size = guide_legend(title = "Number of Speakers (log10)")) 
speaker_map
ggsave("analysis/plots/speaker_map.png", speaker_map, height = 8, width = 12, dpi = 300)
  

# log transform all variables
panel = panel %>%
  mutate(log_page_views = safe_log(page_views),
         log_speakers = safe_log(speakers),
         log_articles = safe_log(articles))

panel %>%
  filter(year == 2024) %>%
  select(log_articles, log_speakers, log_page_views) %>%
  ggpairs()


Desc(panel$log_page_views)
Desc(panel$log_speakers)
Desc(panel$log_articles)


# spaghetti plots with value across time
panel %>%
  ggplot(aes(y = log_page_views, x = year, fill = language_name, colour = language_name)) +
  geom_point() + 
  geom_line() + 
  theme(legend.position = "none") # there are time shocks affecting all languages --> time fixed effects are necessary


panel %>%
  ggplot(aes(y = log_speakers, x = year, fill = language_name, colour = language_name)) +
  geom_point() + 
  geom_line() + 
  theme(legend.position = "none")

panel %>%
  ggplot(aes(y = log_articles, x = year, fill = language_name, colour = language_name)) +
  geom_point() + 
  geom_line() + 
  theme(legend.position = "none")

panel %>%
  filter(year == 2024, articles == 0) # Akan and Afar no articles in 2024, might have been discontinued

# cross sectional view 
panel_2024 = panel %>%
  filter(year == 2024)


pw_vs_speak = panel_2024 %>%
  ggplot(aes(y = log_page_views, x = log_speakers)) + 
  geom_point(aes(colour = income_grp)) +
  geom_smooth(method = "lm") + 
  geom_label(aes(label = ISO6393, colour = income_grp)) +
  theme_minimal(base_size = 14) +
  labs(title = "page views vs native speakers, 2024",
       x = "Native speakers (ln)",
       y = "Wikipedia page views (ln)",
       colour = "Income group") + 
  theme(legend.position = c(0.8, 0.1))

print(pw_vs_speak)
ggsave("analysis/plots/pw_vs_speak_cross.png", pw_vs_speak, dpi = 300, height = 8, width = 12)


cor.test(panel_2024$log_page_views, panel_2024$log_speakers, method = "pearson")
cor.test(panel_2024$log_page_views, panel_2024$log_speakers, method = "spearman")


pw_vs_articles = panel_2024 %>%
  ggplot(aes(y = log_page_views, x = log_articles)) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  geom_label(aes(label = ISO6393, colour = income_grp)) + 
  theme_minimal(base_size = 14) +
  labs(title = "B) page views vs number of articles, 2024",
       x = "Number of articles (ln)",
       y = "Wikipedia page views (ln)",
       colour = "Income group") + 
  theme(legend.position = c(0.8, 0.1))
print(pw_vs_articles)
ggsave("analysis/plots/pw_vs_articles_cross.png", pw_vs_articles, dpi = 300, height = 8, width = 12)

# exponential relationship
cor.test(panel_2024$log_page_views, panel_2024$log_articles, method = "pearson")
cor.test(panel_2024$log_page_views, panel_2024$log_articles, method = "spearman")

combined_plot = pw_vs_speak + pw_vs_articles 
print(combined_plot)
ggsave("analysis/plots/combined_pw_art_cross.png", combined_plot, dpi = 300, height = 8, width = 12)

# colour according to region
speakers_region_24 = panel_2024 %>%
  ggplot(aes(y = rank(log_page_views), x = rank(log_speakers))) + 
  geom_point(aes(colour = region_wb)) +
  geom_smooth(method = "lm") + 
  geom_label(aes(label = ISO6393, colour = region_wb)) + 
  labs(title = "page views vs native speakers, 2024",
       x = "Native speakers (Rank)",
       y = "Wikipedia page views (Rank)",
       colour = "Region") + 
  theme(legend.position = c(0.8, 0.15))


articles_region_2024 = panel_2024 %>%
  ggplot(aes(y = rank(log_page_views), x = rank(log_articles))) + 
  geom_point() +
  geom_smooth(method = "lm") + 
  geom_label(aes(label = ISO6393, colour = region_wb)) + 
  labs(title = "page views vs number of articles, 2024",
       x = "Number of articles (Rank)",
       y = "Wikipedia page views (Rank)",
       colour = "Region") + 
  theme(legend.position = c(0.8, 0.15))



comb_region_24 = speakers_region_24 + articles_region_2024
ggsave("analysis/plots/comb_region_cross.png", comb_region_24, dpi = 300, height = 8, width = 12)


# full panel ######
df_labs = panel %>%
  filter(year == 2024)

pw_vs_speak_panel = panel %>%
  ggplot(aes(y = log_page_views, x = log_speakers)) + 
  geom_point(aes(fill = region_wb, colour = region_wb, alpha = 0.5)) + 
  geom_smooth(method = "lm") +
  geom_smooth(method = "lm", aes(group = language_name, colour = region_wb), se = FALSE) +
  theme(legend.position = "none") + 
  geom_label_repel(data = df_labs, aes(y = log_page_views, x = log_speakers, label = ISO6393, colour = region_wb)) + 
  labs(y = "Wikipedia page views (ln)",
       x = "L1 speakers (ln)",
       title = "A) Wikipedia page views vs L1 speakers",
       colour = "Region",
       fill = "Region") + 
  guides(alpha = "none") + 
  theme_minimal(base_size = 14) + 
  theme(legend.position = c(0.25, 0.8))

pw_vs_speak_panel
         
cor.test(panel$log_page_views, panel$log_speakers, method = "pearson") # 0.70
cor.test(panel$log_page_views, panel$log_speakers, method = "spearman") # 0.62


pw_vs_art_panel = panel %>%
  ggplot(aes(y = log_page_views, x = log_articles)) + 
  geom_point(aes(fill = region_wb, colour = region_wb, alpha = 0.5)) + 
  geom_smooth(method = "lm") + 
  geom_smooth(method = "lm", aes(group = language_name , colour = region_wb), se = FALSE) + 
  geom_label_repel(data = df_labs, aes(y = log_page_views, x = log_articles, label = ISO6393, colour = region_wb), max.overlaps = 15) + 
  theme(legend.position = "none") + 
  labs(y = "Wikipedia page views (ln)",
       x = "Number of articles (ln)",
       title = "B) Wikipedia page views vs number of articles",
       colour = "Region",
       fill = "Region") + 
  guides(alpha = "none") +
  theme_minimal(base_size = 14) + 
  theme(legend.position = "none") + 
  geom_vline(xintercept = 11, linetype = 2)

pw_vs_art_panel
cor.test(panel$log_page_views, panel$log_articles, method = "pearson") # 0.79
cor.test(panel$log_page_views, panel$log_articles, method = "spearman") # 0.91



comb = pw_vs_speak_panel + pw_vs_art_panel
ggsave("analysis/plots/combined_pw_art_panel.png", comb, dpi = 300, height = 8, width = 12)


