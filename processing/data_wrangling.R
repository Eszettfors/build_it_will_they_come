library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(WDI)

df_page_views = read_csv("data/raw/finland_ts_page_views.csv")
df_wiki = read_csv("data/raw/country_year_lang_views.csv")
df_country = read_csv("data/raw/country_data.csv")
df_lang = read_csv("data/raw/language_data.csv")
df_speakers = read_csv("data/raw/national_time_series_speakers.csv")
df_supp = read_csv("data/raw/wikipedia_support_time_series.csv")
geo = ne_countries(scale = "medium")
income_grp = read_csv("data/raw/world-bank-income-groups.csv")


# join wikipedia data
df_page_views = df_wiki %>%
  left_join(df_country) %>%
  left_join(df_lang)

# subset to finland
df_page_views = df_page_views %>%
  filter(country_name == "Finland")

# inner joins --> language year observations with at least one non zero value for page views speakers or articles are kept.
df_full = df_page_views %>%
  select(ISO6393, year, family, latitude, longitude, page_views) %>%
  inner_join(df_speakers) %>%
  inner_join(df_supp %>%
               select(ISO6393, year, articles, edits))

# lang not in wikipedia
df_speakers %>%
  filter(!ISO6393 %in% df_page_views$ISO6393) # luba-


# give ido the coordinates of esparanto since it is a derivative
df_full = df_full %>%
  mutate(latitude = ifelse(ISO6393 == "ido", 49.33, latitude),
         longitude = ifelse(ISO6393 == "ido", 2.81, longitude))


df_langs = df_full %>%
  distinct(ISO6393, language_name, latitude, longitude)

# infere geographical region from latitude and longitude
geo = geo %>%
  select(iso_a3_eh, name, region_wb, gdp_md, pop_est) %>%
  mutate(gdp_pc = gdp_md * 10^6 / pop_est) %>%
  st_as_sf() %>%
  st_make_valid()

df_langs = df_langs %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_join(geo, join = st_within)


df_langs %>%
  ungroup() %>%
  as_tibble() %>%
  select(!geometry)

# classify based on economy
df_langs = df_langs %>%
  left_join(income_grp, join_by("iso_a3_eh" == "Code"))

df_langs %>%
  count(`World Bank's income classification`)

df_langs %>%
  filter(is.na(`World Bank's income classification`)) # Ethiopian languages --> low income country

df_langs = df_langs %>%
  mutate(income_grp = ifelse(`World Bank's income classification` %in% c("Upper-middle-income countries", "High-income countries"), "high", "low")) %>%
  select(!`World Bank's income classification`)

df_langs %>%
  count(income_grp)

# add language data
df_full = df_langs %>%
  right_join(df_full)

nrow(df_full) # 997 observations

df_full %>%
  distinct(year) # from 2015 to 2024 -> 10 years

df_full %>%
  distinct(ISO6393) # 107 languages
# we are following 107 languages across 10 years in Finland --> short panel t << n

# write data
write_csv(df_full, "data/processed/finland_panel.csv")

