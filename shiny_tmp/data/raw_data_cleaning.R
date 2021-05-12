# =============================================================================
# Load Packages
# =============================================================================

library(tidyverse)
library(rgdal)
library(viridis)
library(RColorBrewer)

# =============================================================================
# Load Data
# =============================================================================
# Directories
PROJ_DIR <- file.path("mnt", "general-purpose", "nowcasting","employment_sa4_by_anzsco4_256")

IN_DIR <- file.path(PROJ_DIR, "04_public_release")
OUT_DIR <- file.path("~","nowcasting_shiny_employments", "data")

## Loading prediction data sa4-anzsco4 model

sa4_anzsco4_df <- read_csv(file = file.path("", "dbfs", IN_DIR,"all_output_2021-05-04.csv"),
                        col_types = cols_only(
                          sa4_code = col_double(),
                          sa4_name = col_character(),
                          anzsco4_code = col_double(),
                          anzsco4_name = col_character(),
                          state_name = col_character(),
                          extraction_date = col_date(),
                          original = col_double(),
                          trend = col_double()
                          )) 

# =============================================================================
# Transform input data
# =============================================================================

## add change variables
calc_change <- function(column, len, dt) {
  prev_val <- lag(column, n = len, order_by = dt)
  ((column - prev_val)/prev_val) 
}


sa4_anzsco4_df <- sa4_anzsco4_df %>% 
  group_by(sa4_code, anzsco4_code) %>%
  mutate(org_chg_mth = round(calc_change(original, 1, extraction_date),2),
         org_chg_qtr = round(calc_change(original, 3, extraction_date),2),
         org_chg_year = round(calc_change(original, 12, extraction_date),2),
         org_chg_5year = round(calc_change(original, 60, extraction_date),2),
         trd_chg_mth = round(calc_change(trend, 1, extraction_date),2),
         trd_chg_qtr = round(calc_change(trend, 3, extraction_date),2),
         trd_chg_year = round(calc_change(trend, 12, extraction_date),2),
         trd_chg_5year = round(calc_change(trend, 60, extraction_date),2)) %>%
  ungroup()


## Create sa4 filters df
filters_df <- sa4_anzsco4_df %>% 
  select(state_name,sa4_code,sa4_name, anzsco4_code, anzsco4_name) %>% 
  drop_na() %>% 
  distinct()

## Recent months 
m <- 6 # months to highlight

dates_df <- sa4_anzsco4_df %>% 
  pull(extraction_date) %>%
  unique()
recent_dates <- tail(dates_df, m)


## SA4 aggregation

agg_sa4_df <-  sa4_anzsco4_df %>% 
  group_by(sa4_code, sa4_name, extraction_date) %>%
  summarise(original = sum(original, na.rm = T),
            trend = sum(trend, na.rm = T)) %>%
  ungroup() %>%
  group_by(sa4_name) %>%
  mutate(org_chg_mth = round(calc_change(original, 1, extraction_date),2),
         org_chg_qtr = round(calc_change(original, 3, extraction_date),2),
         org_chg_year = round(calc_change(original, 12, extraction_date),2),
         org_chg_5year = round(calc_change(original, 60, extraction_date),2),
         trd_chg_mth = round(calc_change(trend, 1, extraction_date),2),
         trd_chg_qtr = round(calc_change(trend, 3, extraction_date),2),
         trd_chg_year = round(calc_change(trend, 12, extraction_date),2),
         trd_chg_5year = round(calc_change(trend, 60, extraction_date),2)
         ) %>%
  ungroup()

## ANZSCO4 aggregation
agg_anzsco4_df <- sa4_anzsco4_df %>% 
  group_by(anzsco4_code, anzsco4_name, extraction_date) %>%
  summarise(original = sum(original, na.rm = T),
            trend = sum(trend, na.rm = T)) %>%
  ungroup() %>%
  group_by(anzsco4_name) %>%
  mutate(org_chg_mth = round(calc_change(original, 1, extraction_date),2),
         org_chg_qtr = round(calc_change(original, 3, extraction_date),2),
         org_chg_year = round(calc_change(original, 12, extraction_date),2),
         org_chg_5year = round(calc_change(original, 60, extraction_date),2),
         trd_chg_mth = round(calc_change(trend, 1, extraction_date),2),
         trd_chg_qtr = round(calc_change(trend, 3, extraction_date),2),
         trd_chg_year = round(calc_change(trend, 12, extraction_date),2),
         trd_chg_5year = round(calc_change(trend, 60, extraction_date),2)
  ) %>%
  ungroup()

## Loading prediction data weekly index model
wkl_indx <- read_csv(file = file.path("", "dbfs",IN_DIR,"results_employment_level_stacked.csv"),
                     col_types = cols_only(
                       extraction_date = col_date(),
                       stacked_smoothed = col_double()
                     )) %>% 
  filter(!is.na(stacked_smoothed))


wkl_indx <- wkl_indx %>% mutate(chg_wkl = (stacked_smoothed - lag(stacked_smoothed,1))/ lag(stacked_smoothed,1),
                                chg_mth = (stacked_smoothed - lag(stacked_smoothed,4))/ lag(stacked_smoothed,4),
                                chg_qtr = (stacked_smoothed - lag(stacked_smoothed,12))/ lag(stacked_smoothed,12))

# =============================================================================
# colour pal
# =============================================================================
n <- length(unique(filters_df$sa4_code))

heat_pal <- inferno(n)

qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
sa4_pal <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))) #only 74 colours
## add colours
sa4_pal = c(sa4_pal,sample(heat_pal, n - length(sa4_pal)))


# =============================================================================
# Load geojson file
# =============================================================================
sa4_json <- rgdal::readOGR(file.path("", "dbfs", IN_DIR,"SA4_2016_AUST_simplify_60prc.json"), stringsAsFactors = F)

# remove extra data
sa4_json@data <- sa4_json@data %>% select(SA4_CODE16, SA4_NAME16) %>% 
  filter(SA4_NAME16 != "Other Territories") %>%
  rename(sa4_code = SA4_CODE16, sa4_name = SA4_NAME16) %>%
  mutate(sa4_code = as.numeric(sa4_code))%>%
  left_join(agg_sa4_df %>%
              select(sa4_name, sa4_code, extraction_date, original, trend) %>% 
              filter(extraction_date == max(extraction_date)) %>% 
              select(-extraction_date, -sa4_name), by = "sa4_code" ) %>% 
  mutate(pal = sa4_pal )

# =============================================================================
# Output ---> shiny
# =============================================================================

df_lst <- list(agg_sa4_df = agg_sa4_df, 
               agg_anzsco4_df = agg_anzsco4_df,
               sa4_anzsco4_df = sa4_anzsco4_df,
               filters_df = filters_df,
               sa4_json = sa4_json,
               recent_dates = recent_dates,
               heat_pal = heat_pal,
               wkl_indx = wkl_indx)

df_lst %>% write_rds(file = file.path(OUT_DIR, "shiny_df.rds"))


