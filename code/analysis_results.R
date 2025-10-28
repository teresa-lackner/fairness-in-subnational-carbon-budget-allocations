#-----------------------------------------------------------------------------#
# File: analysis_results.R                                                 ---#      
# Author: Teresa Lackner                                                   ---#
#-----------------------------------------------------------------------------#

#-----------------------------------------------------------------------------#
# 1. Initial setup, data import etc.                                       ----
#-----------------------------------------------------------------------------#

# 1.1 R setup ----------------------------------------------------------------#

library(here)
library(readr)
library(dplyr)
library(tidyverse)
library(patchwork)
library(viridis)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(forcats)

# 1.1 load results -----------------------------------------------------------#

es_pbe_eu_alloc_nuts2_scale_ESR <- 
  readr::read_csv(here::here("results", "es_pbe_eu_alloc_nuts2_scale_ESR.csv"))

ets_pbe_eu_alloc_new_direct_EU_to_NUTS2 <- 
  readr::read_csv(here::here("results",
                             "ets_pbe_eu_alloc_new_direct_EU_to_NUTS2.csv"))

cbe_global_alloc_nation_scale <-
  readr::read_csv(here::here("results", "cbe_global_alloc_nation_scale.csv"))

cbe_eu_alloc_nuts2_scale_hqual <-
  readr::read_csv(here::here("results", "cbe_eu_alloc_nuts2_scale_hqual.csv"))


#-----------------------------------------------------------------------------#
# 2. Analysis of regional ESR PBE budgets                                  ----
#-----------------------------------------------------------------------------#

# national population data based on sum of NUTS-2 regions
population_national <-
  es_pbe_eu_alloc_nuts2_scale_ESR %>% 
  mutate(avg_pop = (pop_21+pop_50)/2) %>%   
  group_by(ISO, NUTS_ID) %>% 
  summarise(pop_21 = unique(pop_21),
            avg_pop = unique(avg_pop)) %>% 
  ungroup() %>% 
  group_by(ISO) %>% 
  summarise(pop_21 = sum(pop_21),
            avg_pop = sum(avg_pop)) %>% 
  ungroup()

# make dataset national budgets 
pbe_es_budget_alloc_nation_scale_EU <-
  es_pbe_eu_alloc_nuts2_scale_ESR %>% 
  group_by(ISO) %>% 
  # national budget in GtCO2eq based on national ESR targets
  summarise(es_budget = sum(sect_b)) %>%  
  ungroup() %>% 
  # join datasets by ISO and calculate per capita budget in t per capita
  full_join(population_national, by = "ISO") %>% 
  mutate(es_budget = es_budget*1000000000/pop_21) %>% # per capita budgets
  select(-pop_21)

# make dataset for regional budgets
pbe_es_budget_alloc_nuts2_scale_det <-
  es_pbe_eu_alloc_nuts2_scale_ESR %>% 
  group_by(NUTS_ID, ISO, pop_21) %>% 
  summarise(es_budget = sum(sect_b)) %>% # NUTS2 budgets in Gt CO2eq
  ungroup() %>% 
  mutate(es_budget = es_budget*1000000000/pop_21) %>% # per capita budgets
  select(-pop_21)

# combine the data into one dataset 
national_data <-
  pbe_es_budget_alloc_nation_scale_EU %>% 
  rename(country = ISO, per_capita_budget = es_budget) %>%
  mutate(region = "Country Average")

sub_national_data <-
  pbe_es_budget_alloc_nuts2_scale_det %>% 
  rename(country = ISO, per_capita_budget = es_budget, region = NUTS_ID)

combined_data <- bind_rows(national_data, sub_national_data)

# Creating the plot for Figure 2)d)

# Create an ordering variable based on the average per capita budget
order_data <- combined_data %>%
  filter(region == "Country Average") %>%
  arrange(desc(per_capita_budget)) %>%
  mutate(order = row_number())

# Merge the ordering back into the main dataset
combined_data <- combined_data %>%
  left_join(order_data %>% select(country, order), by = "country")

# Creating the plot with ordered y-axis based on country average and color
# gradient for NUTS2 regions
plot_esr <-
  ggplot(combined_data, aes(x = per_capita_budget, y = reorder(country, order))) +
  geom_point(data = filter(combined_data, region != "Country Average"), 
             # Apply color based on per capita budget
             aes(color = per_capita_budget),  
             size = 2, alpha = 0.6) +
  scale_color_viridis(option = "D", direction = 1) +  # Use viridis color scale
  geom_point(data = filter(combined_data, region == "Country Average"), 
             shape = 15, size = 2, color = "black") +
  # Change legend title here: tons CO2e\nper capita
  labs(x = "tons CO2e per capita", y = "Country", color = "") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('./plots/nuts2_es_pbe_alloc_dotplot.pdf', plot_esr, 
       width = 90, height = 105, units = "mm")

#-----------------------------------------------------------------------------#
# 3. Analysis of regional ETS PBE budgets                                  ----
#-----------------------------------------------------------------------------#

# make dataset national budgets
pbe_ets_budget_alloc_nation_scale_EU <-
  ets_pbe_eu_alloc_new_direct_EU_to_NUTS2 %>% 
  group_by(ISO) %>% 
  summarise(ets_budget = sum(sect_b)) %>% 
  ungroup() %>% 
  # join datasets by ISO and calculate per capita budget in t per capita
  full_join(population_national, by = "ISO") %>% 
  mutate(ets_budget = ets_budget*1000000000/pop_21) %>% 
  select(-pop_21) 

# NUTS 2 population not reported in ETS results file
population_NUTS_2 <-
  es_pbe_eu_alloc_nuts2_scale_ESR %>% 
  group_by(ISO, NUTS_ID) %>% 
  summarise(pop_21 = unique(pop_21),
            pop_50 = unique(pop_50)) %>% # new line
  ungroup() %>% 
  mutate(avg_pop = (pop_21+pop_50)/2) # new line

# make dataset for regional budgets
pbe_ets_budget_alloc_nuts2_scale_det <-
  ets_pbe_eu_alloc_new_direct_EU_to_NUTS2 %>% 
  group_by(NUTS_ID, ISO) %>% 
  summarise(ets_budget = sum(sect_b)) %>% 
  ungroup() %>% 
  left_join(population_NUTS_2, by = c("ISO", "NUTS_ID")) %>% 
  mutate(ets_budget = ets_budget*1000000000/pop_21) %>% 
  select(-pop_21)

# combine the data into one dataset to make plots 
national_data <-
  pbe_ets_budget_alloc_nation_scale_EU %>% 
  rename(country = ISO, per_capita_budget = ets_budget) %>%
  mutate(region = "Country Average")

sub_national_data <-
  pbe_ets_budget_alloc_nuts2_scale_det %>% 
  rename(country = ISO, per_capita_budget = ets_budget, region = NUTS_ID)

combined_data <- bind_rows(national_data, sub_national_data)

# Creating the plot for Figure 2)a)

# Create an ordering variable based on the average per capita budget
order_data <- combined_data %>%
  filter(region == "Country Average") %>%
  arrange(desc(per_capita_budget)) %>%
  mutate(order = row_number())

# Merge the ordering back into the main dataset
combined_data <- combined_data %>%
  left_join(order_data %>% select(country, order), by = "country")

# Creating the plot with ordered y-axis based on country average and color
# gradient for NUTS2 regions

plot_ets <-
  ggplot(combined_data, aes(x = per_capita_budget, y = reorder(country, order))) +
  geom_point(data = filter(combined_data, region != "Country Average"), 
             # Apply color based on per capita budget
             aes(color = per_capita_budget),  
             size = 2, alpha = 0.6) +
  # Use viridis color scale; , guide = FALSE REMOVES LEGEND
  scale_color_viridis(option = "D", direction = 1) +  
  geom_point(data = filter(combined_data, region == "Country Average"), 
             shape = 15, size = 2, color = "black") +
  # alternative legend title: tons CO2e\nper capita
  labs(x = "tons CO2e per capita", y = "Country", color = "") + 
theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave('./plots/nuts2_ets_pbe_alloc_dotplot.pdf', plot_ets, 
       width = 90, height = 105, units = "mm")

#-----------------------------------------------------------------------------#
# 4. Analysis of regional PBE budgets (ESR+ETS)                            ----
#-----------------------------------------------------------------------------#

pbe_budget_alloc_nation_scale_EU <- 
  pbe_es_budget_alloc_nation_scale_EU %>% select(-avg_pop) %>%
  left_join(pbe_ets_budget_alloc_nation_scale_EU, by = "ISO") %>%
  mutate(pbe_budget = es_budget + ets_budget) %>%
  select(-c(es_budget, ets_budget))

pbe_budget_alloc_nuts2_scale_det <-
  pbe_es_budget_alloc_nuts2_scale_det %>%
  left_join(pbe_ets_budget_alloc_nuts2_scale_det %>% select(-ISO), by = "NUTS_ID") %>%
  mutate(pbe_budget = es_budget + ets_budget) #%>%
#select(-c(es_budget, ets_budget))

# combine the data into one dataset to make plots 
national_data <-
  pbe_budget_alloc_nation_scale_EU %>% 
  rename(country = ISO, per_capita_budget = pbe_budget) %>%
  mutate(region = "Country Average")

sub_national_data <-
  pbe_budget_alloc_nuts2_scale_det %>% 
  rename(country = ISO, per_capita_budget = pbe_budget, region = NUTS_ID)

combined_data <- bind_rows(national_data, sub_national_data)

# Creating the plot (this plot is not presented in the paper)

# Create an ordering variable based on the average per capita budget
order_data <- combined_data %>%
  filter(region == "Country Average") %>%
  arrange(desc(per_capita_budget)) %>%
  mutate(order = row_number())

# Merge the ordering back into the main dataset
combined_data <- combined_data %>%
  left_join(order_data %>% select(country, order), by = "country")

# Creating the plot with ordered y-axis based on country average and color
# gradient for NUTS2 regions

ggplot(combined_data, aes(x = per_capita_budget, y = reorder(country, order))) +
  geom_point(data = filter(combined_data, region != "Country Average"), 
             aes(color = per_capita_budget),  
             size = 2, alpha = 0.6) +
  scale_color_viridis(option = "D", direction = 1) +  
  geom_point(data = filter(combined_data, region == "Country Average"), 
             shape = 15, size = 2, color = "black") +
  labs(x = "tons CO2e per capita", y = "Country", color = "") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#-----------------------------------------------------------------------------#
# 5. Analysis of regional CBE budgets                                      ----
#-----------------------------------------------------------------------------#

# make dataset incl CBE budgets for EU countries
cbe_budget_alloc_nation_scale_EU <-
  cbe_global_alloc_nation_scale %>% 
  filter(ISO %in% c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST",
                    "FIN", "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA",
                    "LTU", "LUX", "MLT", "NLD","POL", "PRT", "ROU", "SVK",
                    "SVN", "ESP", "SWE")) %>% 
  select(c(ISO, epc_s, avg_pop)) %>% 
  # attention, here per capita budgets in terms of avg population
  mutate(cbe_budget = epc_s/avg_pop) %>% 
  select(-avg_pop) 

# extract the two ID‐vectors
ids_pbe <- pbe_budget_alloc_nuts2_scale_det$NUTS_ID
ids_cbe <- cbe_eu_alloc_nuts2_scale_hqual$NUTS_ID

# in pbe but not in cbe:
setdiff(ids_pbe, ids_cbe)
# in cbe but not in pbe:
setdiff(ids_cbe, ids_pbe)
# "ES70" "FRY1" "FRY2" "FRY3" "FRY4" "FRY5" "PT20" "PT30"

# make dataset incl PBE budgets for NUTS2 countries
cbe_budget_alloc_nuts2_scale_det <-
  cbe_eu_alloc_nuts2_scale_hqual %>% 
  filter(! NUTS_ID %in% 
           c("ES70", "FRY1", "FRY2", "FRY3", "FRY4", "FRY5", "PT20", "PT30")) %>% 
  mutate(avg_pop = (pop_21+pop_50)/2) %>% 
  mutate(cbe_budget = eu_epc_s*1000000000/avg_pop) %>% 
  select(c(NUTS_ID, ISO, cbe_budget)) %>% 
  # note we exclude PRT regional budgets because no consistent historical
  # emissions dataset due to change in NUTS-2 classification also needed for NLD
  mutate(cbe_budget = case_when(ISO %in% c("PRT", "NLD") ~ NA, TRUE ~ cbe_budget))

# combine the data into one dataset to make plots 
national_data <-
  cbe_budget_alloc_nation_scale_EU %>% 
  rename(country = ISO, per_capita_budget = cbe_budget) %>%
  mutate(region = "Country Average") %>% 
  select(-epc_s)

sub_national_data <-
  cbe_budget_alloc_nuts2_scale_det %>% 
  rename(country = ISO, per_capita_budget = cbe_budget, region = NUTS_ID)

combined_data <- bind_rows(national_data, sub_national_data)

# Creating the plot for Figure 3)b)

# Create an ordering variable based on the average per capita budget
order_data <- combined_data %>%
  filter(region == "Country Average") %>%
  arrange(desc(per_capita_budget)) %>%
  mutate(order = row_number())

# Merge the ordering back into the main dataset
combined_data <- combined_data %>%
  left_join(order_data %>% select(country, order), by = "country")

# Creating the plot with ordered y-axis based on country average and color gradient for NUTS2 regions

plot_cbe <- 
  ggplot(combined_data, aes(x = per_capita_budget, y = reorder(country, order))) +
  geom_point(data = filter(combined_data, region != "Country Average"), 
             aes(color = per_capita_budget),  # Apply color based on per capita budget
             size = 2, alpha = 0.6) +
  scale_color_viridis(option = "D", direction = 1) +  # Use viridis color scale , , guide = FALSE removes legend
  geom_point(data = filter(combined_data, region == "Country Average"), 
             shape = 15, size = 2, color = "black") +
  labs(x = "t CO2 per capita", y = "Country", color = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# attention to missing data in NL und PT

ggsave('./plots/nuts2_cbe_alloc_dotplot.pdf', plot_cbe, 
       width = 90, height = 105, units = "mm")

# explore in which countries variance in regional budgets is highest
cbe_budget_alloc_nuts2_scale_det %>%
  # drop rows with missing budgets
  filter(!is.na(cbe_budget)) %>%
  # group by country
  group_by(ISO) %>%
  # compute variability metrics
  summarise(
    n_regions   = n(),
    mean_budget = mean(cbe_budget),
    var_budget  = var(cbe_budget),                         # variance
    sd_budget   = sd(cbe_budget),                          # standard deviation
    cv_budget   = sd_budget / mean_budget,                 # coefficient of variation
    iqr_budget  = IQR(cbe_budget)                          # interquartile range
  ) %>%
  # order by descending inequality (e.g. by coefficient of variation)
  arrange(desc(cv_budget))

#-----------------------------------------------------------------------------#
# 5. Table for SI                                      ----
#-----------------------------------------------------------------------------#

# make table for Supplementary material

# create table 
library(eurostat)

# 1. Get the NUTS geo‐dictionary and filter to NUTS‑2 (4‑character codes)
nuts2_labels <- get_eurostat_dic("geo") %>%
  filter(nchar(code_name) == 4) %>%
  rename(NUTS_ID = code_name, RegionName = full_name)

# 2. Build your budgets table, joining in NUTS2 names
budgets <- pbe_budget_alloc_nuts2_scale_det %>%
  select(ISO, NUTS_ID, es_budget, ets_budget) %>%
  left_join(cbe_budget_alloc_nuts2_scale_det %>% select(NUTS_ID, cbe_budget),
            by = "NUTS_ID") %>%
  left_join(nuts2_labels, by = "NUTS_ID") %>%
  # reorder so RegionName sits right after the code
  select(ISO, NUTS_ID, RegionName, es_budget, ets_budget, cbe_budget)

# 3. Tidy up column names (including our new “Region name”)
colnames(budgets) <- c(
  "Country",
  "NUTS-2",
  "Region name",
  "PBE budget (ESR) \\newline [t CO2e per capita]",
  "PBE budget (ETS) \\newline [t CO2e per capita]",
  "CBE budget \\newline [t CO2 per capita]"
)

# 4. Render LaTeX with xtable as before
library(xtable)
latex_table <- xtable(
  budgets,
  caption = "Regional carbon budgets for the EU27 at NUTS-2 scale. Note that PBE
  budgets are derived - in line with EU targets - for all GHG emissions
  (measured in CO2e) while CBE budgets are expressed in terms of CO2 emissions
  because CBE data is only available in CO2. Missing values for regions in
  Portugal and the Netherlands are due to missing consistent historical data on
  regional income to estimate CBE."
)

# print(
#  latex_table,
#  type = "latex",
#  file = "table_final.tex",
#  include.rownames = FALSE,
#  tabular.environment = "longtable",
#  floating = FALSE,
#  sanitize.colnames.function = identity
# )

# correlation ETS + ESR
plot(pbe_budget_alloc_nuts2_scale_det$es_budget,
     pbe_budget_alloc_nuts2_scale_det$ets_budget)
cor(pbe_budget_alloc_nuts2_scale_det$es_budget,
    pbe_budget_alloc_nuts2_scale_det$ets_budget)
# 0.08291029

# correlation PBE + CBE
plot(pbe_budget_alloc_nuts2_scale_det$pbe_budget,
     cbe_budget_alloc_nuts2_scale_det$cbe_budget)
cor(pbe_budget_alloc_nuts2_scale_det$pbe_budget,
    cbe_budget_alloc_nuts2_scale_det$cbe_budget, use="complete.obs")
# 0.13

#-----------------------------------------------------------------------------#
# 6. Compare allocation mechanisms (ESR)                                  ----
#-----------------------------------------------------------------------------#

cl <- es_pbe_eu_alloc_nuts2_scale_ESR %>%
  group_by(sector, ETS) %>% summarise(capital_lifetime=unique(capital_lifetime))

# 1. Compute per-capita allocations for all regions
alloc_all <- es_pbe_eu_alloc_nuts2_scale_ESR %>% 
  # collapse to NUTS2 level first
  summarise(
    e_21  = sum(e_21,  na.rm = TRUE),
    ETOPA_a = sum(sect_b, na.rm = TRUE),
    pop_21 = first(pop_21),
    pop_50 = first(pop_50),
    .by = c(CNTR_CODE, NUTS_ID)
  ) %>%
  mutate(pop_avg = (pop_21 + pop_50) / 2) %>%
  group_by(CNTR_CODE) %>%
  mutate(
    national_budget = sum(ETOPA_a, na.rm = TRUE),
    EPC_a      = national_budget * pop_avg / sum(pop_avg, na.rm = TRUE),
    e_21_share = e_21 / sum(e_21, na.rm = TRUE),
    GF_a       = national_budget * e_21_share
  ) %>%
  ungroup() %>%
  mutate(
    ETOPA = ETOPA_a * 1e9 / pop_avg,
    EPC   = EPC_a   * 1e9 / pop_avg,
    GF    = GF_a    * 1e9 / pop_avg
  )

# 2. Pairwise differences between mechanisms
mechs <- c("GF", "ETOPA", "EPC")

pairwise_diffs <- map_dfr(combn(mechs, 2, simplify = FALSE), function(p) {
  m1 <- p[1]; m2 <- p[2]
  alloc_all %>%
    transmute(
      CNTR_CODE, NUTS_ID,
      pair      = paste(m1, "vs", m2),
      diff      = .data[[m2]] - .data[[m1]],
      abs_diff  = abs(diff),
      rel_diff  = diff / .data[[m1]]
    )
})

# 3. Where is difference between GF_pc and ETOPA_pc largest? 
top_GF_ETOPA <- pairwise_diffs %>%
  filter(pair == "GF vs ETOPA") %>%
  arrange(desc(abs_diff)) %>%
  slice_head(n = 10)   # change to 1 for only the max

# 4. Summary tables for appendix 
summary_country <- pairwise_diffs %>%
  group_by(CNTR_CODE, pair) %>%
  summarise(
    mean_abs_diff = mean(abs_diff, na.rm = TRUE),
    median_abs_diff = median(abs_diff, na.rm = TRUE),
    max_abs_diff  = max(abs_diff, na.rm = TRUE),
    region_max    = NUTS_ID[which.max(abs_diff)],
    mean_rel_diff = mean(rel_diff, na.rm = TRUE),
    .groups = "drop"
  )

summary_country_wide <- summary_country %>%
  tidyr::pivot_wider(
    names_from  = pair,
    values_from = c(mean_abs_diff, max_abs_diff, region_max, mean_rel_diff),
    names_glue  = "{pair}__{.value}"
  )

summary_region <- pairwise_diffs %>%
  select(CNTR_CODE, NUTS_ID, pair, diff, abs_diff, rel_diff)

eu_summary <- pairwise_diffs %>%
  group_by(pair) %>%
  summarise(
    mean_abs_diff  = mean(abs_diff, na.rm = TRUE),
    median_abs_diff= median(abs_diff, na.rm = TRUE),
    max_abs_diff   = max(abs_diff, na.rm = TRUE),
    top_region     = NUTS_ID[which.max(abs_diff)],
    top_country    = CNTR_CODE[which.max(abs_diff)],
    rel_mean       = mean(rel_diff, na.rm = TRUE),
    .groups = "drop"
  )

max_rel <- alloc_all %>%
  mutate(rel_ETOPA_GF = abs(ETOPA - GF) / GF) %>%
  slice_max(rel_ETOPA_GF, n = 1, with_ties = FALSE)

top10_rel <- alloc_all %>%
  mutate(rel_ETOPA_GF = abs(ETOPA - GF) / GF) %>%
  arrange(desc(rel_ETOPA_GF)) %>%
  slice_head(n = 10)

# 5. Example plot: lollipop of GF_pc - ETOPA_pc for each country
# (faceted; adjust scales/labels as needed)

alloc_all %>% filter(CNTR_CODE %in% c("BE", "ES", "NL", "PT", "FI", "DK")) %>% 
  mutate(diff_ETOPA_GF = (ETOPA - GF)/GF) %>%
  group_by(CNTR_CODE) %>%
  arrange(desc(abs(diff_ETOPA_GF))) %>%
  ggplot(aes(x = reorder(NUTS_ID, diff_ETOPA_GF), y = diff_ETOPA_GF)) +
  geom_segment(aes(xend = NUTS_ID, y = 0, yend = diff_ETOPA_GF)) +
  geom_point() +
  coord_flip() +
  facet_wrap(~ CNTR_CODE, scales = "free_y") +
  labs(x = "NUTS-2 region",
       y = "Relative change in ESR budget (ETOPA vs. Grandfathering): (ETOPA – GF) / GF",
       title = "") +
  theme_minimal()

# comparison of all three budget types

mechs <- c("ETOPA", "GF", "EPC")
sel_countries <- c("BE", "ES", "NL", "PT", "FI", "DK")

# Order regions within each country (use avg of the 3 budgets; pick another if you prefer)
alloc_sel <- alloc_all %>%
  filter(CNTR_CODE %in% sel_countries) %>%
  mutate(avg_pc = rowMeans(across(all_of(mechs)), na.rm = TRUE)) %>%
  group_by(CNTR_CODE) %>%
  mutate(NUTS_ID_ord = fct_reorder(NUTS_ID, avg_pc)) %>%
  ungroup()

# Segment data (min→max per region)
seg_df <- alloc_sel %>%
  transmute(
    CNTR_CODE, NUTS_ID_ord,
    x_min = pmin(ETOPA, GF, EPC, na.rm = TRUE),
    x_max = pmax(ETOPA, GF, EPC, na.rm = TRUE)
  )

# Points data (long)
pts_df <- alloc_sel %>%
  pivot_longer(all_of(mechs),
               names_to = "mechanism",
               values_to = "budget_pc") %>%
  select(CNTR_CODE, NUTS_ID_ord, mechanism, budget_pc)

ggplot() +
  geom_segment(data = seg_df,
               aes(x = x_min, xend = x_max, y = NUTS_ID_ord, yend = NUTS_ID_ord),
               colour = "grey75", linewidth = 0.4) +
  geom_point(data = pts_df,
             aes(x = budget_pc, y = NUTS_ID_ord, colour = mechanism),
             size = 2) +
  facet_wrap(~ CNTR_CODE, scales = "free_y") +
  labs(x = "ESR budget [t CO2e per capita]",
       y = "NUTS-2 region",
       colour = "Mechanism") +
  theme_minimal() +
  theme(
    legend.position = "bottom",      # put legend underneath
    legend.direction = "horizontal"  # lay out items in a row
  ) +
  guides(
    colour = guide_legend(
      nrow = 1,       # force a single row
      byrow = TRUE    # fill legend items by row
    )
  )

# investigate capital use times 
NL <- 
  es_pbe_eu_alloc_nuts2_scale_ESR %>% filter(CNTR_CODE == "NL") %>% 
  group_by(NUTS_ID) %>% 
  summarise(avg_cl=weighted.mean(capital_lifetime, w=e_21)) 


