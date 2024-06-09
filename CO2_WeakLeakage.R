# This is a replication script for "On the Interpretation and Measurement of Technology-Adjusted Emissions Embodied in Trade"
# The script imports EXIOBASE3 and creates the paper's tables and figures.
# Author: Aldy Darwili

# Load Libraries ----
packages = c("tidyverse",
             "openxlsx",
             "ggthemes",
             "ggrepel",
             "hrbrthemes",
             "magrittr",
             "rsdmx",
             "kableExtra",
             "pwt10",
             "sandwich",
             "stargazer",
             "zoo",
             "xtable")

# Now load or install&load all
package.check = lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

# 1. Metadata --------- 
## Period of Analysis ---------
year <- seq(from = 1995, to = 2015)

## Sector List ------
Sectors <- read.delim(paste0("dataraw/IOT_", 2015, "_ixi/industries.txt", sep = ""), stringsAsFactors = FALSE) %>% 
  rename(sector = 2)

## Country List -----
Countries <- read.delim(paste0("dataraw/IOT_", 2015, "_ixi/unit.txt", sep = ""), stringsAsFactors = FALSE) %>% 
  select(region) %>% 
  distinct()

# focus countries
focus <- c("China", "Germany", "United Kingdom","Japan", "India", "United States")

## Country-Sector Combination------
# expected dimension: 163 * 49 = 7987 
Order <- expand_grid(Countries, Sectors %>% select(-Number, -CodeTxt))

## Joining Region & Sector into a column to create unique values
Region_CodeNr <- Order %>% 
  select(region, CodeNr) %>% 
  unite("reg_CodeNr", region:CodeNr, sep = "_")

## Country Full Name
Region_Full <- read.delim(paste0("dataraw/region.txt"), stringsAsFactors = FALSE) %>% 
  set_colnames(c("region", "region_full")) %>% 
  left_join(Countries, ., by = c("region"))

# 2. Data Preparation --------
## CO2, Gross output, and emissions intensity-------
for (i in year) {
  # gross output
  x <- read.delim(paste0("dataraw/IOT_", i,"_ixi/X.txt", sep = ""), stringsAsFactors = FALSE) %>% 
    mutate(indout = as.numeric(indout)) %>% 
    right_join(Order %>% 
                 select(-CodeNr), ., by = c("region", "sector")) %>% # Adding Country-Sector columns
    mutate(indout = case_when(indout < 1 ~ 0, indout > 1 ~ indout), # replacing small sectors (< 1 million euro) with 0
           mapping = case_when(indout == 0 ~ 0, indout != 0 ~ 1)) # creating a categorical column; 0 for sectors with 0 gross output sector, 1 for GO > 0 
  
  # direct emissions vector
  f <- read.delim(paste0("dataraw/IOT_", i,"_ixi/satellite/F.txt", sep = ""), stringsAsFactors = FALSE) %>% 
    filter(!(region %in% c("category", "region"))) %>% # removing unused rows
    rename("stressor" = "region") %>% 
    filter(grepl("CO2", stressor)) %>% # retrieving rows containing relevant CO2 emissions categories
    select(-stressor) %>% 
    set_colnames(c(as_vector(Region_CodeNr))) %>% 
    gather("reg_CodeNr", "f", 1:7987) %>% 
    mutate(f = as.numeric(f)) %>% 
    group_by(reg_CodeNr) %>% 
    summarise(f = sum(f, na.rm = TRUE)) %>% # sum over all relevant CO2 categories to create vector of CO2 emissions
    ungroup() %>% 
    right_join(Region_CodeNr %>% select(reg_CodeNr), ., by = c("reg_CodeNr")) %>%  # rearranging rows
    separate(., reg_CodeNr, into = c("region", "CodeNr"), sep = "_") %>% # separating a joined country-sector
    right_join(Order, ., by = c("region", "CodeNr")) # adding Country-Sector columns
  
  f <- f %>% 
    left_join(x, by = c("region", "sector")) %>% # joining direct emissions with gross output
    select(region, sector, f, indout) %>% 
    mutate(f = case_when(indout == 0 ~ 0, indout != 1 ~ f)) # setting emissions value with 0 gross output to 0
  
  # emissions intensity
  q <- f %>% 
    mutate(q = f/indout, # calculating emissions intensity
           q = replace(q, is.na(q), 0)) %>%  # removing possible NA values
    select(q)
  
  saveRDS(f, file = paste0("dataraw/IOT_", i,"_ixi/f_nosmall", "_", as.character(i), ".rds"))
  saveRDS(x, file = paste0("dataraw/IOT_", i,"_ixi/x_nosmall", "_", as.character(i), ".rds"))
  saveRDS(q, file = paste0("dataraw/IOT_", i,"_ixi/q_nosmall", "_", as.character(i), ".rds"))
  rm(f, x, q)
  gc()
}

## Final demand data ----
for (i in year){
  # final demand from raw data
  y <- read.delim(paste0("dataraw/IOT_", i,"_ixi/Y.txt", sep = ""), stringsAsFactors = FALSE) %>%
    filter(!(region %in% c("category", "region"))) %>% # removing unused rows
    rename("sector" = "X") %>% 
    left_join(Order, by = c("region", "sector")) %>% # joining sector Order data with sector code
    select(region, CodeNr, everything(), -sector) %>% 
    pivot_longer(., 3:345, names_to = "region_category") %>% # transforming wide to long table
    separate(., region_category, into = c("region_consumption", "category"), sep = 2) %>% 
    mutate(value = as.numeric(value)) %>% 
    group_by(region, CodeNr, region_consumption) %>% # grouping by supplying region and supply sector 
    summarise(value = sum(value, na.rm = TRUE)) %>% # aggregating final demand categories
    ungroup() %>% 
    pivot_wider(names_from = "region_consumption", values_from = "value") %>% # transforming from long to wide table
    right_join(Order %>% select(-sector), ., by = c("region", "CodeNr")) %>% # rearranging rows
    select(c(Countries$region)) %>% # rearranging columns
    as.matrix() # transforming final demand table into matrix class
  
  # loading gross output data
  x <- readRDS(paste0("dataraw/IOT_", i,"_ixi/x_nosmall", "_", i, ".rds")) %>% 
    select(mapping) %>% 
    as_vector()
  
  # creating clean final demand matrix
  y <- (x * y) %>% # removing rows in final demand with very small sectors and sectors with 0 gross output*
    data.frame() 
  # *this is done by matrix pre-multiplication of mapping vector and final demand matrix 
  
  saveRDS(y, file = paste0("dataraw/IOT_", i,"_ixi/y_agg_nosmall", "_", as.character(i), ".rds"))
  rm(y, x)
  gc()
}


## Intermediate Input matrix Z ----
for (i in year) {
  # loading intermediate input raw data
  z <- read.delim(paste0("dataraw/IOT_", i,"_ixi/Z.txt", sep = ""), stringsAsFactors = FALSE) %>% 
    filter(!(region %in% c("sector", "region"))) %>% # removing unused rows
    select(-region, -X) %>% 
    set_colnames(c(Order$region))
  
  z[is.na(z)] = 0 # replacing existing NA values with 0
  z[] <- lapply(z, function(x) as.numeric(as.character(x))) # converting data from character to numeric
  z <- as.matrix(z) # transforming table into matrix format
  
  # loading gross output data
  x <- readRDS(paste0("dataraw/IOT_", i,"_ixi/x_nosmall", "_", i, ".rds")) %>% 
    select(mapping) %>% 
    as_vector()

  # creating intermediate input matrix excluding small and 0 gross output sectors
  # setting columns of consuming sectors with very small and zero gross output to 0
  z <- t(t(z) * x)
  
  # setting rows of supplying sectors with very small and zero gross output to 0
  z <- x * z
  
  saveRDS(z, file = paste0("dataraw/IOT_", i,"_ixi/Z_nosmall", "_", as.character(i), ".rds"))
  
  rm(z, x)
  gc()
  
}
  
## Technical Coefficient matrix A -----------
for (i in year) {
  # loading intermediate input matrix x; in matrix format
  z <- readRDS(paste0("dataraw/IOT_", i,"_ixi/Z_nosmall", "_", i, ".rds"))
  
  # loading gross output vector x
  x <- readRDS(paste0("dataraw/IOT_", i,"_ixi/x_nosmall", "_", i, ".rds")) %>% 
    select(indout) %>% 
    as_vector()
  
  # calculating technical coefficient matrix
  a <- t((t(z))/x)

  gc()
  a[is.na(a)] = 0 # replacing NA values with 0
  a[is.infinite(a)] = 0 # replacing infinite values with 0
  
  saveRDS(a, file = paste0("dataraw/IOT_", i,"_ixi/A_nosmall", "_", as.character(i), ".rds"))
  
  rm(z, x, a)
  gc()
}

## Leontief Inverse Matrix ------
for (i in year) {
  # loading technical coefficient matrix
  a <- readRDS(paste0("dataraw/IOT_", i,"_ixi/A_nosmall", "_", i, ".rds"))

  # calculating leontief inverse matrix
  L <- solve(diag(7987) - a, tol = 1e-18) #Matrix inversion
  
  L[is.na(L)] = 0 # replacing NA values with 0
  L[is.infinite(L)] = 0 # replacing infinite values with 0
  
  saveRDS(L, file = paste0("dataraw/IOT_", i,"_ixi/L_nosmall", "_", as.character(i), ".rds"))
  
  rm(a, L)
  gc()
}

# 3. EE MRIO Model: E = q.L.y -------------
## Actual Emissions Embodied in Input-Output ------
for (i in year) {
  #Leontief inverse matrix
  L <- readRDS(paste0("dataraw/IOT_", i,"_ixi/L_nosmall", "_", as.character(i), ".rds"))
  
  #Final demand aggregate
  y <- readRDS(paste0("dataraw/IOT_", i,"_ixi/y_agg_nosmall", "_", as.character(i), ".rds")) %>% 
    as.matrix()
  
  #Emission Intensity
  q <- readRDS(paste0("dataraw/IOT_", i,"_ixi/q_nosmall", "_", as.character(i), ".rds")) %>% 
    as_vector()
  
  #EE MRIO model
  E <- q * (L %*% y) # expected dimension: 163 x 49 x 49
  
  saveRDS(E, file = paste0("dataraw/IOT_", i,"_ixi/E_nosmall", "_", as.character(i), ".rds"))
  
  rm(q, L, y, E)
  gc()
}

## Emission-intensity-adjusted EE MRIO model --------
# Emissions Intensity is adjusted with weighted average based on Kander (2015);
# Kander et. al. (2015) use exported output as weight, we use total gross output as weight.

for (i in year) {
  ## loading direct emissions vector
  f <- readRDS(paste0("dataraw/IOT_", i,"_ixi/f_nosmall", "_", as.character(i), ".rds")) %>% 
    rename(f = 3, x = 4) %>% 
    data.frame()

  # calculating world average emission intensity 
  q_avg <- f %>% 
    mutate(q = f / x, # calculating actual emission intensity
           q = replace(q, is.na(q), 0)) %>% # replacing NA values with 0
    group_by(sector) %>% # grouping by sector
    mutate(q_avg = sum(q*x, na.rm = TRUE) / sum(x, na.rm = TRUE), # calculating world average emission intensity
           q_avg = replace(q_avg, is.na(q_avg), 0), # replacing NA values with 0
           q_avg = replace(q_avg, is.infinite(q_avg), 0)) %>%  # replacing infinite values with 0
    ungroup() %>%
    right_join(Order %>% select(-CodeNr), ., by = c("region", "sector")) %>% # rearrange rows
    select(q_avg) %>% 
    as_vector()
  
  # loading Leontief inverse matrix
  L <- readRDS(paste0("dataraw/IOT_", i,"_ixi/L_nosmall", "_", as.character(i), ".rds"))
  
  # loading aggregate final demand matrix
  y <- readRDS(paste0("dataraw/IOT_", i,"_ixi/y_agg_nosmall", "_", as.character(i), ".rds")) %>% 
    as.matrix()
  
  #Intensity-adjusted EEIO; Kander
  E_knd <- q_avg * (L %*% y) # expected dimension: 163 x 49 x 49
  
  saveRDS(E_knd, file = paste0("dataraw/IOT_", i,"_ixi/E_knd_nosmall", "_", as.character(i), ".rds"))
  saveRDS(q_avg, file = paste0("dataraw/IOT_", i,"_ixi/q_avg_nosmall", "_", as.character(i), ".rds"))
  
  rm(q_avg, L, y, E_knd)
  gc()
  
}

## Technology-adjusted EE MRIO ----
### Standardizing Leontief Inverse Matrix----
for (i in year) {
  # loading technical coefficient data
  a <- readRDS(paste0("dataraw/IOT_", i,"_ixi/A_nosmall", "_", i, ".rds"))
  
  # loading gross output
  x <- readRDS(paste0("dataraw/IOT_", i,"_ixi/x_nosmall", "_", i , ".rds")) %>% 
    select(indout) %>% 
    bind_cols(Region_CodeNr, .) %>%
    rename(x = 2)
  
  # creating total sector input matrix h (n x mn; 163 x 7987)
  h <- a %>%
    as_tibble() %>%
    bind_cols(Order %>% select(CodeNr),.) %>%
    group_by(CodeNr) %>% # grouping by sector
    summarise(across(everything(), sum)) %>% # column sum of input coeff. matrix by sector
    ungroup() %>%
    right_join(Sectors %>% select(CodeNr), ., by = c("CodeNr")) %>% # rearrange data
    set_colnames(c("CodeNr", as_vector(Region_CodeNr))) # rename columns
  
  # calculating average total sector input matrix; (n x n; 163 x 163)
  H_avg <- h %>%
    gather("reg_CodeNr", "h", 2:7988) %>% # transfrom wide table to long table format
    left_join(x, by = c("reg_CodeNr")) %>% # adding gross output into the table
    rename(supplying_sector = 1) %>% 
    separate(., reg_CodeNr, into = c("consuming_region", "consuming_sector"), sep = "_") %>% 
    group_by(consuming_sector, supplying_sector) %>% 
    mutate(hx = sum(h * x, na.rm = TRUE), # sum of numerator in weighted-average calculation
           x_sum = sum(x, na.rm = TRUE), # sum of denominator in weighted-average calculation
           h_std = hx / x_sum) %>% # weighted average
    ungroup() %>% 
    select(supplying_sector, consuming_sector, h_std) %>% 
    distinct_all() %>% # this function works like unique(), but across all column. Here, we get 163 x 163 world average total input matrix in long table format
    mutate(h_std = replace(h_std, is.na(h_std), 0)) %>% # replace possible NA values with 0
    spread(., consuming_sector, h_std) %>% # transforming long table to wide table format
    rename(CodeNr = 1) %>% 
    right_join(Sectors %>% select(CodeNr), ., by = c("CodeNr")) %>% # reordering row
    select(c(as_vector(Sectors$CodeNr))) %>% # reordering column
    .[,rep(1:163, 49)] %>% # This is to horizontally replicate 163 x 163 world average total input, 49 times (number of countries)
    rbind(., .[rep(1:163, 48),]) %>% # this is to vertically replicate 163 x 163 x 49 world average total input matrix. Resulting dimension is 163 x 49 x 163 x 49
    as.matrix() # transforming table into matrix format
  
  # trade structure matrix, T
  T <- a / (h %>% # hadamard multiplication of input coefficiient matrix and 1/h  
              select(-CodeNr) %>% 
              rbind(., .[rep(1:163, 48),]) %>% # this is to vertically replicate 163 x 163 x 49 total input matrix. Resulting dimension is 163 x 49 x 163 x 49
              as.matrix()) 
  
  is.na(T) <- sapply(T, is.infinite) #removing possible infinite value
  T[is.na(T)] = 0 #removing possible NA value
  
  # standardized technical coefficient matrix
  A_std <- as.matrix(T) * H_avg # hadamard multiplication of trade structure matrix T and world average total input matrix
  rm(a, h, H_avg, T)
  gc()
  
  L_std <- solve(diag(7987) - A_std, tol = 1e-18) # Calculating standardized leontief inverse matrix
  
  saveRDS(L_std, file = paste0("dataraw/IOT_", i,"_ixi/L_std_nosmall", "_", i, ".rds"))
  rm(L_std, A_std)
  gc()
}

### Technology-adjusted EE MRIO model----
for (i in year) {
  # loading standardized Leontief inverse matrix
  L_std <- readRDS(paste0("dataraw/IOT_", i,"_ixi/L_std_nosmall", "_", as.character(i), ".rds"))
  
  # loading final demand aggregate
  y <- readRDS(paste0("dataraw/IOT_", i,"_ixi/y_agg_nosmall", "_", as.character(i), ".rds"))
  
  # loading world average emission intensity 
  q_avg <- readRDS(paste0("dataraw/IOT_", i,"_ixi/q_avg_nosmall", "_", as.character(i), ".rds"))
  
  #Technology-adjusted EE MRIO model
  E_tech_adj <- as_vector(q_avg) * (L_std %*% as.matrix(y)) # expected dimension: 163 x 49 x 49
  
  saveRDS(E_tech_adj, file = paste0("dataraw/IOT_", i,"_ixi/E_tadj_nosmall", "_", as.character(i), ".rds"))
  rm(q_avg, L_std, y, E_tech_adj)
  gc()
}


# 4. Gathering Data -----

##Household Direct Emissions -----
list_Hh <- list()

for (i in year) {
  # loading household direct emissions raw data
  HHE <- read.delim(paste0("dataraw/IOT_", i,"_ixi/satellite/F_Y.txt", sep = ""), stringsAsFactors = FALSE) %>%
    filter(!(region %in% c("stressor", "category"))) %>% # removing unused rows
    rename("stressor" = "region") %>%
    filter(grepl("CO2", stressor)) %>% # getting rows containing relevant CO2 emissions categories
    select(-stressor)
  
  # converting from character to numeric
  HHE[] <- lapply(HHE, function(x) as.numeric(as.character(x))) # converting data from character to numeric
  
  # preparing household direct emissions data
  HHE <- HHE %>% 
    colSums() %>% 
    data.frame() %>% 
    rename(HHE = 1) %>% 
    rownames_to_column() %>% 
    separate(., rowname, into = c("reg_prod", "category"), sep = 2) %>% 
    select(-category) %>% 
    group_by(reg_prod) %>% 
    mutate(HHE = sum(HHE, na.rm = TRUE)) %>% 
    distinct_all() %>% 
    left_join(Region_Full %>% rename(reg_prod = 1), ., by = c("reg_prod")) %>%
    mutate(year = i) %>% 
    select(region_full, year, HHE) %>% 
    rename(reg_prod = 1)
  
  list_Hh[[as.character(i)]] <- HHE
  rm(HHE)
}

Hh <- bind_rows(list_Hh) %>% 
  mutate(reg_cons = reg_prod)


## Actual Emissions Embodied in Input Output ------
list_EEIO_agg_long <- list()

for (i in year) {
  # Real, or actual, emissions
  list_EEIO_agg_long[[as.character(i)]] <- readRDS(paste0("dataraw/IOT_", i,"_ixi/E_nosmall", "_", as.character(i), ".rds")) %>% # loading 163 x 49 x 49 emissions data
    data.frame() %>% 
    bind_cols(Order %>% select(region), .) %>% # this is to add column with region data
    group_by(region) %>% 
    summarise_all(list(sum = sum), na.rm = TRUE) %>% # this is to calculate sum of emission over all sectors for each country
    ungroup() %>% 
    left_join(Region_Full, ., by = c("region")) %>% # reorder data
    mutate(year = i) %>% # creating year column
    select(region_full, year, everything(), -region) %>% 
    set_colnames(c("reg_prod", "year", as_vector(Region_Full %>% select(region_full)))) %>% # this is to rename the column with country full name
    pivot_longer(3:51, names_to = "reg_cons", values_to = "CO2_emission") %>% 
    select(reg_prod, reg_cons, year, CO2_emission)
}

EEIO_agg_long <- bind_rows(list_EEIO_agg_long) %>% 
  left_join(Hh, by = c("reg_prod", "reg_cons", "year")) %>% 
  mutate(HHE = replace(HHE, is.na(HHE), 0), 
         CO2_emission = CO2_emission + HHE) %>% # joining household direct emissions data
  select(-HHE)

### Emissions Embodied in Domestic Production and Consumption ------
DE <- EEIO_agg_long %>% 
  filter(reg_prod == reg_cons) %>% 
  select(reg_cons, year, CO2_emission) %>% 
  rename(reg = 1, DE = 3) %>% 
  mutate(DE = DE / 1e+12)

### Emissions Embodied in Imports ------
EEI <- EEIO_agg_long %>% 
  filter(reg_prod != reg_cons) %>% 
  group_by(reg_cons, year) %>%
  mutate(EEM = sum(CO2_emission, na.rm = TRUE)) %>% 
  select(reg_cons, year, EEM) %>% 
  distinct_at(c("reg_cons", "year", "EEM")) %>% 
  rename(reg = 1) %>% 
  mutate(EEM = EEM / 1e+12)

### Emissions Embodied in Exports -------
EEE <- EEIO_agg_long %>% 
  filter(reg_prod != reg_cons) %>% 
  group_by(reg_prod, year) %>%
  mutate(EEX = sum(CO2_emission, na.rm = TRUE)) %>% 
  select(reg_prod, year, EEX) %>% 
  distinct_at(c("reg_prod", "year", "EEX")) %>% 
  rename(reg = 1) %>% 
  mutate(EEX = EEX / 1e+12)


## Intensity-adjusted Emissions Embodied in Input Output ---------
list_EEIO_knd_agg_long <- list()

for (i in year) {
#Intensity-adjusted embodied emissions in domestic and international trade (Kander)
list_EEIO_knd_agg_long[[as.character(i)]] <- readRDS(paste0("dataraw/IOT_", i,"_ixi/E_knd_nosmall", "_", as.character(i), ".rds")) %>% # loading 163 x 49 x 49 emissions data
  data.frame() %>% 
  bind_cols(Order %>% select(region), .) %>% # this is to add column with region data
  group_by(region) %>% 
  summarise_all(list(sum = sum), na.rm = TRUE) %>% # this is to calculate sum of emission over all sectors for each country
  ungroup() %>% 
  left_join(Region_Full, ., by = c("region")) %>% # reorder data
  mutate(year = i) %>% # creating year column
  select(region_full, year, everything(), -region) %>% 
  set_colnames(c("reg_prod", "year", as_vector(Region_Full %>% select(region_full)))) %>% # this is to rename the column with country full name
  pivot_longer(3:51, names_to = "reg_cons", values_to = "CO2_emission") %>% 
  select(reg_prod, reg_cons, year, CO2_emission)
}

EEIO_knd_agg_long <- bind_rows(list_EEIO_knd_agg_long) %>% 
  filter(reg_prod != reg_cons) %>% 
  rename(E_knd = 4)

### Intensity-adjusted Emissions Embodied in Exports --------
EEE_knd <- EEIO_knd_agg_long %>% 
  filter(reg_prod != reg_cons) %>% 
  group_by(reg_prod, year) %>%
  mutate(EIEEX = sum(E_knd, na.rm = TRUE)) %>% 
  select(reg_prod, year, EIEEX) %>% 
  distinct_at(c("reg_prod", "year", "EIEEX")) %>% 
  rename(reg = 1) %>% 
  mutate(EIEEX = EIEEX / 1e+12)


## Technology-adjusted Emissions Embodied in Input Output ---------
list_EEIO_Tadj_agg_long <- list()

for (i in year) {
#Technology-adjusted emissions embodied in domestic and international trade
list_EEIO_Tadj_agg_long[[as.character(i)]] <- readRDS(paste0("dataraw/IOT_", i,"_ixi/E_Tadj_nosmall", "_", as.character(i), ".rds")) %>% # loading 163 x 49 x 49 emissions data
  data.frame() %>% 
  bind_cols(Order %>% select(region), .) %>% # this is to add column with region data
  group_by(region) %>% 
  summarise_all(list(sum = sum), na.rm = TRUE) %>% # this is to calculate sum of emission over all sectors for each country
  ungroup() %>% 
  left_join(Region_Full, ., by = c("region")) %>% # reorder data
  mutate(year = i) %>% # creating year column
  select(region_full, year, everything(), -region) %>% 
  set_colnames(c("reg_prod", "year", as_vector(Region_Full %>% select(region_full)))) %>% # this is to rename the column with country full name
  pivot_longer(3:51, names_to = "reg_cons", values_to = "CO2_emission") %>% 
  select(reg_prod, reg_cons, year, CO2_emission)
}

EEIO_Tadj_agg_long <- bind_rows(list_EEIO_Tadj_agg_long) %>% 
  filter(reg_prod != reg_cons) %>% 
  rename(E_Tadj = 4)

### Technology-adjusted Emissions Embodied in Exports -----------
EEE_Tadj <- EEIO_Tadj_agg_long %>% 
  filter(reg_prod != reg_cons) %>% 
  group_by(reg_prod, year) %>%
  mutate(TEEX = sum(E_Tadj, na.rm = TRUE)) %>% 
  select(reg_prod, year, TEEX) %>% 
  distinct_at(c("reg_prod", "year", "TEEX")) %>% 
  rename(reg = 1) %>% 
  mutate(TEEX = TEEX / 1e+12)



# 5. Visualization ------
##Figures ------
### Graph: Emissions Embodied in Exports---------
# All Countries
Data_plot <- EEE %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  rename("Unadjusted" = 3,
         "EI-adjusted" = 4,
         "Tech-adjusted" =5) %>% 
  gather(., "emission", "value", 3:5) %>% 
  mutate(reg = as.character(reg),
         emission = factor(emission, levels = c("Unadjusted", "EI-adjusted", "Tech-adjusted")))

#Custom Potrait
pdf("figs/Line_EEE_EXIOBASE.pdf", width = 17, height = 24)
print(
  ggplot(Data_plot, aes(year, value, color = emission, shape = emission)) +
    geom_line() +
    geom_point() +
    scale_color_wsj() +
    scale_fill_solarized() +
    scale_shape_few() +
    facet_wrap(~reg, scales = "free_y", ncol = 5) +
    theme_pander(base_family = "Times") +
    theme(plot.title = element_text(size = 24, hjust = .5, vjust = 2.5),
          plot.subtitle = element_text(size = 20, hjust = .5, vjust = 2.5),
          plot.caption = element_text(vjust = 4.5),
          plot.background = element_rect(fill = "transparent"),
          legend.background = element_rect(fill = "transparent"),
          legend.position = "top",
          legend.title = element_blank(),
          panel.border = element_rect(color = "white"),
          panel.spacing.y = unit(1, "lines"),
          plot.margin = margin(2, 1, 0, 0, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    labs(y=expression(CO[2]* " Emissions (Gt)"))
)

dev.off()


### Graph: Balance of Emissions Embodied in Trade -----
Data_plot_BEET <- EEE %>% 
  left_join(EEI, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(BEET = EEX - EEM,
         EIBEET = EIEEX - EEM,
         TBEET = TEEX - EEM) %>%
  select(reg, year, BEET, EIBEET, TBEET) %>% 
  rename("Unadjusted" = 3,
         "EI-adjusted" = 4,
         "Tech-adjusted" =5) %>% 
  gather(., "emission", "value", 3:5) %>% 
  mutate(reg = as.character(reg),
         emission = factor(emission, levels = c("Unadjusted", "EI-adjusted", "Tech-adjusted")))

pdf("figs/Line_BEET_EXIOBASE.pdf", width = 17, height = 24)
print(
  ggplot(Data_plot_BEET, aes(year, value, color = emission, shape = emission)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept=0, size = 0.4, lty = 2) +
    scale_color_wsj() +
    scale_fill_solarized() +
    scale_shape_few() +
    facet_wrap(~reg, scales = "free_y", ncol = 5) +
    theme_pander(base_family = "Times") +
    theme(plot.title = element_text(size = 24, hjust = .5, vjust = 2.5),
          plot.subtitle = element_text(size = 20, hjust = .5, vjust = 2.5),
          plot.caption = element_text(vjust = 4.5),
          plot.background = element_rect(fill = "transparent"),
          legend.background = element_rect(fill = "transparent"),
          legend.position = "top",
          legend.title = element_blank(),
          panel.border = element_rect(color = "white"),
          panel.spacing.y = unit(1, "lines"),
          plot.margin = margin(2, 1, 0, 0, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    labs(y=expression(CO[2]* " Emissions (Gt)"))
)

dev.off()

# 6 countries
Data_plot_BEET_6 <- Data_plot_BEET %>% 
  filter(reg %in% focus)

pdf("figs/Line_BEET_selected.pdf", width = 12, height = 6)
print(
  ggplot(Data_plot_BEET_6, aes(year, value, color = emission, shape = emission)) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept=0, size = 0.4, lty = 2) +
    scale_color_wsj() +
    scale_fill_solarized() +
    scale_shape_few() +
    facet_wrap(~reg, scales = "free_y", ncol = 3) +
    theme_pander(base_family = "Times") +
    theme(plot.background = element_rect(fill = "transparent"),
          legend.background = element_rect(fill = "transparent"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          strip.text.x = element_text(size = 16, margin = margin(0,0,0.5,0, "cm")),
          panel.border = element_rect(color = "white"),
          panel.spacing.x = unit(1, "lines"),
          panel.spacing.y = unit(2, "lines"),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
)

dev.off()

### Graph: Technology-adjusted CBA -----
Data_plot_TCBA <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  left_join(EEI, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(CBA = DE + EEM,
         EICBA = DE + EEM + EEX - EIEEX,
         TCBA = DE + EEM + EEX - TEEX,
         PBA = DE + EEX) %>%
  select(reg, year, CBA, EICBA, TCBA, PBA) %>% 
  rename("Unadjusted" = 3,
         "EI-adjusted" = 4,
         "Tech-adjusted" = 5,
         "PB" = 6) %>% 
  gather(., "emission", "value", 3:6) %>% 
  mutate(reg = as.character(reg),
         emission = factor(emission, levels = c("Unadjusted", "EI-adjusted", "Tech-adjusted", "PB")))


pdf("figs/Line_TCBAvsPB2_EXIOBASE.pdf", width = 17, height = 24)
print(
  ggplot(Data_plot_TCBA, aes(year, value, color = emission, shape = emission)) +
    geom_line() +
    geom_point() +
    scale_color_wsj() +
    scale_fill_solarized() +
    scale_shape_few() +
    facet_wrap(~reg, scales = "free_y", ncol = 5) +
    theme_pander(base_family = "Times") +
    theme(plot.title = element_text(size = 24, hjust = .5, vjust = 2.5),
          plot.subtitle = element_text(size = 20, hjust = .5, vjust = 2.5),
          plot.caption = element_text(vjust = 4.5),
          plot.background = element_rect(fill = "transparent"),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.position = "top",
          panel.border = element_rect(color = "white"),
          panel.spacing.y = unit(1, "lines"),
          plot.margin = margin(2, 1, 0, 0, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    labs(y=expression(CO[2]* " Emissions (Gt)"))
)

dev.off()


# 6 countries
Data_plot_TCBA_6 <- Data_plot_TCBA %>% 
  filter(reg %in% focus)

pdf("figs/Line_TCBAvsPB2_selected.pdf", width = 12, height = 6)
print(
  ggplot(Data_plot_TCBA_6, aes(year, value, color = emission, shape = emission)) +
    geom_line() +
    geom_point() +
    scale_color_wsj() +
    scale_fill_solarized() +
    scale_shape_few() +
    facet_wrap(~reg, scales = "free_y", ncol = 3) +
    theme_pander(base_family = "Times") +
    theme(plot.background = element_rect(fill = "transparent"),
          legend.background = element_rect(fill = "transparent"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          strip.text.x = element_text(size = 16, margin = margin(0,0,0.5,0, "cm")),
          panel.border = element_rect(color = "white"),
          panel.spacing.x = unit(1, "lines"),
          panel.spacing.y = unit(2, "lines"),
          plot.margin = margin(0, 0, 0, 0, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
)

dev.off()

## Tables ------
### Table: Balance of Emissions Embodied in Trade; Gt of CO2 ----
dt95 <- EEE %>% 
  left_join(EEI, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(BEET = EEX - EEM,
         EIBEET = EIEEX - EEM,
         TBEET = TEEX - EEM)  %>%
  filter(year == 1995) %>% 
  data.frame() %>% 
  select(reg, BEET, EIBEET, TBEET)

dt05 <- EEE %>% 
  left_join(EEI, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(BEET = EEX - EEM,
         EIBEET = EIEEX - EEM,
         TBEET = TEEX - EEM) %>%
  filter(year == 2005) %>% 
  data.frame() %>% 
  select(BEET, EIBEET, TBEET)

dt15 <- EEE %>% 
  left_join(EEI, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(BEET = EEX - EEM,
         EIBEET = EIEEX - EEM,
         TBEET = TEEX - EEM) %>%
  filter(year == 2015) %>% 
  data.frame() %>% 
  select(BEET, EIBEET, TBEET)

write_data_sel <- bind_cols(dt95 , dt05, dt15) %>%
  arrange(reg) %>% 
  set_colnames(c("reg", "BEET", "EIBEET", "TBEET", "BEET", "EIBEET", "TBEET", "BEET", "EIBEET", "TBEET")) 

# Saving table in LaTex kable format
save_kable(
  kable(write_data_sel, 
        digits = 3,
        caption = "Balance of Emissions Embodiex in Trade (Gt of CO2 Emissions)",
        format="latex", booktabs=TRUE) %>%
    kable_styling(font_size = 8) %>%
    add_header_above(c(" " = 1,"1995" = 3, "2005" = 3, "2015" = 3)),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "tabs/Table_BEET.tex"
)

### Table: Balance of Emissions Embodied in Trade in % of PBE -----
dt95 <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  left_join(EEI, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(BEET = (EEX - EEM) / (DE + EEX) * 100,
         EIBEET = (EIEEX - EEM) / (DE + EEX) * 100,
         TBEET = (TEEX - EEM) / (DE + EEX) * 100)  %>%
  filter(year == 1995) %>% 
  data.frame() %>% 
  select(reg, BEET, EIBEET, TBEET)

dt05 <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  left_join(EEI, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(BEET = (EEX - EEM) / (DE + EEX) * 100,
         EIBEET = (EIEEX - EEM) / (DE + EEX) * 100,
         TBEET = (TEEX - EEM) / (DE + EEX) * 100)  %>%
  filter(year == 2005) %>% 
  data.frame() %>% 
  select(BEET, EIBEET, TBEET)

dt15 <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  left_join(EEI, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(BEET = (EEX - EEM) / (DE + EEX) * 100,
         EIBEET = (EIEEX - EEM) / (DE + EEX) * 100,
         TBEET = (TEEX - EEM) / (DE + EEX) * 100)  %>%
  filter(year == 2015) %>% 
  data.frame() %>% 
  select(BEET, EIBEET, TBEET)

write_data_sel <- bind_cols(dt95 , dt05, dt15) %>%
  arrange(reg) %>% 
  set_colnames(c("reg", "BEET", "EIBEET", "TBEET", "BEET", "EIBEET", "TBEET", "BEET", "EIBEET", "TBEET")) 

# Saving table in LaTex kable format
save_kable(
  kable(write_data_sel, 
        digits = 3,
        caption = "Balance of Emissions Embodiex in Trade (in percentage of PBE)",
        format="latex", booktabs=TRUE) %>%
    kable_styling(font_size = 8) %>%
    add_header_above(c(" " = 1,"1995" = 3, "2005" = 3, "2015" = 3)),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "tabs/Table_BEET_alt.tex"
)


### Table: Consumption-based Emissions; Gt of CO2 ----
dt95 <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  left_join(EEI, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(CBA = DE + EEM,
         EICBA = DE + EEM + EEX - EIEEX,
         TCBA = DE + EEM + EEX - TEEX) %>% 
  filter(year == 1995) %>% 
  data.frame() %>% 
  select(reg, CBA, EICBA, TCBA)

dt05 <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  left_join(EEI, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(CBA = DE + EEM,
         EICBA = DE + EEM + EEX - EIEEX,
         TCBA = DE + EEM + EEX - TEEX) %>% 
  filter(year == 2005) %>% 
  data.frame() %>% 
  select(CBA, EICBA, TCBA)

dt15 <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  left_join(EEI, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(CBA = DE + EEM,
         EICBA = DE + EEM + EEX - EIEEX,
         TCBA = DE + EEM + EEX - TEEX) %>% 
  filter(year == 2015) %>% 
  data.frame() %>% 
  select(CBA, EICBA, TCBA)

write_data_sel <- bind_cols(dt95 , dt05, dt15) %>%
  arrange(reg) %>% 
  set_colnames(c("reg", "CBA", "EICBA", "TCBA", "CBA", "EICBA", "TCBA", "CBA", "EICBA", "TCBA")) 

save_kable(
  kable(write_data_sel, 
        digits = 3,
        caption = "Consumption-Based Emissions (Gt of CO2 Emissions)",
        format="latex", booktabs=TRUE) %>%
    kable_styling(font_size = 8) %>%
    add_header_above(c(" " = 1,"1995" = 3, "2005" = 3, "2015" = 3)),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "tabs/Table_CBA2.tex"
)

### Table: Consumption-based Emissions in % of PBE -----
dt95 <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  left_join(EEI, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(CBA = (DE + EEM)/(DE + EEX)*100,
         EICBA = (DE + EEM + EEX - EIEEX)/(DE + EEX)*100,
         TCBA = (DE + EEM + EEX - TEEX)/(DE + EEX)*100) %>% 
  filter(year == 1995) %>% 
  data.frame() %>% 
  select(reg, CBA, EICBA, TCBA)

dt05 <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  left_join(EEI, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(CBA = (DE + EEM)/(DE + EEX)*100,
         EICBA = (DE + EEM + EEX - EIEEX)/(DE + EEX)*100,
         TCBA = (DE + EEM + EEX - TEEX)/(DE + EEX)*100) %>% 
  filter(year == 2005) %>% 
  data.frame() %>% 
  select(CBA, EICBA, TCBA)

dt15 <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  left_join(EEI, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(CBA = (DE + EEM)/(DE + EEX)*100,
         EICBA = (DE + EEM + EEX - EIEEX)/(DE + EEX)*100,
         TCBA = (DE + EEM + EEX - TEEX)/(DE + EEX)*100) %>% 
  filter(year == 2015) %>% 
  data.frame() %>% 
  select(CBA, EICBA, TCBA)

write_data_sel <- bind_cols(dt95 , dt05, dt15) %>%
  arrange(reg) %>% 
  set_colnames(c("reg", "CBA", "EICBA", "TCBA", "CBA", "EICBA", "TCBA", "CBA", "EICBA", "TCBA")) 

save_kable(
  kable(write_data_sel, 
        digits = 2,
        caption = "Consumption-Based Emissions (in percentage of PBE)",
        format="latex", booktabs=TRUE) %>%
    kable_styling(font_size = 8) %>%
    add_header_above(c(" " = 1,"1995" = 3, "2005" = 3, "2015" = 3)),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "tabs/Table_CBA2_alt.tex"
)

### Table: Emission Embodied in Export;  Gt of CO2-------
dt95 <- EEE %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year"))  %>%
  filter(year == 1995) %>% 
  data.frame() %>% 
  select(-year)

dt05 <- EEE %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>%
  filter(year == 2005) %>% 
  data.frame() %>% 
  select(3:5)

dt15 <- EEE %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>%
  filter(year == 2015) %>% 
  data.frame() %>% 
  select(3:5)

write_data_sel <- bind_cols(dt95 , dt05, dt15) %>%
  arrange(reg) %>% 
  set_colnames(c("reg", "EEX", "EIEEX", "TEEX", "EEX", "EIEEX", "TEEX", "EEX", "EIEEX", "TEEX")) 

# Saving table in LaTex kable format
save_kable(
  kable(write_data_sel, 
        digits = 3,
        caption = "Emissions Embodiex in Exports (Gt of CO2 Emissions)",
        format="latex", booktabs=TRUE) %>%
    kable_styling(font_size = 8) %>%
    add_header_above(c(" " = 1,"1995" = 3, "2005" = 3, "2015" = 3)),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "tabs/Table_EX.tex"
)


### Table: Emission Embodied in Export in % of PBE-------
dt95 <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(EEE = (EEX)/(DE + EEX)*100,
         EIEEX = (EIEEX)/(DE + EEX)*100,
         TEEX = (TEEX)/(DE + EEX)*100)  %>%
  filter(year == 1995) %>% 
  data.frame() %>% 
  select(reg, EEE, EIEEX, TEEX)

dt05 <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(EEE = (EEX)/(DE + EEX)*100,
         EIEEX = (EIEEX)/(DE + EEX)*100,
         TEEX = (TEEX)/(DE + EEX)*100) %>%
  filter(year == 2005) %>% 
  data.frame() %>% 
  select(EEE, EIEEX, TEEX)

dt15 <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  left_join(EEE_knd, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(EEE = (EEX)/(DE + EEX)*100,
         EIEEX = (EIEEX)/(DE + EEX)*100,
         TEEX = (TEEX)/(DE + EEX)*100) %>%
  filter(year == 2015) %>% 
  data.frame() %>% 
  select(EEE, EIEEX, TEEX)

write_data_sel <- bind_cols(dt95 , dt05, dt15) %>%
  arrange(reg) %>% 
  set_colnames(c("reg", "EEX", "EIEEX", "TEEX", "EEX", "EIEEX", "TEEX", "EEX", "EIEEX", "TEEX"))

save_kable(
  kable(write_data_sel, 
        digits = 3,
        caption = "Emissions Embodiex in Exports (in percentage of PBE)",
        format="latex", booktabs=TRUE) %>%
    kable_styling(font_size = 8) %>%
    add_header_above(c(" " = 1,"1995" = 3, "2005" = 3, "2015" = 3)),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "tabs/Table_EX_alt.tex"
)

### Table: Production-based Emissions; Gt of CO2 ----
dt95 <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  mutate(PBA = DE + EEX) %>% 
  filter(year == 1995) %>% 
  data.frame() %>% 
  select(reg, PBA)

dt05 <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  mutate(PBA = DE + EEX) %>% 
  filter(year == 2005) %>% 
  data.frame() %>% 
  select(PBA)

dt15 <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  mutate(PBA = DE + EEX) %>% 
  filter(year == 2015) %>% 
  data.frame() %>% 
  select(PBA)

write_data_sel <- bind_cols(dt95 , dt05, dt15) %>%
  arrange(reg) %>% 
  set_colnames(c("reg", "1995", "2005", "2015")) 

save_kable(
  kable(write_data_sel, 
        digits = 3,
        caption = "Production-Based Emissions (Gt of CO2 Emissions)",
        format="latex", booktabs=TRUE) %>%
    kable_styling(font_size = 8),
  size = "\\fontsize{8pt}{10pt}\\selectfont",
  floating = TRUE, 
  latex.environments = "center",
  caption.placement = "top",
  math.style.exponents = TRUE,
  file = "tabs/Table_PBA.tex"
)

### Emissions--------------------------------
unit <- read.delim(paste0("dataraw/IOT_", 2015, "_ixi/satellite/unit.txt", sep = ""), stringsAsFactors = FALSE) %>% 
  set_colnames(c("Parameter", "Unit")) %>% 
  filter(grepl("CO2", Parameter))

print(xtable(unit, caption = "Emissions"),
      floating = TRUE, 
      latex.environments = "center",
      caption.placement = "top",
      file = "tabs/Emissions.tex"
)

### Countries-----------------------------
Countries <- read.csv(paste("dataraw/cntrs.csv", sep = ""), stringsAsFactors = FALSE)
Countries2 <- read.csv(paste("dataraw/cntrs2.csv", sep = ""), stringsAsFactors = FALSE)

cjoin <- merge(data.frame(Countries, row.names=NULL), data.frame(Countries2, row.names=NULL), 
               by = 0, all = TRUE)[-1]

cjoin[is.na(cjoin)] <- " "

print(xtable(cjoin, caption = "Country List"),
      floating = TRUE, 
      latex.environments = "center",
      include.rownames=FALSE,
      caption.placement = "top",
      scalebox = 0.8,
      file = "tabs/cjoin.tex"
)

### Industries---------------------------
industries <- Sectors %>%
  set_colnames(c("Industry", "Code Number"))

print(xtable(industries, 
             caption = "Industry List",
             align = c("l", "p{4in}", "l")),
      size = "\\fontsize{8pt}{10pt}\\selectfont",
      latex.environments = "center",
      tabular.environment = "longtable",
      caption.placement = "top",
      file = "tabs/industries.tex"
)

## Scatter Plot ----
data("pwt10.0")

pwt10.0 <- data.frame(as.list(pwt10.0), stringsAsFactors = FALSE) %>%
  as_tibble()

i <- sapply(pwt10.0 , is.factor)
pwt10.0 [i] <- lapply(pwt10.0 [i], as.character)
rm(i)

country_pwt <- pwt10.0 %>% 
  select(country, isocode) %>% 
  distinct_all() #183

country_pwt_non_exio <- read.csv("dataraw/penn_region.csv") %>% 
  rename(isocode = 2)

country_exio_non_row <- Region_Full %>% 
  select(region_full) %>% 
  rename(country = 1) %>% 
  filter(!(grepl("RoW", country))) #44

pwt_row <- pwt10.0 %>% 
  select(country, isocode, year, rgdpe, pop) %>% 
  left_join(country_pwt_non_exio, ., by = c("country", "isocode")) %>% 
  group_by(region, year) %>% 
  mutate(rgdpe = sum(rgdpe, na.rm = TRUE),
         pop = sum(pop, na.rm = TRUE)) %>% 
  ungroup() %>% 
  distinct_at(c("rgdpe", "pop"), .keep_all = TRUE) %>% 
  select(region, year, rgdpe, pop) %>% 
  mutate(GDPperCap = rgdpe/pop) %>% 
  filter(year %in% c(year))

pwt10.0$country[pwt10.0$country == "United States of America"] <- "United States"
pwt10.0$country[pwt10.0$country == "Republic of Korea"] <- "South Korea"
pwt10.0$country[pwt10.0$country == "Russian Federation"] <- "Russia"

pwt_exio <- pwt10.0 %>% 
  select(country, isocode, year, rgdpe, pop) %>% 
  left_join(country_exio_non_row, ., by = c("country")) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(GDPperCap = rgdpe/pop) %>% 
  select(country, year, rgdpe, pop, GDPperCap) %>% 
  rename(region = 1) %>% 
  as_tibble() %>% 
  filter(year %in% c(year))

dt15 <- EEE %>% 
  left_join(DE, by = c("reg", "year")) %>% 
  left_join(EEI, by = c("reg", "year")) %>% 
  left_join(EEE_Tadj, by = c("reg", "year")) %>% 
  mutate(TBEET = (TEEX - EEM) / (DE + EEX) * 100)  %>%
  filter(year %in% c(year)) %>% 
  data.frame() %>% 
  select(reg, year, TBEET)

sctr_data <- bind_rows(pwt_exio, pwt_row) %>% 
  data.frame() %>% 
  select(region, year, pop, GDPperCap) %>% 
  mutate(region = replace(region, region == "WWA", "RoW Asia and Pacific"),
         region = replace(region, region == "WWM", "RoW Middle East"),
         region = replace(region, region == "WWL", "RoW (Latin) America"),
         region = replace(region, region == "WWF", "RoW Africa"),
         region = replace(region, region == "WWE", "RoW Europe")) %>% 
  rename(reg = 1) %>% 
  left_join(dt15, by = c("reg", "year")) %>% 
  mutate(GDPperCap = GDPperCap/1000) %>% 
  filter(!is.na(TBEET))

write.csv(sctr_data, "dataprocessed/regression_data.csv")

plot_1 <- sctr_data %>% 
  filter(year == 2015) %>% 
  filter(!grepl("RoW", reg)) %>% 
  select(reg, GDPperCap, TBEET) %>%
  mutate(Model = c("Full Sample"))

plot_2 <- sctr_data %>% 
  filter(year == 2015) %>% 
  filter(!grepl("RoW", reg)) %>% 
  filter(pop >= 10) %>% 
  select(reg, GDPperCap, TBEET) %>% 
  mutate(Model = c("Large Economies"))

isocode3 <- read.delim(paste0("dataraw/countries.txt", sep = ""), stringsAsFactors = FALSE) %>% 
  rename("reg" = "Countries")

plot_facet <- bind_rows(plot_1, plot_2) %>% 
  select(reg, Model, everything()) %>% 
  left_join(isocode3, by = c("reg")) %>% 
  select(-reg) %>% 
  rename("reg" = "Code") #%>% 
  #Sfilter(!(reg %in% c("HRV")))

pdf("figs/SctPlotFacet2015.pdf", width = 16, height = 8)
print(
  ggplot(plot_facet, aes(GDPperCap, TBEET)) +
    geom_hline(yintercept=0, size = 0.4, lty = 2) +
    geom_point(shape = 16, size = 3, alpha = 0.7) +
    stat_smooth(geom = "line", method = "lm", se = FALSE, size = 0.4, color = "#00008B", alpha = 0.7) +
    theme_pander(base_family = "Times") +
    geom_text_repel(aes(label = reg), size = 6, alpha = 0.6, max.overlaps = getOption("ggrepel.max.overlaps", default = 12000)) +
    facet_wrap(~Model, scales = "free", ncol = 2) +
    scale_y_continuous() +
    theme(plot.background = element_rect(fill = "transparent"),
          legend.position = "none",
          plot.margin = margin(1.5, 1, 2, 0.5, "cm"),
          axis.title.x = element_text(size = 22, vjust = -2),
          axis.title.y = element_text(size = 22, vjust = 2),
          plot.caption = element_text(size = 8, vjust = -4),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          panel.spacing = unit(3, "lines"),
          strip.text.x = element_text(size = 26, vjust = 1.5),
          axis.line=element_line()) +
    labs(y = "TBEET",
         x ="Income per capita"))

dev.off()



# 
# # 6 Countries
# Data_plot_EEE_6 <- Data_plot %>% 
#   filter(reg %in% focus)
# 
# #Custom Potrait
# pdf("figs/Line_EEE_nosmall_6.pdf", width = 12, height = 6)
# print(
#   ggplot(Data_plot_EEE_6, aes(year, value, color = emission, shape = emission)) +
#     geom_line() +
#     geom_point() +
#     scale_color_wsj() +
#     scale_fill_solarized() +
#     scale_shape_few() +
#     facet_wrap(~reg, scales = "free_y", ncol = 3) +
#     theme_pander(base_family = "Times") +
#     theme(plot.background = element_rect(fill = "transparent"),
#           legend.background = element_rect(fill = "transparent"),
#           legend.position = "bottom",
#           legend.title = element_blank(),
#           legend.text = element_text(size = 16),
#           strip.text.x = element_text(size = 16, margin = margin(0,0,0.5,0, "cm")),
#           panel.border = element_rect(color = "white"),
#           panel.spacing.x = unit(1, "lines"),
#           panel.spacing.y = unit(2, "lines"),
#           plot.margin = margin(0, 0, 0, 0, "cm"),
#           axis.title.x = element_blank(),
#           axis.title.y = element_blank(),
#           axis.text.x = element_text(size = 14),
#           axis.text.y = element_text(size = 14)),
#   
# )
# 
# dev.off()

# #Model 1
# Mod1 <- sctr_data %>%
#   filter(year == 2015) %>% 
#   select(reg, year, GDPperCap, TBEET) %>%
#   set_colnames(c("country", "year", "GDPperCapita", "TBEET"))
# 
# Model1 <- lm(TBEET ~ GDPperCapita, data = Mod1)
# cov <- vcovHC(Model1, type = "HC")
# hr_se1 <- sqrt(diag(cov))
# #source: https://www.rdocumentation.org/packages/sandwich/versions/3.0-2/topics/vcovHC
# 
# #Model 2
# Mod2 <- sctr_data %>%
#   filter(year == 2015) %>% 
#   filter(pop >= 10) %>% 
#   select(reg, year, GDPperCap, TBEET) %>%
#   set_colnames(c("country", "year", "GDPperCapita", "TBEET"))
# 
# Model2 <- lm(TBEET ~ GDPperCapita, data = Mod2)
# cov <- vcovHC(Model2, type = "HC")
# hr_se2 <- sqrt(diag(cov))
# 
# #Model 3
# Mod3 <- sctr_data %>%
#   select(reg, year, GDPperCap, TBEET) %>%
#   set_colnames(c("country", "year", "GDPperCapita", "TBEET"))
# 
# Model3_basic <- lm(TBEET ~ GDPperCapita, data = Mod3)
# Model3_cl <- lm(TBEET ~ GDPperCapita + factor(country), data = Mod3)
# cov_cl <- vcovCL(Model3_cl, cluster = ~country)[1:2,1:2]
# cl_se3 <- sqrt(diag(cov_cl))
# #source: https://www.rdocumentation.org/packages/sandwich/versions/3.0-2/topics/vcovCL
# 
# #Model4
# Mod4 <- sctr_data %>%
#   filter(pop >= 10) %>% 
#   select(reg, year, GDPperCap, TBEET) %>%
#   set_colnames(c("country", "year", "GDPperCapita", "TBEET"))
# 
# Model4_basic <- lm(TBEET ~ GDPperCapita, data = Mod4)
# Model4_cl <- lm(TBEET ~ GDPperCapita + factor(country), data = Mod4)
# cov_cl <- vcovCL(Model4_cl, cluster = ~country)[1:2,1:2]
# cl_se4 <- sqrt(diag(cov_cl))
# 
# #standard OLS with robust standard error
# stargazer(Model1, Model2, Model3_basic, Model4_basic, 
#           se= bind_cols(hr_se1, hr_se2, cl_se3, cl_se4), 
#           title="Standard OLS with Robust Standard Error of Figure 4", 
#           column.labels = c("TBEET/PBE (percent)"), 
#           align = TRUE,
#           out = "tabs/Models_nosmall.txt")
# 
