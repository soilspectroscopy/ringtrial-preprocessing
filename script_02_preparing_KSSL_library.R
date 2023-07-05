
## Loading packages
library("tidyverse")
library("readr")
library("readxl")
library("purrr")
library("curl")
library("qs")

# Mounted disk for storing big files
mnt.dir <- "~/projects/mnt-ringtrial/"
dir.preprocessed <- paste0(mnt.dir, "preprocessed/")

## KSSL
kssl.folder <- paste0("~/projects/soilspec4gg-mac/heavydata/KSSL15ksample/")
kssl.subset <- readRDS(paste0(kssl.folder, "ctransfer_kssl_spectra.rds"))
kssl.subset

names(kssl.subset[,1:5])

kssl.subset %>%
  group_by(sample_id) %>%
  summarise(repeats = n()) %>%
  count(repeats)

kssl.subset.test <- kssl.subset %>%
  sample_n(100) %>%
  select(1:5)

kssl.potassium <- read_csv(paste0(kssl.folder, "clhs/KSSL_cLHS_15k_subset_potassium.csv"))
kssl.potassium

kssl.potassium %>%
  mutate(test = gsub("XS|XN", "", id)) %>%
  count(test, name = "repeats") %>%
  count(repeats)

kssl.potassium <- kssl.potassium %>%
  rename(sample_id = id) %>%
  mutate(sample_id = gsub("XS|XN", "", sample_id)) %>%
  mutate(sample_id = as.integer(sample_id))

kssl.potassium

subset.spectral.range <- as.character(seq(630, 4000, by = 2))

kssl.potassium.join <- kssl.potassium %>%
  filter(sample_id %in% kssl.subset$sample_id) %>%
  select(sample_id, K)

kssl.potassium.bind <- kssl.potassium %>%
  filter(!(sample_id %in% kssl.subset$sample_id)) %>%
  select(sample_id, K, all_of(subset.spectral.range))

kssl.subset.full <- kssl.subset %>%
  right_join(kssl.potassium.join, ., by = "sample_id") %>%
  bind_rows(kssl.potassium.bind)

kssl.subset.full %>%
  select(-all_of(subset.spectral.range)) %>%
  summarise_all(function(x){sum(!is.na(x))})

# Renaming
kssl.subset <- kssl.subset.full %>%
  rename(clay_perc = clay,
         pH_H20 = pH,
         carbon_tot_perc = TC,
         carbon_org_perc = OC,
         potassium_cmolkg = K)

kssl.subset

new.spectra.range <- as.character(seq(650, 4000, by = 2))
important.columns <- c("sample_id", "clay_perc", "pH_H20",
                       "carbon_tot_perc", "carbon_org_perc",
                       "potassium_cmolkg")

kssl.subset <- kssl.subset %>%
  select(all_of(important.columns),
         all_of(new.spectra.range))
  
kssl.subset

write_csv(kssl.subset, paste0(dir.preprocessed, "KSSL_soilMIRspectra_raw.csv"))

## BOC

kssl.subset

kssl.subset.SGS <- kssl.subset %>%
  select(-all_of(important.columns)) %>%
  as.matrix() %>%
  prospectr::savitzkyGolay(X = ., m = 0, p = 2, w = 11, delta.wav = 2) %>%
  bind_cols({kssl.subset %>%
      select(all_of(important.columns))}, .)

kssl.subset.SGS

kssl.subset.baseline <- kssl.subset.SGS %>%
  select(sample_id, any_of(new.spectra.range)) %>%
  rowwise(sample_id) %>%
  summarise(baseline = min(c_across(everything()))) %>%
  left_join(kssl.subset.SGS, by = "sample_id") %>%
  ungroup()

kssl.subset.baseline

kssl.subset.BC <- kssl.subset.baseline %>%
  pivot_longer(-all_of(c(important.columns, "baseline")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  mutate(absorbance = absorbance-baseline) %>%
  pivot_wider(id_cols = all_of(c(important.columns, "baseline")),
              names_from = "wavenumber", values_from = "absorbance") %>%
  select(-baseline) %>%
  mutate_if(is.numeric, round, digits = 5)

kssl.subset.BC

write_csv(kssl.subset.BC, paste0(dir.preprocessed, "KSSL_soilMIRspectra_BOC.csv"))

## SG First Derivative

kssl.subset

kssl.subset.1stDer <- kssl.subset %>%
  select(-all_of(important.columns)) %>%
  as.matrix() %>%
  prospectr::savitzkyGolay(X = ., m = 1, p = 2, w = 11, delta.wav = 2) %>%
  bind_cols({kssl.subset %>%
      select(all_of(important.columns))}, .) %>%
  mutate_if(is.numeric, round, digits = 8)

kssl.subset.1stDer

write_csv(kssl.subset.1stDer, paste0(dir.preprocessed, "KSSL_soilMIRspectra_SG1stDer.csv"))

## Standard Normal Variate

kssl.subset.SNV <- kssl.subset %>%
  select(-all_of(important.columns)) %>%
  as.matrix() %>%
  prospectr::savitzkyGolay(X = ., m = 0, p = 2, w = 11, delta.wav = 2) %>%
  prospectr::standardNormalVariate(X = .) %>%
  bind_cols({kssl.subset %>%
      select(all_of(important.columns))}, .) %>%
  mutate_if(is.numeric, round, digits = 5)

kssl.subset.SNV

write_csv(kssl.subset.SNV, paste0(dir.preprocessed, "KSSL_soilMIRspectra_SNV.csv"))

## SNV + SG1stDer

kssl.subset.SNVplusSG1stDer <- kssl.subset %>%
  select(-all_of(important.columns)) %>%
  as.matrix() %>%
  prospectr::standardNormalVariate(X = .) %>%
  prospectr::savitzkyGolay(X = ., m = 1, p = 2, w = 11, delta.wav = 2) %>%
  bind_cols({kssl.subset %>%
      select(all_of(important.columns))}, .) %>%
  mutate_if(is.numeric, round, digits = 8)

kssl.subset.SNVplusSG1stDer

write_csv(kssl.subset.SNVplusSG1stDer, paste0(dir.preprocessed, "KSSL_soilMIRspectra_SNVplusSG1stDer.csv"))
