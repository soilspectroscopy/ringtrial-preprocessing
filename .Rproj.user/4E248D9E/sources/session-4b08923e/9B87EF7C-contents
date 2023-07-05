
## Loading packages

library("dplyr")
library("readr")
library("readxl")
library("tidyr")
library("purrr")
library("prospectr")

# Mounted disk for storing big files
mnt.dir <- "~/projects/mnt-ringtrial/"
dir.gdrivedownloads <- paste0(mnt.dir, "gdrive_downloads/")
dir.stdfiles <- paste0(mnt.dir, "std_files/")
dir.preprocessed <- paste0(mnt.dir, "preprocessed/")

# Clean old files
do.call(file.remove, list(list.files(dir.preprocessed, full.names = TRUE, pattern = "RT_STD_allMIRspectra")))

## Reading wet chem data

wetchem <- read_xlsx(paste0(mnt.dir, "RT_data.xlsx"), sheet = 2)

wetchem <- wetchem %>%
  rename(sample_id = simple_id) %>%
  mutate(sample_iud = gsub("-", "", sample_iud))

wetchem

## Listing and reading formatted spectra

listed.files <- list.files(paste0(mnt.dir, "std_files"), full.names = T)
listed.files

# Defining columns types across datasets

coltypes <- cols(organization = col_character(),
                 sample_id = col_character(),
                 sample_iud = col_character(),
                 prep = col_character(),
                 replicate = col_character())

# Reading

all.files <- list()

for(i in 1:length(listed.files)) {
  all.files[[i]] <- read_csv(listed.files[i], col_types = coltypes)
}

lapply(all.files, nrow)

rt.mirdata <- Reduce(bind_rows, all.files) %>%
  select(organization, sample_id, sample_iud, prep, replicate, everything()) %>%
  mutate(sample_iud = gsub("-", "", sample_iud))

ncol(rt.mirdata)

# ## Checking wetchem data
# 
# # Available sample_id
# wetchem %>%
#   pull(sample_id)
# 
# # Length sample_id
# wetchem %>%
#   pull(sample_id) %>%
#   length()
# 
# # Unique sample_id
# wetchem %>%
#   distinct(sample_id) %>%
#   nrow()
# 
# # Available sample_iud
# wetchem %>%
#   pull(sample_iud)
# 
# # Length sample_iud
# wetchem %>%
#   pull(sample_iud) %>%
#   length()
# 
# # Unique sample_id
# wetchem %>%
#   distinct(sample_iud) %>%
#   nrow()

# # Checking KSSL
# 
# rt.mirdata %>%
#   distinct(organization)
# 
# rt.mirdata %>%
#   filter(organization == "KSSL")
#   
# rt.mirdata %>%
#   filter(organization == "KSSL") %>%
#   group_by(prep) %>%
#   summarize(count = n())
# 
# kssl.xn <- rt.mirdata %>%
#   filter(organization == "KSSL") %>%
#   filter(prep == "XN") %>%
#   pull(sample_iud); kssl.xn
# 
# kssl.xs <- rt.mirdata %>%
#   filter(organization == "KSSL") %>%
#   filter(prep == "XS") %>%
#   pull(sample_iud); kssl.xs
# 
# kssl.xs %in% kssl.xn

## Reference ids

reference.sample.ids <- wetchem %>%
  pull(sample_id)

reference.sample.iuds <- wetchem %>%
  pull(sample_iud)

## Checking sample_id consistency

check.sample.id <- rt.mirdata %>%
  select(organization, sample_id) %>%
  group_by(organization, sample_id) %>%
  summarise_all(first) %>%
  ungroup() %>%
  nest(data = sample_id) %>%
  mutate(check_sample_id = unlist(map(.x = data,
                                      .f = ~{
                                        length(which(reference.sample.ids %in% unique(.x$sample_id)))
                                      }))) %>%
  select(organization, check_sample_id)

## Checking sample_iud consistency

check.sample.iud <- rt.mirdata %>%
  select(organization, sample_iud) %>%
  group_by(organization, sample_iud) %>%
  summarise_all(first) %>%
  ungroup() %>%
  nest(data = sample_iud) %>%
  mutate(check_sample_iud = unlist(map(.x = data,
                                      .f = ~{
                                        length(which(reference.sample.iuds %in% unique(.x$sample_iud)))
                                      }))) %>%
  select(organization, check_sample_iud)

# Exporting ID checks

check.samples <- left_join(check.sample.id, check.sample.iud, by = "organization")
check.samples

check.samples <- check.samples %>%
  mutate(measured_samples = max(check_sample_id, check_sample_iud))
check.samples

write.table(check.samples, paste0(mnt.dir, "RT_check_measured_ids.csv"),
            col.names = T, row.names = F, sep = ",", dec = ".")

## Transforming sample_iud to sample_id

rt.mirdata.ok <- rt.mirdata %>%
  filter(!is.na(sample_id))

rt.mirdata.join <- rt.mirdata %>%
  filter(!is.na(sample_iud)) %>%
  select(-prep, -sample_id)

wetchem.sample.id <- wetchem %>%
  select(sample_iud, sample_id)

rt.mirdata.id.transformed <- rt.mirdata.join %>%
  left_join(wetchem.sample.id, by = "sample_iud") %>%
  filter(!is.na(sample_id)) %>%
  select(-sample_iud) %>%
  select(organization, sample_id, replicate, everything())

rt.mirdata.binded <- rt.mirdata.ok %>%
  bind_rows(rt.mirdata.id.transformed) %>%
  select(-sample_iud, -prep)

rt.mirdata.binded
names(rt.mirdata.binded)[1:6]
names(rt.mirdata.binded)[(ncol(rt.mirdata.binded)-6):(ncol(rt.mirdata.binded))]
ncol(rt.mirdata.binded)

rt.mirdata.binded.avg <- rt.mirdata.binded %>%
  select(-replicate) %>%
  group_by(organization, sample_id) %>%
  summarize_all(mean) %>%
  ungroup() %>%
  mutate_if(is.numeric, round, digits = 5)

rt.mirdata.binded <- rt.mirdata.binded.avg

rt.mirdata.binded %>%
  group_by(organization) %>%
  summarise(n = n())

write.table(rt.mirdata.binded, paste0(dir.preprocessed, "RT_STD_allMIRspectra_raw.csv"))

## Preprocessings

# rt.mirdata.binded <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_raw.csv"))

## BOC

rt.mirdata.binded

rt.mirdata.binded.SGS <- rt.mirdata.binded %>%
  select(-organization, -sample_id) %>%
  as.matrix() %>%
  prospectr::savitzkyGolay(X = ., m = 0, p = 2, w = 11, delta.wav = 2) %>%
  bind_cols({rt.mirdata.binded %>%
      select(organization, sample_id)}, .)

rt.mirdata.binded.SGS

rt.mirdata.binded.baseline <- rt.mirdata.binded.SGS %>%
  rowwise(organization, sample_id) %>%
  summarise(baseline = min(c_across(everything()))) %>%
  left_join(rt.mirdata.binded.SGS, by = c("organization", "sample_id")) %>%
  ungroup()

rt.mirdata.binded.baseline

rt.mirdata.binded.BC <- rt.mirdata.binded.baseline %>%
  pivot_longer(-all_of(c("organization", "sample_id", "baseline")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  mutate(absorbance = absorbance-baseline) %>%
  pivot_wider(id_cols = all_of(c("organization", "sample_id", "baseline")),
              names_from = "wavenumber", values_from = "absorbance") %>%
  select(-baseline) %>%
  mutate_if(is.numeric, round, digits = 5)

rt.mirdata.binded.BC

rt.mirdata.binded.BC %>%
  group_by(organization) %>%
  summarise(n = n())

write_csv(rt.mirdata.binded.BC, paste0(dir.preprocessed, "RT_STD_allMIRspectra_BOC.csv"))

## SG First Derivative

rt.mirdata.binded

rt.mirdata.binded.1stDer <- rt.mirdata.binded %>%
  select(-organization, -sample_id) %>%
  as.matrix() %>%
  prospectr::savitzkyGolay(X = ., m = 1, p = 2, w = 11, delta.wav = 2) %>%
  bind_cols({rt.mirdata.binded %>%
      select(organization, sample_id)}, .) %>%
  mutate_if(is.numeric, round, digits = 8)

rt.mirdata.binded.1stDer

rt.mirdata.binded.1stDer %>%
  group_by(organization) %>%
  summarise(n = n())

write_csv(rt.mirdata.binded.1stDer, paste0(dir.preprocessed, "RT_STD_allMIRspectra_SG1stDer.csv"))

## Standard Normal Variate

rt.mirdata.binded.SNV <- rt.mirdata.binded %>%
  select(-organization, -sample_id) %>%
  as.matrix() %>%
  prospectr::savitzkyGolay(X = ., m = 0, p = 2, w = 11, delta.wav = 2) %>%
  prospectr::standardNormalVariate(X = .) %>%
  bind_cols({rt.mirdata.binded %>%
      select(organization, sample_id)}, .) %>%
  mutate_if(is.numeric, round, digits = 5)

rt.mirdata.binded.SNV

rt.mirdata.binded.SNV %>%
  group_by(organization) %>%
  summarise(n = n())

write_csv(rt.mirdata.binded.SNV, paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNV.csv"))

## SNV + SG1stDer

rt.mirdata.binded.SNVplusSG1stDer <- rt.mirdata.binded %>%
  select(-organization, -sample_id) %>%
  as.matrix() %>%
  prospectr::standardNormalVariate(X = .) %>%
  prospectr::savitzkyGolay(X = ., m = 1, p = 2, w = 11, delta.wav = 2) %>%
  bind_cols({rt.mirdata.binded %>%
      select(organization, sample_id)}, .) %>%
  mutate_if(is.numeric, round, digits = 8)

rt.mirdata.binded.SNVplusSG1stDer

rt.mirdata.binded.SNVplusSG1stDer %>%
  group_by(organization) %>%
  summarise(n = n())

write_csv(rt.mirdata.binded.SNVplusSG1stDer, paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNVplusSG1stDer.csv"))

## Preparing wetchem

names(wetchem)

wetchem.selection <- wetchem %>%
  rename(clay_perc = Clay,
         pH_H20 = `pH, 1:1 Soil-Water Suspension`,
         carbon_tot_perc = `Carbon, Total`,
         carbon_org_perc = `Estimated Organic Carbon`,
         potassium_cmolkg = `Potassium, NH4OAc Extractable, 2M KCl displacement`,
         phosporus_mgkg = `Phosphorus, Mehlich3 Extractable`) %>%
  select(sample_id, clay_perc, pH_H20, carbon_tot_perc, carbon_org_perc, potassium_cmolkg, phosporus_mgkg) %>%
  mutate(source = ifelse(sample_id %in% c(paste0("RT_", seq(61, 70, 1))), "NAPT", "KSSL")) %>%
  relocate(source, .after = sample_id) %>%
  mutate_if(is.numeric, round, digits = 5)

wetchem.selection

write_csv(wetchem.selection, paste0(dir.preprocessed, "RT_wetchem_soildata.csv"))
