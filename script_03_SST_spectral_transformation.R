
library("tidyverse")
library("tidymodels")
library("lubridate")
library("prospectr")
library("cowplot")
library("qs")

# Mounted disk for storing big files
mnt.dir <- "~/projects/mnt-ringtrial/"
dir.preprocessed <- paste0(mnt.dir, "preprocessed/")

## Loading data for reference
spec.data <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNV.csv"))

spec.data %>%
  group_by(organization) %>%
  summarise(count = n())

####################################
##### Kennard-Stone subsetting #####
####################################

spec.data.kssl <- spec.data %>%
  filter(organization == "KSSL") %>%
  select(-organization, -sample_id) %>%
  as.matrix()

set.seed(1993)
kenStone.selection <- kenStone(X = spec.data.kssl, k = 50,
                               metric = "mahal", pc = 0.9999,
                               .center = T, .scale = T)

plot(kenStone.selection$pc[, 1:2], xlab = "PC1", ylab = "PC2")
points(kenStone.selection$pc[kenStone.selection$model, 1:2], pch = 19, col = 2)

selected.rows <- kenStone.selection$model

sample.ids <- spec.data %>%
  distinct(sample_id) %>%
  pull(sample_id)

sst.ids <- sample.ids[selected.rows]
sort(sst.ids)

test.ids <- sample.ids[-selected.rows]
sort(test.ids)

qsave(sst.ids, "outputs/RT_sst_ids.qs")
qsave(test.ids, "outputs/RT_test_ids.qs")

## Checking PC distribution of RT SST and test onto KSSL PC space

## PCA projection

kssl.library <- read_csv(paste0(dir.preprocessed, "KSSL_soilMIRspectra_SNV.csv"))
head(kssl.library[,1:10])

spectra.range <- as.character(seq(660, 3990, by = 2))

kssl.library <- kssl.library %>%
  select(sample_id, all_of(spectra.range)) %>%
  mutate(subset = "KSSL_library", .before = 1)

head(kssl.library[,1:10])

kssl.ringtrial <- spec.data %>%
  filter(organization == "KSSL")

kssl.ringtrial.sst <- kssl.ringtrial %>%
  filter(sample_id %in% sst.ids) %>%
  select(-organization) %>%
  mutate(subset = "RT_SST", .before = 1)

kssl.ringtrial.test <- kssl.ringtrial %>%
  filter(!(sample_id %in% sst.ids)) %>%
  select(-organization) %>%
  mutate(subset = "RT_test", .before = 1)

# Reference PC space

pca.model <- kssl.library %>%
  recipe() %>%
  update_role(everything()) %>%
  update_role(all_of(c("sample_id", "subset")), new_role = "id") %>%
  step_normalize(all_predictors(), id = "normalization") %>% # Center and scale spectra
  step_pca(all_predictors(), num_comp = 4, id = "pca") %>% # 4 comps just for visualization purpose
  prep()

pca.scores.train <- juice(pca.model) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .))))

p.scores <- pca.scores.train %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(aes(color = "KSSL library"), size = 0.5, alpha = 0.25, color = "black") +
  labs(x = "PC1", y = "PC2", color = "",
       title = "KSSL library") +
  theme_light() +
  theme(legend.position = "bottom"); p.scores

# Projection of RT SST and test

pca.scores.sst <- bake(pca.model, new_data = kssl.ringtrial.sst) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .)))) %>%
  select(-sample_id, -subset) %>%
  bind_cols({kssl.ringtrial.sst %>%
      select(sample_id, subset)}, .)

p.scores.sst <- p.scores +
  geom_point(data = pca.scores.sst,
             aes(x = PC1, y = PC2, color = "RT_SST"),
             size = 1, alpha = 1) +
  scale_color_manual(values = c("KSSL library"="black", "RT_SST" = "red")) +
  labs(title = "Projection of RT SST samples - SNV preprocessed"); p.scores.sst

pca.scores.test <- bake(pca.model, new_data = kssl.ringtrial.test) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .)))) %>%
  select(-sample_id, -subset) %>%
  bind_cols({kssl.ringtrial.test %>%
      select(sample_id, subset)}, .)

p.scores.test <- p.scores +
  geom_point(data = pca.scores.test,
             aes(x = PC1, y = PC2, color = "RT_test"),
             size = 1, alpha = 1) +
  scale_color_manual(values = c("KSSL library"="black", "RT_test" = "green")) +
  labs(title = "Projection of RT test samples - SNV preprocessed"); p.scores.test

# Full plot

pca.variance <- tidy(pca.model, id = "pca", type = "variance")

pca.variance %>%
  distinct(terms)

pca.percents <- pca.variance %>%
  filter(terms == "percent variance") %>%
  filter(component <= 2) %>%
  mutate(value = round(value, 2))

p.scores.all <- p.scores +
  geom_point(data = pca.scores.sst,
             aes(x = PC1, y = PC2, color = "RT SST (n=50)"),
             size = 1, alpha = 1) +
  geom_point(data = pca.scores.test,
             aes(x = PC1, y = PC2, color = "RT test (n=20)"),
             size = 1, alpha = 1) +
  scale_color_manual(values = c("KSSL library"="black", "RT SST (n=50)"="red", "RT test (n=20)" = "green")) +
  # labs(title = "Projection of ring trial subsets",
  labs(title = "",
       x = paste0("PC1 (", pca.percents[[1, "value"]], "%)"),
       y = paste0("PC2 (", pca.percents[[2, "value"]], "%)")); p.scores.all

ggsave("outputs/plot_pca_projection_RT_SSTandTest_onto_KSSLlibrary.png",
       p.scores.all, dpi = 300, width = 8, height = 6,
       units = "in", scale = 1)


###################################################
##### Spectral Subspace Transformation - SST #####
###################################################

sst.ids <- qread("outputs/RT_sst_ids.qs")
test.ids <- qread("outputs/RT_test_ids.qs")

# Spectral subset

spectral.subset <- spec.data %>%
  mutate(ct_subset = ifelse(sample_id %in% sst.ids, "SST", "test")) %>%
  relocate(ct_subset, .after = sample_id)

spectral.subset

# Spectral Subspace Transformation - SST

testset.kssl <- spectral.subset %>%
  filter(organization == "KSSL") %>%
  filter(ct_subset == "test")

testsets.ct <- spectral.subset %>%
  filter(!(organization == "KSSL")) %>%
  filter(ct_subset == "test")

ct.orgs <- testsets.ct %>%
  distinct(organization) %>%
  pull(organization)

ct.list <- list()

for(k in 1:length(ct.orgs)) {
  
  korganization <- ct.orgs[k]
  
  primary <- spectral.subset %>%
    filter(organization == "KSSL") %>%
    filter(ct_subset == "SST") %>%
    arrange(sample_id)
  
  secondary <- spectral.subset %>%
    filter(organization == korganization) %>%
    filter(ct_subset == "SST") %>%
    arrange(sample_id)
  
  testset <- testsets.ct %>%
    filter(organization == korganization) %>%
    filter(ct_subset == "test")
  
  ## SST
  
  id.vars <- c("organization", "sample_id", "ct_subset")
  cumvar <- 0.9999
  
  xtest <- testset %>%
    select(-all_of(id.vars)) %>%
    as.matrix()
  
  spec1 <- primary %>%
    select(-all_of(id.vars)) %>%
    as.matrix()
  
  spec2 <- secondary %>%
    select(-all_of(id.vars)) %>%
    as.matrix()
  
  xcomb <- cbind(spec1, spec2)
  xcomb.svd <- svd(xcomb)
  
  d <- xcomb.svd$d
  cumvar.xcomb <- cumsum(d^2)/sum(d^2)
  ncomps <- sum(cumvar.xcomb <= cumvar)
  
  P1T <- t(xcomb.svd$v[seq(1,ncol(spec1),1),1:ncomps])
  
  P2T <- t(xcomb.svd$v[seq(ncol(spec1)+1,ncol(t(xcomb.svd$v)),1),1:ncomps])
  
  stdmat <- diag(ncol(spec1)) + MASS::ginv(P2T) %*% (P1T - P2T)
  
  xtrans <- xtest %*% stdmat
  xtrans <- as_tibble(as.data.frame(xtrans))
  names(xtrans) <- colnames(xtest)
  
  test.trans <- bind_cols({testset %>%
      select(all_of(id.vars))}, xtrans)
  
  # comparison <- bind_rows({testset %>% mutate(ct_subset = "original")}, test.trans)
  # 
  # test.id <- "RT_12"
  # 
  # comparison %>%
  #   filter(sample_id == test.id) %>%
  #   pivot_longer(-all_of(id.vars), names_to = "wavenumber", values_to = "absorbance") %>%
  #   ggplot(aes(x = as.numeric(wavenumber), y = absorbance)) +
  #   labs(x = bquote("Wavenumber"~cm^-1), y = "Absorbance", color = "") +
  #   scale_x_continuous(breaks = c(600, 1200, 1800, 2400, 3000, 3600, 4000)) +
  #   geom_line(aes(color = ct_subset, group = ct_subset)) +
  #   theme_light() + theme(legend.position = "bottom")
  
  ct.list[[k]] <- test.trans
  
  cat(paste0("Run ", k, "/", length(ct.orgs), "\n"))
  
}

spectral.sst <- Reduce(bind_rows, ct.list) %>%
  mutate(ct_subset = ifelse(ct_subset == "test", "afterSST", NA))

before.sst <- spectral.subset %>%
  filter(ct_subset == "test") %>%
  mutate(ct_subset = ifelse(ct_subset == "test", "beforeSST", NA))
  
full.sst <- bind_rows(spectral.sst, before.sst) %>%
  arrange(organization, sample_id)

## Preparation and export

id.vars <- c("organization", "sample_id", "ct_subset")

rt.mirdata.binded <- full.sst %>%
  mutate_if(is.numeric, round, digits = 5)

write_csv(rt.mirdata.binded, paste0(dir.preprocessed, "RT_STD_allMIRspectra_SST.csv"))
