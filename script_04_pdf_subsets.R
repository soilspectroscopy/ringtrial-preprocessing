
library("tidyverse")
library("tidymodels")
library("lubridate")
library("prospectr")
library("cowplot")
library("qs")

# Mounted disk for storing big files
mnt.dir <- "~/projects/mnt-ringtrial/"
dir.preprocessed <- paste0(mnt.dir, "preprocessed/")

########################################
##### Soil Properties distribution #####
########################################

kssl.library.soil <- read_csv(paste0(dir.preprocessed, "KSSL_soilMIRspectra_SNV.csv"),
                              col_select = 1:10)
head(kssl.library.soil)

rt.wetchem <- read_csv(paste0(dir.preprocessed, "RT_wetchem_soildata.csv"))
rt.wetchem

sst.ids <- qread("outputs/RT_sst_ids.qs")
test.ids <- qread("outputs/RT_test_ids.qs")

# Panel carbon_org_perc

data.carbon_org_perc <- kssl.library.soil %>%
  mutate(sample_id = as.character(sample_id)) %>%
  select(sample_id, carbon_org_perc) %>%
  filter(!is.na(carbon_org_perc)) %>%
  mutate(subset = "KSSL library") %>%
  bind_rows({
    rt.wetchem %>%
      select(sample_id, carbon_org_perc) %>%
      mutate(subset = ifelse(sample_id %in% sst.ids, "RT SST", "RT test"))
  }, .)

# data.carbon_org_perc %>%
#   mutate(carbon_org_perc = ifelse(carbon_org_perc <= 0, 0.01, carbon_org_perc)) %>%
#   mutate(carbon_org_perc = log(carbon_org_perc)) %>%
#   ggplot(aes(x = carbon_org_perc, fill = subset, color = subset)) +
#   geom_density(alpha = 0.15) +
#   labs(y = "Density", x = "", fill = "", color = "") +
#   theme_light()

p.carbon_org_perc <- data.carbon_org_perc %>%
  mutate(carbon_org_perc = ifelse(carbon_org_perc <= 0, 0.01, carbon_org_perc)) %>%
  mutate(carbon_org_perc = log(carbon_org_perc)) %>%
  ggplot(aes(x = carbon_org_perc, fill = subset, color = subset)) +
  geom_density(alpha = 0.15, show.legend = F) +
  labs(y = "", x = expression(OC~('%')), fill = "", color = "") +
  scale_x_continuous(breaks = c(-5, -2.5, 0, 2.5), labels = round(exp(c(-5, -2.5, 0, 2.5)), 2)) +
  # scale_fill_manual(values = c("darkred", "darkblue")) +
  # scale_color_manual(values = c("darkred", "darkblue")) +
  annotate("text", label = "A)", x = -5.25, y = 0.50, size = 4, colour = "black") +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Panel clay_perc

data.clay_perc <- kssl.library.soil %>%
  mutate(sample_id = as.character(sample_id)) %>%
  select(sample_id, clay_perc) %>%
  filter(!is.na(clay_perc)) %>%
  mutate(subset = "KSSL library") %>%
  bind_rows({
    rt.wetchem %>%
      select(sample_id, clay_perc) %>%
      mutate(subset = ifelse(sample_id %in% sst.ids, "RT SST", "RT test"))
  }, .)

# data.clay_perc %>%
#   mutate(clay_perc = ifelse(clay_perc <= 0, 0.01, clay_perc)) %>%
#   ggplot(aes(x = clay_perc, fill = subset, color = subset)) +
#   geom_density(alpha = 0.15) +
#   labs(y = "Density", x = "", fill = "", color = "") +
#   theme_light()

p.clay_perc <- data.clay_perc %>%
  mutate(clay_perc = ifelse(clay_perc <= 0, 0.01, clay_perc)) %>%
  ggplot(aes(x = clay_perc, fill = subset, color = subset)) +
  geom_density(alpha = 0.15, show.legend = T) +
  labs(y = "", x = expression(Clay~('%')), fill = "", color = "") +
  # scale_fill_manual(values = c("darkred", "darkblue")) +
  # scale_color_manual(values = c("darkred", "darkblue")) +
  annotate("text", label = "B)", x = 0, y = 0.06, size = 4, colour = "black") +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = c(0.75, 0.75))

# Panel pH_H2O

data.pH_H2O <- kssl.library.soil %>%
  mutate(sample_id = as.character(sample_id)) %>%
  rename(pH_H2O = pH_H20) %>%
  select(sample_id, pH_H2O) %>%
  filter(!is.na(pH_H2O)) %>%
  mutate(subset = "KSSL library") %>%
  bind_rows({
    rt.wetchem %>%
      rename(pH_H2O = pH_H20) %>%
      select(sample_id, pH_H2O) %>%
      mutate(subset = ifelse(sample_id %in% sst.ids, "RT SST", "RT test"))
  }, .)

# data.pH_H2O %>%
#   mutate(pH_H2O = ifelse(pH_H2O <= 0, 0.01, pH_H2O)) %>%
#   ggplot(aes(x = pH_H2O, fill = subset, color = subset)) +
#   geom_density(alpha = 0.15) +
#   labs(y = "Density", x = "", fill = "", color = "") +
#   theme_light()

p.pH_H2O <- data.pH_H2O %>%
  mutate(pH_H2O = ifelse(pH_H2O <= 0, 0.01, pH_H2O)) %>%
  ggplot(aes(x = pH_H2O, fill = subset, color = subset)) +
  geom_density(alpha = 0.15, show.legend = F) +
  labs(y = "", x = expression(pH~(H[2]*O)), fill = "", color = "") +
  # scale_fill_manual(values = c("darkred", "darkblue")) +
  # scale_color_manual(values = c("darkred", "darkblue")) +
  annotate("text", label = "C)", x = 2, y = 0.50, size = 4, colour = "black") +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Panel potassium_cmolkg

data.potassium_cmolkg <- kssl.library.soil %>%
  mutate(sample_id = as.character(sample_id)) %>%
  select(sample_id, potassium_cmolkg) %>%
  filter(!is.na(potassium_cmolkg)) %>%
  mutate(subset = "KSSL library") %>%
  bind_rows({
    rt.wetchem %>%
      select(sample_id, potassium_cmolkg) %>%
      mutate(subset = ifelse(sample_id %in% sst.ids, "RT SST", "RT test"))
  }, .)

# data.potassium_cmolkg %>%
#   mutate(potassium_cmolkg = ifelse(potassium_cmolkg <= 0, 0.01, potassium_cmolkg)) %>%
#   mutate(potassium_cmolkg = log(potassium_cmolkg)) %>%
#   ggplot(aes(x = potassium_cmolkg, fill = subset, color = subset)) +
#   geom_density(alpha = 0.15) +
#   labs(y = "Density", x = "", fill = "", color = "") +
#   theme_light()

p.potassium_cmolkg <- data.potassium_cmolkg %>%
  mutate(potassium_cmolkg = ifelse(potassium_cmolkg <= 0, 0.01, potassium_cmolkg)) %>%
  mutate(potassium_cmolkg = log(potassium_cmolkg)) %>%
  ggplot(aes(x = potassium_cmolkg, fill = subset, color = subset)) +
  geom_density(alpha = 0.15, show.legend = F) +
  labs(y = "", x = expression(K~(cmol[c]~kg^-1)), fill = "", color = "") +
  scale_x_continuous(breaks = c(-4, -2, 0, 2), labels = round(exp(c(-4, -2, 0, 2)), 2)) +
  # scale_fill_manual(values = c("darkred", "darkblue")) +
  # scale_color_manual(values = c("darkred", "darkblue")) +
  annotate("text", label = "D)", x = -5.5, y = 0.90, size = 4, colour = "black") +
  theme_light() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Panel combination

p.final.ver <- plot_grid(p.carbon_org_perc, p.clay_perc, p.pH_H2O, p.potassium_cmolkg,
                         labels = c('', '', '', ''),
                         ncol = 2, align = 'v', axis = 'l')

ggsave("outputs/plot_soil_properties_distribution.png", p.final.ver, dpi = 300,
       width = 8, height = 6, units = "in", scale = 1)
