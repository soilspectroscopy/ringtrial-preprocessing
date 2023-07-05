
## Loading packages
library("tidyverse")

## Creating input/output dirs
if(!dir.exists("outputs")){dir.create("outputs")}

## Mounted disck for storing big files
mnt.dir <- "~/projects/mnt-ringtrial/"

## Creating raw_files dir
if(!dir.exists(paste0(mnt.dir, "raw_files"))){dir.create(paste0(mnt.dir, "raw_files"))}

## Creating gdrive_downloads dir
if(!dir.exists(paste0(mnt.dir, "gdrive_downloads"))){dir.create(paste0(mnt.dir, "gdrive_downloads"))}

## Creating std_files dir
if(!dir.exists(paste0(mnt.dir, "std_files"))){dir.create(paste0(mnt.dir, "std_files"))}

## Creating preprocessed dir
if(!dir.exists(paste0(mnt.dir, "preprocessed"))){dir.create(paste0(mnt.dir, "preprocessed"))}
