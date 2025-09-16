# =====================================================================
# adding_databases.R
# ---------------------------------------------------------------------
# Purpose: Curate and validate multiple compiled trait datasets for a
# traits.build project, fill safe defaults for compilations,
# run basic checks, and produce a gap report before building.
# Author: Charlie Hart
# Date: 2025-09-15
# =====================================================================

setwd("C:/Users/charliehart/The University of Melbourne/Billy Geary - fire_traits/CH_Step_4/traits.build_CH")

#remotes::install_github("traitecoevo/traits.build", quick = TRUE)
pacman::p_load(traits.build, austraits, ggplot2, rcrossref)

# adding new data to an existing database
# from data.csv and metadata.yml in subfolder of 'data' 
# named according to author_year

# 1) Soria_2021
metadata_create_template("Soria_2021") # ID of paper folder
# add data doi: note there could be multiple for various traits
metadata_add_source_doi(
  dataset_id = "Soria_2021", # ID of paper folder
  doi = "10.1242/jeb.245657")
# adding traits from paper to database
metadata_add_traits(dataset_id = "Soria_2021") # ID of paper folder


metadata_check_custom_R_code("Soria_2021")

# 2) Wilman_2014
metadata_create_template("Wilman_2014") # ID of paper folder
# add data doi: note there could be multiple for various traits
#metadata_add_source_doi(
#  dataset_id = "Wilman_2014", # ID of paper folder
#  doi = "10.1890/13-1917.1")
# adding traits from paper to database
metadata_add_traits(dataset_id = 'Wilman_2014') # ID of paper folder

metadata_check_custom_R_code("Wilman_2014")

# 3) Myhrvold_2015
metadata_create_template("Myhrvold_2015") # ID of paper folder
# add data doi: note there could be multiple for various traits
metadata_add_source_doi(
  dataset_id = "Myhrvold_2015", # ID of paper folder
  doi = "10.1890/15-0846.1")
# adding traits from paper to database
metadata_add_traits(dataset_id = "Myhrvold_2015") # ID of paper folder

metadata_check_custom_R_code("Myhrvold_2015")

# 4) Tobias_2022
metadata_create_template("Tobias_2022") # ID of paper folder
# add data doi: note there could be multiple for various traits
metadata_add_source_doi(
  dataset_id = "Tobias_2022", # ID of paper folder
  doi = "10.1111/ele.13898")
# adding traits from paper to database
metadata_add_traits(dataset_id = "Tobias_2022") # ID of paper folder

metadata_check_custom_R_code("Tobias_2022")

# 5) Oliveira_2017
metadata_create_template("Oliveira_2017") # ID of paper folder
# add data doi: note there could be multiple for various traits
metadata_add_source_doi(
  dataset_id = "Oliveira_2017", # ID of paper folder
  doi = "10.1038/sdata.2017.123")
# adding traits from paper to database
metadata_add_traits(dataset_id = "Oliveira_2017") # ID of paper folder

metadata_check_custom_R_code("Oliveira_2017")

# 6) SMP_amph
metadata_create_template("SMP_amph") # ID of paper folder
# add data doi: note there could be multiple for various traits
metadata_add_source_doi(
  dataset_id = "SMP_amph", # ID of paper folder
  doi = "")
# adding traits from paper to database
metadata_add_traits(dataset_id = "SMP_amph") # ID of paper folder

metadata_check_custom_R_code("SMP_amph")

# 7) SMP_mamm
metadata_create_template("SMP_mamm") # ID of paper folder
# add data doi: note there could be multiple for various traits
metadata_add_source_doi(
  dataset_id = "SMP_mamm", # ID of paper folder
  doi = "")
# adding traits from paper to database
metadata_add_traits(dataset_id = "SMP_mamm") # ID of paper folder

metadata_check_custom_R_code("SMP_mamm")

# 8) SMP_birds
metadata_create_template("SMP_birds") # ID of paper folder
# add data doi: note there could be multiple for various traits
metadata_add_source_doi(
  dataset_id = "SMP_birds", # ID of paper folder
  doi = "")
# adding traits from paper to database
metadata_add_traits(dataset_id = "SMP_birds") # ID of paper folder

metadata_check_custom_R_code("SMP_birds")

# 9) SMP_reptiles
metadata_create_template("SMP_reptiles") # ID of paper folder
# add data doi: note there could be multiple for various traits
metadata_add_source_doi(
  dataset_id = "SMP_reptiles", # ID of paper folder
  doi = "")
# adding traits from paper to database
metadata_add_traits(dataset_id = "SMP_reptiles") # ID of paper folder

metadata_check_custom_R_code("SMP_reptiles")

# 10) Oskyrko_2024
metadata_create_template("Oskyrko_2024") # ID of paper folder
# add data doi: note there could be multiple for various traits
metadata_add_source_doi(
  dataset_id = "Oskyrko_2024", # ID of paper folder
  doi = "10.1038/s41597-024-03079-5")
# adding traits from paper to database
metadata_add_traits(dataset_id = "Oskyrko_2024") # ID of paper folder

metadata_check_custom_R_code("Oskyrko_2024")

# 11) Meiri_2024
metadata_create_template("Meiri_2024") # ID of paper folder
# add data doi: note there could be multiple for various traits
metadata_add_source_doi(
  dataset_id = "Meiri_2024", # ID of paper folder
  doi = "10.1111/geb.13812")
# adding traits from paper to database
metadata_add_traits(dataset_id = "Meiri_2024") # ID of paper folder

metadata_check_custom_R_code("Meiri_2024")

# 12) Robertson_2019
metadata_create_template("Robertson_2019") # ID of paper folder
# add data doi: note there could be multiple for various traits
metadata_add_source_doi(
  dataset_id = "Robertson_2019", # ID of paper folder
  doi = "")
# adding traits from paper to database
metadata_add_traits(dataset_id = "Robertson_2019") # ID of paper folder

metadata_check_custom_R_code("Robertson_2019")

# 13) Clemann_2023
metadata_create_template("Clemann_2023") # ID of paper folder
# add data doi: note there could be multiple for various traits
metadata_add_source_doi(
  dataset_id = "Clemann_2023", # ID of paper folder
  doi = "")
# adding traits from paper to database
metadata_add_traits(dataset_id = "Clemann_2023") # ID of paper folder

metadata_check_custom_R_code("Clemann_2023")


# 14) Cunningham_2024
metadata_create_template("Cunningham_2024") # ID of paper folder
# add data doi: note there could be multiple for various traits
metadata_add_source_doi(
  dataset_id = "Cunningham_2024", # ID of paper folder
  doi = "10.1111/gcb.17130")
# adding traits from paper to database
metadata_add_traits(dataset_id = "Cunningham_2024") # ID of paper folder

metadata_check_custom_R_code("Cunningham_2024")


# 14) Wilman_2016
metadata_create_template("Wilman_2016") # ID of paper folder
# add data doi: note there could be multiple for various traits
metadata_add_source_doi(
  dataset_id = "Wilman_2016", # ID of paper folder
  doi = "10.1890/13-1917.1")
# adding traits from paper to database
metadata_add_traits(dataset_id = "Wilman_2016") # ID of paper folder

metadata_check_custom_R_code("Wilman_2016")



#test datasets. This will flag any erros with the yml files
dataset_test("Soria_2021")
dataset_test("Wilman_2014")
dataset_test("Myhrvold_2015")
dataset_test("Tobias_2022")
dataset_test("Oliveira_2017")
dataset_test("SMP_amph")
dataset_test("SMP_mamm")
dataset_test("SMP_birds")
dataset_test("SMP_reptiles")
dataset_test("Oskyrko_2024")
dataset_test("Meiri_2024")
dataset_test("Robertson_2019")
dataset_test("Clemann_2023")
dataset_test("Cunningham_2024")
dataset_test("Wilman_2016")


#build database
build_setup_pipeline(method = "base", database_name = "traits.build_database_CH")

source("build.R")

traits_db <- readRDS("export/data/curr/traits.build_database_CH.rds")


# Summarise number of traits from each dataset in the final database
traits_summary_by_dataset <- traits_db$traits %>%
  count(dataset_id, trait_name) %>%
  arrange(dataset_id, desc(n))

# View
print(traits_summary_by_dataset, n = 200)





