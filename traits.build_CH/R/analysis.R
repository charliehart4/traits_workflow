# =====================================================================
# analysis.R
# ---------------------------------------------------------------------
# Purpose: Read in traits database, filter by taxon list, prioritise duplicates based on dataset year
# fill gaps using ecological logic (e.g. birds = volant)
# Author: Charlie Hart
# Date: 2025-09-15
# =====================================================================


# 1. Setup ---------------------------------------------------------------------
pacman::p_load(traits.build, dplyr, tidyr, ggplot2, readr, stringr)

# Set working directory (adjust if needed)
setwd("C:/Users/charliehart/The University of Melbourne/Billy Geary - fire_traits/CH_Step_4/traits.build_CH/")
getwd()

# Read compiled database
traits_db <- readRDS("export/data/curr/traits.build_database_CH.rds")
# Read in taxon list (if not already read)
taxon_list <- read_csv("config/taxon_list.csv")


# Quick check
glimpse(traits_db$traits)



##filter traits


# Build a lookup that preserves the “official” taxon_name ------------------

# We’ll treat taxon_list$taxon_name as the official name,
# and Sci_Name_Alt as the alternate. Carry TAXON_ID along.
taxon_lookup <- taxon_list %>%
  select(TAXON_ID, taxon_name, Sci_Name_Alt) %>%
  pivot_longer(
    cols      = c(taxon_name, Sci_Name_Alt),
    names_to  = "source",         # “taxon_name” vs. “Sci_Name_Alt”
    values_to = "lookup_name"
  ) %>%
  filter(!is.na(lookup_name)) %>%  # drop any NA alt names
  distinct(TAXON_ID, lookup_name, source)

# Join traits_db$traits to that lookup by trait’s taxon_name ---------------
# This will attach TAXON_ID and source (“taxon_name” or “Sci_Name_Alt”) to each trait row
joined <- traits_db$traits %>%
  left_join(
    taxon_lookup,
    by = c("taxon_name" = "lookup_name")
  )

# Keep only rows that matched (i.e. TAXON_ID is not NA) ----------------------
filtered_traits <- joined %>%
  filter(!is.na(TAXON_ID))

# Overwrite each trait‐row’s taxon_name with the official taxon_list name --
#    If a row matched on Sci_Name_Alt, we want to rename it back to the main taxon_name.
filtered_traits <- filtered_traits %>%
  mutate(taxon_name = 
           if_else(source == "Sci_Name_Alt",
                   # find the correct official name via a second join
                   taxon_list$taxon_name[match(TAXON_ID, taxon_list$TAXON_ID)],
                   # if source == "taxon_name", keep it as is
                   taxon_name
           )
  ) %>%
  select(-source)  # drop the helper column

# Confirm you now have exactly 563 distinct species -------------------------
cat(
  "Distinct official species after filtering: ",
  n_distinct(filtered_traits$taxon_name),
  "\n"
)
# Should read: 563








# 3. Summarise duplicates ------------------------------------------------------

# Count duplicates: species-trait combinations
duplicates_summary_filtered <- filtered_traits %>%
  count(taxon_name, trait_name) %>%
  filter(n > 1) %>%
  arrange(desc(n))

# Print
print(duplicates_summary_filtered, n = 100)

# How many duplicated species-trait pairs?
n_duplicates_filtered <- nrow(duplicates_summary_filtered)
cat("Number of duplicated species-trait pairs (filtered):", n_duplicates_filtered, "\n")


# 4. Introduce priority rule based on dataset year -----------------------------

# Extract year from dataset_id (assumes format like "Soria_2021", "Myhrvold_2015")
filtered_traits <- filtered_traits %>%
  mutate(
    dataset_year = case_when(
      str_detect(dataset_id, "SMP") ~ 2015,                       # SMP datasets manually assigned 2015
      TRUE ~ str_extract(dataset_id, "\\d{4}") %>% as.numeric()   # Otherwise extract year from dataset_id
    )
  )

# 5. Prioritise by year, ignoring NA or blank values --------------------------

traits_prioritised <- filtered_traits %>%
  filter(!is.na(value) & value != "") %>%    # Only keep rows with non-blank values
  group_by(taxon_name, trait_name) %>%
  slice_max(order_by = dataset_year, n = 1, with_ties = FALSE) %>%
  ungroup()

# Check
glimpse(traits_prioritised)


# 6. Pivot to wide format ------------------------------------------------------

traits_filtered_wide <- traits_prioritised %>%
  select(taxon_name, trait_name, value) %>%
  pivot_wider(
    names_from = trait_name,
    values_from = value
  )

# Check final wide table
glimpse(traits_filtered_wide)
print(traits_filtered_wide, n = 20)




# 7. Summarise missing values per trait ----------------------------------------

missing_summary_per_trait <- traits_filtered_wide %>%
  summarise(across(-taxon_name, ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "trait", values_to = "n_missing") %>%
  arrange(n_missing)

# Print
cat("\nMissing values per trait:\n")
print(missing_summary_per_trait, n = 50)



# 8. Prepare for Gap Filling ---------------------------------------------------

# Read in taxon list (if not already read)
taxon_list <- read_csv("config/taxon_list.csv")

# Join TaxaGroup and TAXON_TYPE onto traits_filtered_wide using taxon_name
traits_filtered_wide <- traits_filtered_wide %>%
  left_join(taxon_list %>% select(taxon_name, COMMON_NAME, TaxaGroup, TAXON_ID, TAXON_TYPE), by = "taxon_name")

# Quick check
glimpse(traits_filtered_wide)
print(traits_filtered_wide, n = 20)

# Summarise how many species per TaxaGroup
taxa_group_summary <- traits_filtered_wide %>%
  count(TaxaGroup) %>%
  arrange(desc(n))

cat("\nNumber of species per TaxaGroup:\n")
print(taxa_group_summary)

taxon_list
traits_filtered_wide




# 9. Manual and Logical Gap Filling --------------------------------------------------

# Convert character columns to numeric where relevant
traits_filtered_wide <- traits_filtered_wide %>%
  mutate(across(c(
   max_longevity_d,
   dispersal_km,
   home_range_km2,
    litter_size_n,
    litters_per_year_n,
    n_offspring_year
  ), ~ as.numeric(.x)))



# 9. Fill volant ---------------------------------------------------------------


traits_filtered_wide <- traits_filtered_wide %>%
  mutate(
    volant = case_when(
      # 1. Force all gliders to 1, even if they had a previous value
      TAXON_ID %in% c(11147, 11133, 11136, 11138, 11137) ~ "1",
      
      # 2. Keep any existing 0/1 for non‐gliders
      !is.na(volant) & volant %in% c("0", "1") ~ volant,
      
      # 3. Birds: volant = 1 except Emu
      TaxaGroup == "Birds" & taxon_name != "Dromaius novaehollandiae" ~ "1",
      TaxaGroup == "Birds" & taxon_name == "Dromaius novaehollandiae" ~ "0",
      
      # 4. Reptiles and Amphibians: volant = 0
      TaxaGroup %in% c("Reptiles", "Amphibians") ~ "0",
      
      # 5. Bats: volant = 1
      TaxaGroup == "Mammals" & TAXON_TYPE == "Bats" ~ "1",
      
      # 6. All other Mammals: volant = 0
      TaxaGroup == "Mammals" ~ "0",
      
      # 7. Everything else stays unchanged
      TRUE ~ volant
    )
  )





# 9.1 Fill hibernation_torpor --------------------------------------------------

# First, define the set of mammal TAXON_IDs known to use torpor
torpor_ids <- c(
  # Gliders
  11147,   # Acrobates pygmaeus  (Feathertail Glider)
  11133,   # Petauroides volans  (Greater Glider)
  11136,   # Petaurus australis  (Yellow-bellied Glider)
  11138,   # Petaurus breviceps  (Sugar Glider)
  11137,   # Petaurus norfolcensis (Squirrel Glider)
  
  # Antechinus
  11028,   # Antechinus agilis      (Agile Antechinus)
  11027,   # Antechinus flavipes    (Yellow-footed Antechinus)
  11034,   # Antechinus minimus maritimus (Swamp Antechinus)
  11033,   # Antechinus swainsonii  (Dusky Antechinus)
  
  # Dasyurid/other small marsupials & rodents
  11072,   # Sminthopsis crassicaudata (Fat-tailed Dunnart)
  11438,   # Mastacomys fuscus mordicus (Broad-toothed Rat)
  11839,   # Mormopterus sp. 2 (Eastern Freetail Bat)
  11809,   # Mormopterus sp. 3 (Inland Freetail Bat)
  11808,   # Mormopterus sp. 4 (Southern Freetail Bat)
  11324,   # Tadarida australis (White-striped Freetail Bat)
  11480,   # Notomys mitchelli (Mitchell's Hopping-mouse)
  11097,   # Perameles nasuta (Long-nosed Bandicoot)
  11017,   # Phascogale tapoatafa (Brush-tailed Phascogale)
  11457,   # Pseudomys apodemoides (Silky Mouse)
  11458,   # Pseudomys fumeus (Smoky Mouse)
  11455,   # Pseudomys novaehollandiae (New Holland Mouse)
  11468,   # Pseudomys shortridgei (Heath Mouse)
  11280,   # Pteropus poliocephalus (Grey-headed Flying-fox)
  11395,   # Rattus fuscipes (Bush Rat)
  11398,   # Rattus lutreolus (Swamp Rat)
  11061,   # Sminthopsis murina murina (Common Dunnart)
  11115,   # Trichosurus cunninghami (Mountain Brushtail Possum)
  11165    # Vombatus ursinus (Common Wombat)
)

# Now apply the combined logic
traits_filtered_wide <- traits_filtered_wide %>%
  mutate(
    hibernation_torpor = case_when(
      # 1. If this is a reptile or amphibian AND missing, assume torpor (1)
      (is.na(hibernation_torpor) | hibernation_torpor == "") &
        TaxaGroup %in% c("Reptiles", "Amphibians") ~ "1",
      
      # 2. If this is a bird AND missing, assume no torpor (0)
      (is.na(hibernation_torpor) | hibernation_torpor == "") &
        TaxaGroup == "Birds" ~ "0",
      
      # 3. If this mammal is in our known‐torpor list, force torpor (1)
      TAXON_ID %in% torpor_ids ~ "1",
      
      # 4. Otherwise keep any existing value (0, 1, or NA)
      TRUE ~ hibernation_torpor
    )
  )

# Summarise the updated hibernation_torpor values
hib_summary <- traits_filtered_wide %>%
  count(hibernation_torpor) %>%
  arrange(desc(n))

cat("\nSummary of hibernation_torpor values:\n")
print(hib_summary)







# 9.2b Correct bat diets with Pteropus exception -------------------------------

traits_filtered_wide <- traits_filtered_wide %>%
  mutate(
    # 1) For all bats except Grey-headed Flying-fox (11280): carnivore, granivore, omnivore = "0"
    diet_carnivore = case_when(
      TAXON_TYPE == "Bats" & TAXON_ID != 11280 ~ "0",
      TRUE                                       ~ diet_carnivore
    ),
    diet_granivore = case_when(
      TAXON_TYPE == "Bats" & TAXON_ID != 11280 ~ "0",
      TRUE                                       ~ diet_granivore
    ),
    diet_omnivore = case_when(
      TAXON_TYPE == "Bats" & TAXON_ID != 11280 ~ "0",
      TRUE                                       ~ diet_omnivore
    ),
    
    # 2) For Grey-headed Flying-fox (11280): diet_herbivore = "1"; all other bats with herbivore stay as is
    diet_herbivore = case_when(
      TAXON_ID == 11280 & TAXON_TYPE == "Bats" ~ "1",
      TRUE                                      ~ diet_herbivore
    ),
    
    # 3) For all bats except Grey-headed Flying-fox: diet_inflorescence = "0";
    #    For Grey-headed Flying-fox (11280): diet_inflorescence = "1"
    diet_inflorescence = case_when(
      TAXON_ID == 11280 & TAXON_TYPE == "Bats" ~ "1",
      TAXON_TYPE == "Bats"                     ~ "0",
      TRUE                                     ~ diet_inflorescence
    ),
    
    # 4) For all bats except Grey-headed Flying-fox: diet_invertivore = "1";
    #    For Grey-headed Flying-fox: diet_invertivore = "0"
    diet_invertivore = case_when(
      TAXON_ID == 11280 & TAXON_TYPE == "Bats" ~ "0",
      TAXON_TYPE == "Bats"                     ~ "1",
      TRUE                                     ~ diet_invertivore
    )
  )

# 5. Verify bat diet corrections
bat_diet_summary3 <- traits_filtered_wide %>%
  filter(TAXON_TYPE == "Bats") %>%
  count(
    diet_carnivore,
    diet_granivore,
    diet_omnivore,
    diet_herbivore,
    diet_inflorescence,
    diet_invertivore
  )

cat("\nUpdated bat diet summary:\n")
print(bat_diet_summary3)






# 9.3 Fill stratum for birds ---------------------------------------------------

traits_filtered_wide <- traits_filtered_wide %>%
  mutate(
    # Only fill if missing
    stratum_saxicolous = ifelse(TaxaGroup == "Birds" & (is.na(stratum_saxicolous) | stratum_saxicolous == ""), "0", stratum_saxicolous),
    stratum_fossorial = ifelse(TaxaGroup == "Birds" & (is.na(stratum_fossorial) | stratum_fossorial == ""), "0", stratum_fossorial),
    
    # Only set stratum_water if missing
    stratum_water = case_when(
      (is.na(stratum_water) | stratum_water == "") & TAXON_TYPE == "Waders" ~ "1",
      (is.na(stratum_water) | stratum_water == "") & TAXON_TYPE == "Passerine birds" ~ "0",
      TRUE ~ stratum_water
    )
  )



# 9.4 Merge stratum_aquatic and stratum_water ----------------------------------

# Convert to numeric safely first (if still in character format)
traits_filtered_wide <- traits_filtered_wide %>%
  mutate(
    stratum_aquatic_num = as.numeric(stratum_aquatic),
    stratum_water_num   = as.numeric(stratum_water)
  )

# Take the max of the two (1 if either is 1, otherwise 0 or NA if both are NA)
traits_filtered_wide <- traits_filtered_wide %>%
  mutate(
    stratum_aquatic = as.character(pmax(stratum_aquatic_num, stratum_water_num, na.rm = TRUE))
  ) %>%
  select(-stratum_aquatic_num, -stratum_water_num, -stratum_water)  # Drop helper and original columns




# 9.5 Fix up binary stratum data ----------------------------------------------------------
traits_filtered_wide <- traits_filtered_wide %>%
  mutate(
    stratum_aerial = case_when(TaxaGroup %in% c("Reptiles", "Amphibians") ~ "0", 
                               TRUE~stratum_aerial),
    stratum_aquatic = case_when(is.na(stratum_aquatic) ~ "0", TRUE ~ stratum_aquatic),
    stratum_fossorial = case_when(is.na(stratum_fossorial) & TaxaGroup == "Birds" ~ "0", TRUE ~ stratum_fossorial)
  )



# 9.6 Fix up binary nesting data ----------------------------------------------------------

traits_filtered_wide <- traits_filtered_wide %>%
  mutate(
    # nest_cave: assume 0 for reptiles, amphibians, and birds
    nest_cave = case_when(
      TaxaGroup %in% c("Reptiles", "Amphibians", "Birds") & is.na(nest_cave) ~ "0",
      TRUE ~ nest_cave
    ),
    
    # nest_burrow: assume 0 for birds and bats
    nest_burrow = case_when(
      (TaxaGroup == "Birds" | (TaxaGroup == "Mammals" & TAXON_TYPE == "Bats")) & is.na(nest_burrow) ~ "0",
      TRUE ~ nest_burrow
    ),
    
    # nest_branch: assume 0 for reptiles, amphibians, mammals
    nest_branch = case_when(
      TaxaGroup %in% c("Reptiles", "Amphibians", "Mammals") & is.na(nest_branch) ~ "0",
      TRUE ~ nest_branch
    ),
    
    # nest_ground: assume 0 for bats
    nest_ground = case_when(
      TaxaGroup == "Mammals" & TAXON_TYPE == "Bats" & is.na(nest_ground) ~ "0",
      TRUE ~ nest_ground
    ),
    
    # nest_hollows: assume 0 for reptiles and amphibians
    nest_hollows = case_when(
      TaxaGroup %in% c("Reptiles", "Amphibians") & is.na(nest_hollows) ~ "0",
      TRUE ~ nest_hollows
    )
  )

# Summarise missing nesting trait values by TaxaGroup
nesting_summary_by_group <- traits_filtered_wide %>%
  pivot_longer(cols = starts_with("nest_"), names_to = "trait", values_to = "value") %>%
  filter(is.na(value)) %>%
  count(TaxaGroup, trait) %>%
  arrange(trait, desc(n))

cat("\nMissing nesting values by TaxaGroup:\n")
print(nesting_summary_by_group, n = 50)



# 9.7 Fix litters_per_year_n and n_offspring_year --------------------------------

traits_filtered_wide <- traits_filtered_wide %>%
  mutate(
    # 1) Wherever litter_size_n is present but litters_per_year_n is missing:
    #    • If not a Mammal, or if Mammal with Mass_g > 1000 g, set litters_per_year_n = 1
    litters_per_year_n = case_when(
      !is.na(litter_size_n) &
        (is.na(litters_per_year_n) | litters_per_year_n == "") &
        (TaxaGroup != "Mammals" |
           (TaxaGroup == "Mammals" & as.numeric(Mass_g) > 1000)) ~ 1,
      TRUE ~ litters_per_year_n
    ),
    
    # 2) Compute n_offspring_year whenever we now have both litter_size_n and litters_per_year_n
    n_offspring_year = case_when(
      !is.na(litter_size_n) &
        !is.na(litters_per_year_n) ~ litter_size_n * litters_per_year_n,
      TRUE ~ n_offspring_year
    )
  )

# 3) Summarise how many remain missing after this step
missing_repro2 <- traits_filtered_wide %>%
  filter(
    (is.na(litter_size_n) | litter_size_n == "") &
      (is.na(litters_per_year_n) | litters_per_year_n == "" |
         is.na(n_offspring_year)   | n_offspring_year == "")
  ) %>%
  select(taxon_name, litter_size_n, litters_per_year_n, n_offspring_year)

cat("\nSpecies still missing reproductive data:\n")
print(missing_repro2, n = Inf)


# 9.8 Enhanced summary of activity traits by TaxaGroup ---------------------------

activity_summary_by_taxa <- traits_filtered_wide %>%
  select(taxon_name, TaxaGroup,
         activity_nocturnal,
         activity_diurnal,
         activity_crepuscular,
         activity_cathemeral) %>%
  pivot_longer(
    cols = starts_with("activity_"),
    names_to = "trait",
    values_to = "value"
  ) %>%
  mutate(status = ifelse(is.na(value), "missing", "present")) %>%
  count(TaxaGroup, trait, status) %>%
  pivot_wider(
    names_from = status,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(TaxaGroup, trait)

cat("\nActivity trait completeness by TaxaGroup:\n")
print(activity_summary_by_taxa, n = 100)

# 9.9 Fill cathermeral = 0 for mammals  ---------------------------

# Ensure activity_cathemeral is numeric before filling
traits_filtered_wide <- traits_filtered_wide %>%
  mutate(activity_cathemeral = as.numeric(activity_cathemeral)) %>%
  mutate(
    activity_cathemeral = case_when(
      TaxaGroup == "Mammals" & is.na(activity_cathemeral) ~ 0,
      TRUE ~ activity_cathemeral
    )
  )





# 10. Summarise missing values per trait ----------------------------------------

missing_summary_per_trait <- traits_filtered_wide %>%
  summarise(across(-taxon_name, ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "trait", values_to = "n_missing") %>%
  arrange(n_missing)

# Print
cat("\nMissing values per trait:\n")
print(missing_summary_per_trait, n = 50)





# 10.2 Drop unwanted traits ----------------------------------------------------------

traits_filtered_wide <- traits_filtered_wide %>% select(-c(stratum_cryptic, stratum_generalist))



# 11. Export ---------------------------------------------------------------------

write.csv(traits_filtered_wide, "export/data/traits_filtered_wide.csv", row.names = FALSE)
