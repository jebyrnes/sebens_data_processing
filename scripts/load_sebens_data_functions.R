#' ------------------------------------------------
#' Functions to load in a Sebens data file 
#' in the summary format
#' ------------------------------------------------

### load libraries
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, stringr, readxl, readr, glue)

### The main function
load_sebens_data_file <- function(f){

  # note, early files might not have summary tab, but it's sheet 1
  dat <- read_excel(f, sheet = "Summary",
                    .name_repair = "minimal")
  
  # get metadata
  metadata <- get_sebens_metadata(f, dat)
  
  # reload the data
  dat_trimmed <- reload_sebens_data(f)
  
  # get the pieces
  dat_split <-  split_sebens_data(dat_trimmed)
  
  # process substrate and figure out if we are using A
  # only or A and b
  subs_list <- process_substrate(dat_split$substrate, metadata)
  substrate_long = subs_list$substrate_long
  has_b <- subs_list$has_b
  
  # canopy
  canopy_long <- process_canopy(dat_split$canopy, has_b, metadata)

  # sediment
  sediment_long <- process_sediment(dat_split$sediment, has_b, metadata)
  
  ### Bind it all together
  final_dat <- bind_rows(substrate_long, 
                         canopy_long,
                         sediment_long)
  
  # write out temp file
  fname <- metadata$filename[1] |>
    str_remove("\\.xls.*$")
  
  write_csv(final_dat, glue("output_data/{fname}.csv"))
  
}

get_sebens_metadata <- function(f, dat){
  ### Metadata
  metadata <- tibble(
    # pull out the filename from the file string
    filename = str_extract(f, "([^/]+$)"),
    
    # pull out the site date
    site_date = names(dat)[3], #consistently the 3rd
    
    # pull out the number of quadrats in this site
    num_quads = dat[1,3]|>pull()|>as.numeric() #some formatting
  )
  
  metadata
}

reload_sebens_data <- function(f){
  
  ### Reload data and prep for extraction
  # reload data a few lines down with appropriate columns
  dat_trimmed <- read_excel(f, sheet = "Summary", skip = 2,
                            .name_repair = "minimal")
  
  # trim to only the quads available based on # of quads
  names(dat_trimmed)[1:2] <- c("Species", "Sq")
  
  dat_trimmed <- dat_trimmed |>
    select(c(Species, Sq, 1:metadata$num_quads+2))
  
  dat_trimmed
}

split_sebens_data <- function(dat_trimmed){
  
  # split canopy and substrate into two data frames
  # first, get locations of where the breaks happen
  canopy_idx <- grep("^CANOPY$", dat_trimmed$Species)
  sediment_idx <- grep("^SEDIMENT", dat_trimmed$Species)
  sediment_end_idx <- grep("^TOTAL SEDIMENT$", dat_trimmed$Species)
  
  # split data into substrate, canopy, and sediment
  substrate <- dat_trimmed |>
    slice(1L:(canopy_idx-1))
  
  canopy <- dat_trimmed |>
    slice(canopy_idx:(sediment_idx-1))
  
  sediment <- dat_trimmed |>
    slice(sediment_idx:sediment_end_idx)
  
  list(substrate = substrate,
       canopy = canopy,
       sediment = sediment)
}

# for substrate, pull out total # points and potential # of points
# then remove those lines saving info for later
get_totals <- function(dat, total, total_potential){
  dat |>
    filter(Species == total |
             Species == total_potential) |>
    pivot_longer(cols = c(-Species, -Sq),
                 names_to = "Quad",
                 values_to = "Points",
                 values_transform = list(Points = as.numeric)
    ) |>
    select(-Sq) |>
    pivot_wider(names_from = "Species",
                values_from = "Points") |>
    rename(total_points = total,
           potential_points = total_potential)
}


# pivot longer and merge back with potential # of points and total #
make_dat_long <- function(dat, dat_totals, type, metadata){
  dat |>
    pivot_longer(cols = c(-Species, -Sq),
                 names_to = "Quad",
                 values_to = "Points",
                 values_transform = list(Points = as.numeric)
    ) |>
    left_join(dat_totals) %>%
    
    # add back metadata
    bind_cols(metadata, .) |>
    select(-num_quads) |>
    mutate(Type = type)
}

## Fix up ragged names
# Make sure species names are constant for A and B
make_consistent_species_names <- function(dat){
  dat |>
    mutate(id = sort(rep(1:(nrow(dat)/2),2))) |>
    group_by(id) |>
    arrange(Sq) |>
    mutate(Species = Species[1]) |>
    ungroup() |> arrange(Species, Sq) |>
    select(-id)
  
}

## Functions to process the three types of data
process_substrate <- function(substrate, metadata){
  
  # for substrate, pull out total # points and potential # of points
  # then remove those lines saving info for later
  sub_totals <- get_totals(substrate, 
                           total = "TOTAL SUBSTRATE", 
                           total_potential = "potential substrate")
  # Remove any rows that are NA in Sq as they have no data
  substrate <- substrate |> 
    filter(!is.na(Sq))
  
  # Check and see if B has NAs and 0s only - if so remove those lines
  # and note that this is A only
  # we determine this by if there are > 5 NA values for B 
  # as 5 NAs is reasonable for typos AND if the sum of all values 
  # for b > 0, implying there are non-zero values in B
  b_substrate <- sum(is.na(substrate |> filter(Sq=="B") |> pull(`1`)))
  b_values <- sum(0 < (substrate |> filter(Sq=="B") |> pull(`1`)), na.rm = TRUE)
  has_b <- (b_values != 0) & (b_substrate < 5)
  
  if(!has_b){
    substrate <- substrate |> 
      filter(Sq == "A")
  }
  
  # make long
  substrate_long <- make_dat_long(substrate, sub_totals, "Substrate", metadata)
  
  return(list(substrate_long = substrate_long, 
         has_b = has_b))
}

process_canopy <- function(canopy, has_b, metadata){
  # for canopy, pull out total # points and potential # of points
  # then remove those lines saving info for later
  canopy_totals <- get_totals(canopy, 
                              total = "TOTAL CANOPY", 
                              total_potential = "Total Potential Canopy")
  
  
  # Remove any rows that are NA in Sq as they have no data
  canopy <- canopy |>
    filter(!is.na(Sq)) |>
    filter((Sq %in% c("A", "B")))
  canopy <- make_consistent_species_names(canopy)
  
  # if there is only square A, then canopy will be in B
  if(!has_b){
    # make sure species copies down nicely (sometimes notes are in B row)
    canopy <- canopy |> 
      filter(Sq == "B") |>
      mutate(Sq = "A") # because that's what it was
  }
  
  # make the long canopy data
  canopy_long <- make_dat_long(canopy, canopy_totals, 
                               "Canopy", metadata)
  
  return(canopy_long)
}

# function to get sediment info
process_sediment <- function(sediment, has_b, metadata){
  sediment <- sediment |>
    filter(!is.na(Sq)) |>
    filter((Sq %in% c("A", "B"))) |>
    make_consistent_species_names()
  
  if(!has_b){
    sediment <- sediment |> 
      filter(Sq == "A")
  }
  
  # reshape the sediment to long to merge with data
  # note - summing total points here, and the data
  # has no "potential points" - ask Ken if we should use 
  # something from canopy or substrate
  sediment_long <- sediment |>
    pivot_longer(cols = c(-Species, -Sq),
                 names_to = "Quad",
                 values_to = "Points",
                 values_transform = list(Points = as.numeric)) |>
    group_by(Quad, Sq) |>
    mutate(total_points = sum(Points),
           potential_points = 200) |>
    ungroup() %>%
    bind_cols(metadata, .) |>
    select(-num_quads) |>
    mutate(Type = "Sediment")
  
  return(sediment_long)
  
}
