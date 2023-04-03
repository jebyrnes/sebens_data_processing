library(dplyr)
library(tidyr)
library(stringr)
library(readxl)


f <- list.files("Sessile QAQC files 2022",
                pattern = "HRI090820H2.xls",
                recursive = TRUE, full.names = TRUE)

f <- f[1]

dat <- read_excel(f, sheet = "Summary")

# note, early files might not have summary tab, but it's sheet 1

### Metadata
metadata <- tibble(
  # pull out the filename from the file string
  filename = str_extract(f, "([^/]+$)"),

  # pull out the site date
  site_date = names(dat)[3], #consistently the 3rd

  # pull out the number of quadrats in this site
  num_quads = dat[1,3]|>pull()|>as.numeric() #some formatting
)

### Reload data and prep for extraction
# reload data a few lines down with appropriate columns
dat_trimmed <- read_excel(f, sheet = "Summary", skip = 2)

# trim to only the quads available based on # of quads
names(dat_trimmed)[1:2] <- c("Species", "Sq")
dat_trimmed <- dat_trimmed |>
  select(c(Species, Sq, 1:metadata$num_quads+2))

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

### Substrate
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


# pivot longer and merge back with potential # of points and total #
make_dat_long <- function(dat, dat_totals, type){
  dat |>
    pivot_longer(cols = c(-Species, -Sq),
                 names_to = "Quad",
                 values_to = "Points",
                 values_transform = list(Points = as.numeric)
    ) |>
    left_join(sub_totals) %>%
    
    # add back metadata
    bind_cols(metadata, .) |>
    select(-num_quads) |>
    mutate(Type = type)
}

substrate_long <- make_dat_long(substrate, sub_totals, "Substrate")

### Canopy

# for canopy, pull out total # points and potential # of points
# then remove those lines saving info for later
canopy_totals <- get_totals(canopy, 
                            total = "TOTAL CANOPY", 
                            total_potential = "Total Potential Canopy")


# Remove any rows that are NA in Sq as they have no data
canopy <- canopy |>
  filter(!is.na(Sq)) |>
  filter((Sq %in% c("A", "B")))

# Make sure species names are constant for A and B
canopy <- canopy |>
  mutate(id = sort(rep(1:(nrow(canopy)/2),2))) |>
  group_by(id) |>
  arrange(Sq) |>
  mutate(Species = Species[1]) |>
  ungroup() |> arrange(Species, Sq) |>
  select(-id)

# if there is only square A, then canopy will be in B
if(!has_b){
  # make sure species copies down nicely (sometimes notes are in B row)
  canopy <- canopy |> 
    filter(Sq == "B") |>
    mutate(Sq = "A") # because that's what it was
}

# make the long canopy data
canopy_long <- make_dat_long(canopy, canopy_totals, "Canopy")


### Sediment

### Bind it all together
