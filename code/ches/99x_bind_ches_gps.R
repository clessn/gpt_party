library(dplyr)

source("code/ches/00_ches_naming.R")

df_gps <- readRDS("data/gps/data_party.rds")
df_ches <- readRDS("data/ches/tmp/ches_data_id_with_scores.rds")

# Rename columns to match the CHES dataset
df_gps_renamed <- df_gps %>%
  rename(
    ches_id = ID_CHES
  )

# Create a named vector with reversed mapping (party name -> ID)
party_to_id <- names(party_map)
names(party_to_id) <- party_map

# Now you can create the ches_id variable in your dataframe using match
# Assuming your dataframe is called 'df' and the party column is called 'party':
df_ches$ches_id <- party_to_id[df_ches$party]

# If you want to ensure numeric IDs, you can convert:
df_ches$ches_id <- as.numeric(df_ches$ches_id)

df_gps_clean <- df_gps_renamed %>%
  select(c(
    ID_GPS,
    ISO,
    Country,
    Partyname,
    Region,
    Type_Partysize_vote,
    Type_Partysize_seat,
    V4_Scale,
    V6_Scale,
    Ideology,
    Difficulty,
    LocationLatitude,
    LocationLongitude,
    Language,
    PartyPerVote,
    PartyPerSeats,
    ches_id,
    Pop2018,
    GDP,
    Region_name,
    group,
    party_alignment,
    econ_alignment,
    starts_with("Ideology_"),
    starts_with("econ_"),
    starts_with("sos_"),
  ))

df_combined <- cbind(df_ches, df_gps_clean, by = "ches_id")
