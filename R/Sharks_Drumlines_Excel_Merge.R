library(readxl)
library(dplyr)
library(chron)

# Read data ####
db <- readxl::read_xlsx(
  path = "../../Data/2024-03_NewDbaseFormat/SharkCapture.xlsx",
  sheet = "MERGED",
  range = NULL,
  col_names = TRUE,
  col_types = c( # "logical", "numeric", "date", "guess", "text"
    "text", "numeric", "date", "date", "text", "text", "numeric", "numeric", "numeric", "numeric",
    "text", "text", "text", "text", "numeric", "numeric", "numeric", "numeric", "numeric",
    "numeric", "text", "text", "logical", "logical", "numeric", "text", "text", "text",
    "text", "numeric", "numeric", "numeric", "numeric", "numeric", "logical", "text", "text",
    "numeric", "text", "text", "numeric", "numeric", "numeric", "numeric", "logical", "numeric",
    "numeric", "numeric", "logical", "logical", "logical", "logical", "logical", "logical",
    "logical", "logical", "text", "logical", "text", "text", "text", "logical", "logical",
    "logical", "logical", "text", "text"

  ),
  na = c("", "xxx", "NA"),
  trim_ws = TRUE,
  skip = 0,
  n_max = Inf,
  # guess_max = min(1000, n_max),
  progress = readxl_progress(),
  .name_repair = "unique"
)

# Clean data ####
db <- db |>
  mutate(
    # convert event_ts, time_in, time_out from yyyy-mm-dd hh:mm:ss to hh:mm:ss
    event_ts = stringr::str_sub(string = as.character(event_ts),
                                start = -8,
                                end = -1),
    time_in = chron::times(time_in),
    time_out = chron::times(time_out),
    # convert drumlines set_no ABC to 123 to groupby later
    set_no = as.integer(dplyr::case_match(
      set_no,
      "A" ~ "1",
      "B" ~ "2",
      "C" ~ "3",
      "D" ~ "4",
      "E" ~ "5",
      "F" ~ "6",
      "G" ~ "7",
      "H" ~ "8",
      "I" ~ "9",
      "J" ~ "10",
      "K" ~ "11",
      "L" ~ "12",
      "M" ~ "13",
      "N" ~ "14",
      "O" ~ "15",
      "P" ~ "16",
      .default = set_no
    )),
    # create index column to split and reattach & sort subsets later
    index = 1:nrow(db),
    # convert DBSOURCE drumlines to Drumlines, So it sorts properly everywhere
    DBSOURCE = stringr::str_to_title(DBSOURCE)
  ) |>
  # sort by date then source so groupby is predictable for first/last (didn't help, found na.rm=TRUE param for first/last)
  dplyr::arrange(event_dt, DBSOURCE)
# 4749 rows initially

# Subset merge data ####
# Subset to rows of concern: drumline data from Drumlines and Sharks tabs which has half info in each
dbfixpit <- db |>
  dplyr::filter(
    # filter for Sharks=drumline + Drumlines=all
    DBSOURCE == "Sharks" & (gear == "Drumline-top" | gear == "Drumline-bottom") | DBSOURCE == "Drumlines",
    # filter for species = no NA blanks #NA
    !is.na(species),
    # filter for PIT = no NA blanks etc
    stringr::str_sub(string = db$pit_tag_full_id_no,
                     start = 1,
                     end = 3) == "3DD"
  )

# Subset the remaining rows for joining later:
# shark tab catches not from drumlines,
# drumlines which caught nothing,
# no PIT tag
dbsafe <- db[which(!db$index %in% dbfixpit$index),] # 3842 rows

# Fix PITs ####
# Merge drumline tab data row info into sharks tab data row info for same entries
# 2024-09-12: 906 to 465 rows: 12 more rows than half. Check these.
# tmp <- dbfixpit |>
#   dplyr::group_by(event_dt, pit_tag_full_id_no) |>
#   dplyr::tally() |>
#   dplyr::filter(n == 1) |>
#   dplyr::select(-n)
dbfixpit <- dbfixpit |>
  # group by date & PIT
  dplyr::group_by(event_dt, pit_tag_full_id_no) |>
  # populating drumline entries into the sharks rows (dplyr first (drumlines) & last (Sharks))
  dplyr::summarise(DBSOURCE = dplyr::last(DBSOURCE, na_rm = TRUE),
                   id = dplyr::last(id, na_rm = TRUE),
                   # event_dt = dplyr::last(event_dt, na_rm = TRUE),
                   event_ts = dplyr::last(event_ts, na_rm = TRUE),
                   gear = dplyr::last(gear, na_rm = TRUE),
                   set_no = dplyr::last(set_no, na_rm = TRUE), # drumlines ABC sharks 123
                   hook_bouy_no = dplyr::last(hook_bouy_no, na_rm = TRUE),
                   time_in = dplyr::first(time_in, na_rm = TRUE),
                   time_out = dplyr::first(time_out, na_rm = TRUE),
                   soak_time = dplyr::first(soak_time, na_rm = TRUE),
                   bottom_top = dplyr::first(bottom_top, na_rm = TRUE),
                   site = dplyr::last(site, na_rm = TRUE),
                   habitat = dplyr::last(habitat, na_rm = TRUE),
                   substrate = dplyr::last(substrate, na_rm = TRUE),
                   latitude = dplyr::last(latitude, na_rm = TRUE),
                   longitude = dplyr::last(longitude, na_rm = TRUE),
                   depth_m = dplyr::last(depth_m, na_rm = TRUE),
                   temperature_C = dplyr::last(temperature_C, na_rm = TRUE),
                   salinity_ppt = dplyr::last(salinity_ppt, na_rm = TRUE),
                   do_mg_l = dplyr::last(do_mg_l, na_rm = TRUE),
                   tide = dplyr::first(tide, na_rm = TRUE),
                   bait_type = dplyr::first(bait_type, na_rm = TRUE),
                   bait_present = dplyr::first(bait_present, na_rm = TRUE),
                   bite_off = dplyr::first(bite_off, na_rm = TRUE),
                   shark_no = dplyr::last(shark_no, na_rm = TRUE),
                   common = dplyr::last(common, na_rm = TRUE),
                   species = dplyr::last(species, na_rm = TRUE),
                   # pit_tag_full_id_no = dplyr::last(pit_tag_full_id_no, na_rm = TRUE),
                   casey_tag_no = dplyr::last(casey_tag_no, na_rm = TRUE),
                   roto_tag_no = dplyr::last(roto_tag_no, na_rm = TRUE),
                   acoustic_tag_id = dplyr::last(acoustic_tag_id, na_rm = TRUE),
                   acoustic_tag_serial = dplyr::last(acoustic_tag_serial, na_rm = TRUE),
                   satellite_tag_id_no = dplyr::last(satellite_tag_id_no, na_rm = TRUE),
                   shark_recapture_no = dplyr::last(shark_recapture_no, na_rm = TRUE),
                   recap = dplyr::last(recap, na_rm = TRUE),
                   external_tag_no = dplyr::last(external_tag_no, na_rm = TRUE),
                   tag_owner = dplyr::last(tag_owner, na_rm = TRUE),
                   cam_tag_length_hrs = dplyr::last(cam_tag_length_hrs, na_rm = TRUE),
                   sex = dplyr::last(sex, na_rm = TRUE),
                   mature = dplyr::last(mature, na_rm = TRUE),
                   pcl = dplyr::last(pcl, na_rm = TRUE),
                   fl = dplyr::last(fl, na_rm = TRUE),
                   tl = dplyr::last(tl, na_rm = TRUE),
                   stl = dplyr::last(stl, na_rm = TRUE),
                   estimate = dplyr::last(estimate, na_rm = TRUE),
                   girth = dplyr::last(girth, na_rm = TRUE),
                   disc_width = dplyr::last(disc_width, na_rm = TRUE),
                   anal_fin_length = dplyr::last(anal_fin_length, na_rm = TRUE),
                   umbilical_scar = dplyr::last(umbilical_scar, na_rm = TRUE),
                   ultrasound = dplyr::last(ultrasound, na_rm = TRUE),
                   fin_genetics = dplyr::last(fin_genetics, na_rm = TRUE),
                   fin_isotopes = dplyr::last(fin_isotopes, na_rm = TRUE),
                   muscle_isotopes = dplyr::last(muscle_isotopes, na_rm = TRUE),
                   whole_blood_isotopes = dplyr::last(whole_blood_isotopes, na_rm = TRUE),
                   plasma_isotopes = dplyr::last(plasma_isotopes, na_rm = TRUE),
                   rbc_isotopes = dplyr::last(rbc_isotopes, na_rm = TRUE),
                   condition_note = dplyr::last(condition_note, na_rm = TRUE),
                   lost = dplyr::last(lost, na_rm = TRUE),
                   film_crew = dplyr::last(film_crew, na_rm = TRUE),
                   adoptee_name = dplyr::last(adoptee_name, na_rm = TRUE),
                   shark_name = dplyr::last(shark_name, na_rm = TRUE),
                   not_standardised = dplyr::last(not_standardised, na_rm = TRUE),
                   tagging_problem = dplyr::last(tagging_problem, na_rm = TRUE),
                   non_stb_recap = dplyr::last(non_stb_recap, na_rm = TRUE),
                   depredation = dplyr::last(depredation, na_rm = TRUE),
                   comments = dplyr::last(comments, na_rm = TRUE),
                   data_enterer_name = dplyr::last(data_enterer_name, na_rm = TRUE),
                   index = dplyr::last(index, na_rm = TRUE)
  ) |>
  dplyr::ungroup() |>
  # reorder columns, put event_ts & pit back in the right place.
  dplyr::select(DBSOURCE, id, event_dt, event_ts:pit_tag_full_id_no, everything())
# 915 initial, 464 after = 451 merged away

# Fix caseys ####
dbfixcasey <- db |>
  dplyr::filter(
    # filter for Sharks=drumline + Drumlines=all. Same as before, this is the bit we care about.
    DBSOURCE == "Sharks" & (gear == "Drumline-top" | gear == "Drumline-bottom") | DBSOURCE == "Drumlines",
    # filter for Casey = no NA blanks #NA
    !is.na(casey_tag_no),
    # filter for pit tag number IS na, else we're re-processing (some of) the same rows
    is.na(pit_tag_full_id_no)
  )

# Subset the remaining rows for joining later
dbsafe <- dbsafe[which(!dbsafe$index %in% dbfixcasey$index),] # 3821 rows

# Merge drumline tab data into sharks tab for same entries
# 2024-09-dbfixcasey: 20 to 465 rows: 4 more rows than half. Check these.
# tmp <- dbfixcasey |>
#   dplyr::group_by(event_dt, casey_tag_no) |>
#   dplyr::tally() |>
#   dplyr::filter(n == 1) |>
#   dplyr::select(-n)
dbfixcasey <- dbfixcasey |>
  # group by date & casey
  dplyr::group_by(event_dt, casey_tag_no) |>
  # populating drumline entries into the sharks rows (dplyr first (drumlines) & last (Sharks))
  dplyr::summarise(DBSOURCE = dplyr::last(DBSOURCE, na_rm = TRUE),
                   id = dplyr::last(id, na_rm = TRUE),
                   # event_dt = dplyr::last(event_dt),
                   event_ts = dplyr::last(event_ts, na_rm = TRUE),
                   gear = dplyr::last(gear, na_rm = TRUE),
                   set_no = dplyr::last(set_no), # drumlines ABC sharks 123
                   hook_bouy_no = dplyr::last(hook_bouy_no, na_rm = TRUE),
                   time_in = dplyr::first(time_in, na_rm = TRUE),
                   time_out = dplyr::first(time_out, na_rm = TRUE),
                   soak_time = dplyr::first(soak_time, na_rm = TRUE),
                   bottom_top = dplyr::first(bottom_top, na_rm = TRUE),
                   site = dplyr::last(site, na_rm = TRUE),
                   habitat = dplyr::last(habitat, na_rm = TRUE),
                   substrate = dplyr::last(substrate, na_rm = TRUE),
                   latitude = dplyr::last(latitude, na_rm = TRUE),
                   longitude = dplyr::last(longitude, na_rm = TRUE),
                   depth_m = dplyr::last(depth_m, na_rm = TRUE),
                   temperature_C = dplyr::last(temperature_C, na_rm = TRUE),
                   salinity_ppt = dplyr::last(salinity_ppt, na_rm = TRUE),
                   do_mg_l = dplyr::last(do_mg_l, na_rm = TRUE),
                   tide = dplyr::first(tide, na_rm = TRUE),
                   bait_type = dplyr::first(bait_type, na_rm = TRUE),
                   bait_present = dplyr::first(bait_present, na_rm = TRUE),
                   bite_off = dplyr::first(bite_off, na_rm = TRUE),
                   shark_no = dplyr::last(shark_no, na_rm = TRUE),
                   common = dplyr::last(common, na_rm = TRUE),
                   species = dplyr::last(species, na_rm = TRUE),
                   pit_tag_full_id_no = dplyr::last(pit_tag_full_id_no, na_rm = TRUE),
                   # casey_tag_no = dplyr::last(casey_tag_no, na_rm = TRUE),
                   roto_tag_no = dplyr::last(roto_tag_no, na_rm = TRUE),
                   acoustic_tag_id = dplyr::last(acoustic_tag_id, na_rm = TRUE),
                   acoustic_tag_serial = dplyr::last(acoustic_tag_serial, na_rm = TRUE),
                   satellite_tag_id_no = dplyr::last(satellite_tag_id_no, na_rm = TRUE),
                   shark_recapture_no = dplyr::last(shark_recapture_no, na_rm = TRUE),
                   recap = dplyr::last(recap, na_rm = TRUE),
                   external_tag_no = dplyr::last(external_tag_no, na_rm = TRUE),
                   tag_owner = dplyr::last(tag_owner, na_rm = TRUE),
                   cam_tag_length_hrs = dplyr::last(cam_tag_length_hrs, na_rm = TRUE),
                   sex = dplyr::last(sex, na_rm = TRUE),
                   mature = dplyr::last(mature, na_rm = TRUE),
                   pcl = dplyr::last(pcl, na_rm = TRUE),
                   fl = dplyr::last(fl, na_rm = TRUE),
                   tl = dplyr::last(tl, na_rm = TRUE),
                   stl = dplyr::last(stl, na_rm = TRUE),
                   estimate = dplyr::last(estimate, na_rm = TRUE),
                   girth = dplyr::last(girth, na_rm = TRUE),
                   disc_width = dplyr::last(disc_width, na_rm = TRUE),
                   anal_fin_length = dplyr::last(anal_fin_length, na_rm = TRUE),
                   umbilical_scar = dplyr::last(umbilical_scar, na_rm = TRUE),
                   ultrasound = dplyr::last(ultrasound, na_rm = TRUE),
                   fin_genetics = dplyr::last(fin_genetics, na_rm = TRUE),
                   fin_isotopes = dplyr::last(fin_isotopes, na_rm = TRUE),
                   muscle_isotopes = dplyr::last(muscle_isotopes, na_rm = TRUE),
                   whole_blood_isotopes = dplyr::last(whole_blood_isotopes, na_rm = TRUE),
                   plasma_isotopes = dplyr::last(plasma_isotopes, na_rm = TRUE),
                   rbc_isotopes = dplyr::last(rbc_isotopes, na_rm = TRUE),
                   condition_note = dplyr::last(condition_note, na_rm = TRUE),
                   lost = dplyr::last(lost, na_rm = TRUE),
                   film_crew = dplyr::last(film_crew, na_rm = TRUE),
                   adoptee_name = dplyr::last(adoptee_name, na_rm = TRUE),
                   shark_name = dplyr::last(shark_name, na_rm = TRUE),
                   not_standardised = dplyr::last(not_standardised, na_rm = TRUE),
                   tagging_problem = dplyr::last(tagging_problem, na_rm = TRUE),
                   non_stb_recap = dplyr::last(non_stb_recap, na_rm = TRUE),
                   depredation = dplyr::last(depredation, na_rm = TRUE),
                   comments = dplyr::last(comments, na_rm = TRUE),
                   data_enterer_name = dplyr::last(data_enterer_name, na_rm = TRUE),
                   index = dplyr::last(index, na_rm = TRUE)
  ) |>
  dplyr::ungroup() |>
  # reorder columns, put event_ts & pit back in the right place.
  dplyr::select(DBSOURCE, id, event_dt, event_ts:pit_tag_full_id_no, casey_tag_no, everything())
# 21 initial, 11 after = 10 merged away

# Find remaining issues/missed matches ####
dbfixremainder <- db |>
  dplyr::filter(
    # filter for Sharks=drumline + Drumlines=all. Same as before, this is the bit we care about.
    DBSOURCE == "Sharks" & (gear == "Drumline-top" | gear == "Drumline-bottom") | DBSOURCE == "Drumlines",
    # sharks only
    !is.na(species),
    # nothing we've done already
    is.na(pit_tag_full_id_no),
    is.na(casey_tag_no)
  ) |>
  dplyr::arrange(
    event_dt,
    DBSOURCE,
    time_in,
    event_ts
  )

dbsafe <- dbsafe[which(!dbsafe$index %in% dbfixremainder$index),] # 3598 rows

# tmp <- dbfixremainder |>
#   dplyr::group_by(event_dt, set_no, hook_bouy_no) |>
#   dplyr::tally() |>
#   dplyr::filter(n == 1) |>
#   dplyr::select(-n) # 2024-09-12: nrow 64/ 2024-10-10 nrow 30

dbfixremainder <- dbfixremainder |>
  dplyr::group_by(event_dt, set_no, hook_bouy_no) |>
  # populating drumline entries into the sharks rows (dplyr first (drumlines) & last (Sharks))
  dplyr::summarise(
    # DBSOURCE as count of input sources i.e. 2 if merged 1 if not
    DBSOURCE = dplyr::last(DBSOURCE, na_rm = TRUE),
    # n = dplyr::n(),
    # ID as list of input IDs, can filter for DBSOURCE=1 for unmatched pairs, & DBSOURCE=2 to see matches.
    # TMPid = list(c(dplyr::first(id, na_rm = TRUE), dplyr::last(id, na_rm = TRUE))),
    # ID as first/last keeps numeric format, can filter DBSOURCE=1 then use IDs to find rows which didn't match
    # id = dplyr::first(id, na_rm = TRUE),
    # ID as min
    id = min(id, na.rm = TRUE),
    # event_dt = dplyr::last(event_dt),
    event_ts = dplyr::last(event_ts, na_rm = TRUE),
    gear = dplyr::last(gear, na_rm = TRUE),
    # set_no = dplyr::last(set_no), # drumlines ABC sharks 123
    # hook_bouy_no = dplyr::last(hook_bouy_no, na_rm = TRUE),
    time_in = dplyr::first(time_in, na_rm = TRUE),
    time_out = dplyr::first(time_out, na_rm = TRUE),
    soak_time = dplyr::first(soak_time, na_rm = TRUE),
    bottom_top = dplyr::first(bottom_top, na_rm = TRUE),
    site = dplyr::last(site, na_rm = TRUE),
    habitat = dplyr::last(habitat, na_rm = TRUE),
    substrate = dplyr::last(substrate, na_rm = TRUE),
    latitude = dplyr::last(latitude, na_rm = TRUE),
    longitude = dplyr::last(longitude, na_rm = TRUE),
    depth_m = dplyr::last(depth_m, na_rm = TRUE),
    temperature_C = dplyr::last(temperature_C, na_rm = TRUE),
    salinity_ppt = dplyr::last(salinity_ppt, na_rm = TRUE),
    do_mg_l = dplyr::last(do_mg_l, na_rm = TRUE),
    tide = dplyr::first(tide, na_rm = TRUE),
    bait_type = dplyr::first(bait_type, na_rm = TRUE),
    bait_present = dplyr::first(bait_present, na_rm = TRUE),
    bite_off = dplyr::first(bite_off, na_rm = TRUE),
    shark_no = dplyr::last(shark_no, na_rm = TRUE),
    common = dplyr::last(common, na_rm = TRUE),
    species = dplyr::last(species, na_rm = TRUE),
    pit_tag_full_id_no = dplyr::last(pit_tag_full_id_no, na_rm = TRUE),
    casey_tag_no = dplyr::last(casey_tag_no, na_rm = TRUE),
    roto_tag_no = dplyr::last(roto_tag_no, na_rm = TRUE),
    acoustic_tag_id = dplyr::last(acoustic_tag_id, na_rm = TRUE),
    acoustic_tag_serial = dplyr::last(acoustic_tag_serial, na_rm = TRUE),
    satellite_tag_id_no = dplyr::last(satellite_tag_id_no, na_rm = TRUE),
    shark_recapture_no = dplyr::last(shark_recapture_no, na_rm = TRUE),
    recap = dplyr::last(recap, na_rm = TRUE),
    external_tag_no = dplyr::last(external_tag_no, na_rm = TRUE),
    tag_owner = dplyr::last(tag_owner, na_rm = TRUE),
    cam_tag_length_hrs = dplyr::last(cam_tag_length_hrs, na_rm = TRUE),
    sex = dplyr::last(sex, na_rm = TRUE),
    mature = dplyr::last(mature, na_rm = TRUE),
    pcl = dplyr::last(pcl, na_rm = TRUE),
    fl = dplyr::last(fl, na_rm = TRUE),
    tl = dplyr::last(tl, na_rm = TRUE),
    stl = dplyr::last(stl, na_rm = TRUE),
    estimate = dplyr::last(estimate, na_rm = TRUE),
    girth = dplyr::last(girth, na_rm = TRUE),
    disc_width = dplyr::last(disc_width, na_rm = TRUE),
    anal_fin_length = dplyr::last(anal_fin_length, na_rm = TRUE),
    umbilical_scar = dplyr::last(umbilical_scar, na_rm = TRUE),
    ultrasound = dplyr::last(ultrasound, na_rm = TRUE),
    fin_genetics = dplyr::last(fin_genetics, na_rm = TRUE),
    fin_isotopes = dplyr::last(fin_isotopes, na_rm = TRUE),
    muscle_isotopes = dplyr::last(muscle_isotopes, na_rm = TRUE),
    whole_blood_isotopes = dplyr::last(whole_blood_isotopes, na_rm = TRUE),
    plasma_isotopes = dplyr::last(plasma_isotopes, na_rm = TRUE),
    rbc_isotopes = dplyr::last(rbc_isotopes, na_rm = TRUE),
    condition_note = dplyr::last(condition_note, na_rm = TRUE),
    lost = dplyr::last(lost, na_rm = TRUE),
    film_crew = dplyr::last(film_crew, na_rm = TRUE),
    adoptee_name = dplyr::last(adoptee_name, na_rm = TRUE),
    shark_name = dplyr::last(shark_name, na_rm = TRUE),
    not_standardised = dplyr::last(not_standardised, na_rm = TRUE),
    tagging_problem = dplyr::last(tagging_problem, na_rm = TRUE),
    non_stb_recap = dplyr::last(non_stb_recap, na_rm = TRUE),
    depredation = dplyr::last(depredation, na_rm = TRUE),
    comments = dplyr::last(comments, na_rm = TRUE),
    data_enterer_name = dplyr::last(data_enterer_name, na_rm = TRUE),
    index = dplyr::last(index, na_rm = TRUE
    )
  ) |>
  dplyr::ungroup() |>
  # reorder columns, put event_ts & pit back in the right place.
  dplyr::select(DBSOURCE, id, event_dt, event_ts, gear, set_no, hook_bouy_no, everything()) #|> #  n, TMPid,
# only unmerged rows for review
# dplyr::filter(DBSOURCE == 1)

# 2024-09-XX: 209 to 136 rows = 73 merged away
# 2024-10-10: 211 to 126 rows = 85 merged away
# 548 merged away total. db 4757-548 = 4209 remaining rows due once all rejoined

# write.csv(x = dbfixremainder2,
#           file = "../../Data/2024-03_NewDbaseFormat/dbfixremainder2_2024-10-10.csv",
#           row.names = FALSE)



# There will likely be more to do due to things without PITs, etc.
# Subset dbsafe for those rows to see what they look like & if anything can be done. Possibly same action using casey or whatever.
dbsafeextra <- db |>
  dplyr::filter(
    # filter for Sharks=drumline + Drumlines=all. Same as before, this is the bit we care about.
    DBSOURCE == "Sharks" & (gear == "Drumline-top" | gear == "Drumline-bottom") | DBSOURCE == "Drumlines",
    # filter for species = no NA blanks #NA (no need to include these in safe CHECK since we're looking for shark data that was missed; no NA sharks are in Sharks tab)
    !is.na(species),
    # filter for PIT = IS blank (non blanks covered by dbfixpit)
    is.na(pit_tag_full_id_no),
    is.na(casey_tag_no)
  ) # 209. 2024-10-10: 222

# Remove dbfixpit & dbfixcasey rows
dbsafeextra <- dbsafeextra[which(!dbsafeextra$index %in% dbfixpit$index),]
dbsafeextra <- dbsafeextra[which(!dbsafeextra$index %in% dbfixcasey$index),]
dbsafeextra <- dbsafeextra[which(!dbsafeextra$index %in% dbfixremainder$index),] # 73 rows. 2024-10-10: 96 rows. 2024-10-15: 72.
# these have all been merged away in dbfixremainder
write.csv(x = dbsafeextra,
          file = "../../Data/2024-03_NewDbaseFormat/dbsafeextra.csv",
          row.names = FALSE)
# check dbsafeextra csv in excel:
# All Sharks tab, no drumline tab, no time, PIT, casey. Fine to use/keep as is.


# Join results to save other subset, dbsafe
dbclean <- dbfixpit |>
  dplyr::bind_rows(dbfixcasey,
                   dbfixremainder,
                   # dbsafeextra,
                   dbsafe) |>  # 4757-4298 = 459 less than db, merged away
  # copy time_out to event_ts if event_ts is NA
  dplyr::mutate(
    event_ts = dplyr::case_match(
      event_ts,
      NA ~ as.character(time_out),
      .default = event_ts
    )) |>
  # If DBSOURCE = drumlines & gear = NA then populate with same gear from that date (& set?)
  # dbclean |> dplyr::filter(DBSOURCE != "Drumlines") |> summarise(nagear = length(which(is.na(gear)))) # 0
  # Only drumline DBSOURCE has NA's so don't need to worry abvout filtering for DBSOURCE == Drumlines
  # could also maybe have used tidyr::fill()
  dplyr::group_by(event_dt) |>
  dplyr::mutate(
    gear = dplyr::case_match(
      gear,
      NA ~ dplyr::first(gear, na_rm = TRUE),
      .default = gear
    ),
    # Populate bottom_top from gear when bottom_top == NA & gear = Drumline-bottom or Drumline-top
    bottom_top = dplyr::case_when(
      gear == "Drumline-bottom" ~ "Bottom",
      gear == "Drumline-top" ~ "Top",
      .default = as.character(bottom_top)
    )
  ) |>
  dplyr::ungroup() |>
  dplyr::arrange(
    event_dt,
    DBSOURCE,
    time_in,
    event_ts
  )

# for loop
dbclean$id <- as.numeric(NA)
counter <- 1
for (i in 1:nrow(dbclean)) { # nrow(dbclean)
  # first ID = 1
  if (counter == 1) {
    dbclean$id[i] <- 1
  } else {
    # subsequent IDs: if pit_tag_full_id_no %in% (existing) pit_tag_full_id_no (and isn't NA, else NAs will match to the first NA)
    if (!is.na(dbclean$pit_tag_full_id_no[i]) & (dbclean$pit_tag_full_id_no[i] %in% dbclean$pit_tag_full_id_no[1:(i - 1)])) {
      # populate the i'th id
      dbclean$id[i] <-
        # with the first (min) pit-matching (which ==) ID from the prior (i - 1) rows. as.numeric because tibble SUCKS
        as.numeric(dbclean[min(which(dbclean$pit_tag_full_id_no[1:(i - 1)] == dbclean$pit_tag_full_id_no[i]), na.rm = TRUE), "id"])
      dbclean$recap[i] <- TRUE
      # Ditto casey_tag_no
    } else if (!is.na(dbclean$casey_tag_no[i]) & (dbclean$casey_tag_no[i] %in% dbclean$casey_tag_no[1:(i - 1)])) {
      dbclean$id[i] <- as.numeric(dbclean[min(which(dbclean$casey_tag_no[1:(i - 1)] == dbclean$casey_tag_no[i]), na.rm = TRUE), "id"])
      dbclean$recap[i] <- TRUE
      # Ditto satellite_tag_id_no
    } else if (!is.na(dbclean$satellite_tag_id_no[i]) & (dbclean$satellite_tag_id_no[i] %in% dbclean$satellite_tag_id_no[1:(i - 1)])) {
      dbclean$id[i] <- as.numeric(dbclean[min(which(dbclean$satellite_tag_id_no[1:(i - 1)] == dbclean$satellite_tag_id_no[i]), na.rm = TRUE), "id"])
      dbclean$recap[i] <- TRUE
    } else {
      # else populate the i'th id with 1 higher than the existing highest value
      dbclean$id[i] <- as.numeric(max(dbclean$id[1:(i - 1)], na.rm = TRUE) + 1)
    } # close nested if chain
  } # close if counter==1 if
  counter <- counter + 1 # increment counter
}

# Data enterer name values missing in dbclean?
length(which(!is.na(dbclean$data_enterer_name))) # 363; 391 in MERGED but likely includes dupes. Happy with this.

# Also doublecheck previous issues discussed above.
check <- db |> dplyr::summarise(across(everything(), ~ sum(!is.na(.)))) # all original rows
tmp <- db |> dplyr::summarise(across(everything(), ~ sum(!is.na(.))) / nrow(db)) # proportion of original rows with data
check <- dplyr::bind_rows(check, tmp)
tmp <- dbclean |> dplyr::summarise(across(everything(), ~ sum(!is.na(.)))) # all cleaned rows
check <- dplyr::bind_rows(check, tmp)
tmp <- dbclean |> dplyr::summarise(across(everything(), ~ sum(!is.na(.))) / nrow(dbclean)) # proportion of cleaned rows with data
check <- dplyr::bind_rows(check, tmp)
check <- as.data.frame(check)
check[5, ] <- check[3,] - check[1, ] # cleaned rows minus all rows
check[6, ] <- check[4,] - check[2, ] # cleaned rows proportion minus all rows proportion
check <- check |>
  dplyr::mutate(
    Source = c("db", "db", "dbclean", "dbclean","calc", "calc"),
    Type = c("DataLength", "DataProp", "DataLength", "DataProp", "DataLength", "DataProp")
  ) |>
  dplyr::select(Source, Type, everything())
write.csv(x = check,
          file = "../../Data/2024-03_NewDbaseFormat/dbMergeCheck.csv",
          row.names = FALSE)

dbclean <- dbclean |>
  dplyr::select(
    DBSOURCE:event_ts, # move PIT later
    gear:bite_off, # remove shark_no
    common:species,
    pit_tag_full_id_no, # move PIT later
    casey_tag_no:data_enterer_name # remove index
  )

write.csv(x = dbclean,
          file = "../../Data/2024-03_NewDbaseFormat/dbclean.csv",
          row.names = FALSE)
# Join this sheet into SharkCapture
