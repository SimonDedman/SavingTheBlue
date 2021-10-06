# 2021-10-04 Silky shark depths
# For STB depth tag purchase background
# First connectTOPP
# silky shark is 69
library(tidyverse)
library(magrittr)
library(tidylog)

tblsharkdeployment %<>%
  filter(stri_startswith_fixed(
    str = sgdepkey,
    pattern = "69"
  ))

tblshark_pdt <- tbl(con, "tblshark_pdt") %>% collect() # 2021-10-04 shark

tblshark_pdt %<>%
  filter(stri_startswith_fixed(
    str = eventid,
    pattern = "69"
  )) %>%
  group_by(eventid, date) %>%
  summarise(MaxDepth = max(depth, na.rm = TRUE))

tblshark_pdt %>%
  group_by(eventid) %>%
  summarise(MaxDepth = max(MaxDepth, na.rm = TRUE)) %>%
  left_join(tblsharkdeployment %>%
    rename(
      eventid = sgdepkey,
      PCL = len3
    ) %>%
    select(eventid, project, seriesname, sex, dlatdd, dlondd, tagtype, PCL))

#     eventid MaxDepth project seriesname             sex   dlatdd dlondd tagtype   PCL
# 1 690400200      980 NonTOPP Ocean Exploration 2004 F      12.5  -110.  satellite NA
# 2 690400300      972 NonTOPP Ocean Exploration 2004 F      12.6  -110.  satellite NA
# 3 690400400      100 NonTOPP Ocean Exploration 2004 F      13.9  -111.  satellite NA
# 4 691300100      400 Chagos  Chagos 2013            U      -5.26   72.0 satellite NA
# 5 691300200     1280 Chagos  Chagos 2013            F      -6.84   71.2 satellite NA
# 6 691700100      400 GTOPP   Palau 2017             F       7.55  134.  satellite 146
# 7 691700200      424 GTOPP   Palau 2017             NA      7.34  134.  satellite 155
# 8 691700300      880 GTOPP   Palau 2017             F       7.34  134.  satellite 164
# 9 691800100      392 Chagos  Chagos 2018            Male   -7.14   72.2 satellite 105
