tmp <- shark %>% select(Common, STL, Sex, Mature) %>% filter(Common == "Caribbean Reef") # 130
tmp <- shark %>% select(Common, STL, Sex, Mature) %>% filter(Common == "Caribbean Reef", !is.na(Mature)) # 112 = correct

tmp <- tmp2 %>% select(Common, STL, Sex, Mature) %>% filter(Common == "Caribbean Reef") # 100, 101 w/o batpit
tmp <- tmp2 %>% select(Common, STL, Sex, Mature) %>% filter(Common == "Caribbean Reef", !is.na(Mature)) # 87 # 88 wo badpit
# no change with dropna

tmp2 <- drumline %>% select(CaribbeanReef, STL, Sex, Mature) %>% filter(CaribbeanReef == 1, !is.na(Mature)) # 87 Better but not right.

# do we know all pits are unique in the drumline "master"?
# presumably losing sharks with no (xxx) PIT tag?
# 13 dupe PITs. 14 missing PITs = 27 which would be lost. 13 which should be lost?
# database: drumline tab: n reefs: 110 (any dupes in here? NO)
# database: shark    tab: n reefs: 130
# why is there a discrepancy even here?


# need to dedupe PITs in shark dbase

# Can't use shark dbase as don't have zeroes from unsuccessful drumlines and therefore no reef shark CPUE



# check all data present, should be 112 non na mature caribbean reefs what about dupes? Those
# numbers were from the shark table, not the drumline. Right?
tmp <- drumline %>% select(CaribbeanReef, STL, Sex, Mature) %>% filter(CaribbeanReef == 1, Mature == TRUE) # 45
tmp <- drumline %>% select(CaribbeanReef, STL, Sex, Mature) %>% filter(CaribbeanReef == 1, Mature == FALSE) # 37
tmp <- drumline %>% select(CaribbeanReef, STL, Sex, Mature) %>% filter(CaribbeanReef == 1, !is.na(Mature)) # 105
tmp <- drumline %>% select(CaribbeanReef, STL, Sex, Mature) %>% filter(CaribbeanReef == 1) # 82.
# drumline spreadsheet:
# all carib reef 110 - 7 dupes = 103. So possibly we have 2 extra. Run anyway, time's wasting.
