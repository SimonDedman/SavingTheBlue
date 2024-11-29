### ====================================================================================================
### Project:    Andros hammerheads - space use and trophic ecology
### Analysis:   Time-At-Temperature SPOT tag data visualisation using histograms
### Script:     ~SavingTheBlue/R/08_TAT_Histograms_SPOT_tags.R
### Author:     Vital Heim
### Version:    1.0
### ====================================================================================================

### ....................................................................................................
### Content: this script contains the code to summarise and visualise time-at-temperature histogram
###          data from Smart Position and Temperature (SPOT) tags
### ....................................................................................................

### ....................................................................................................
### [A] Ready environment, load packages ----
### ....................................................................................................

# A1: clear memory ----

rm(list = ls())

# A2: load necessary packages ----

## if first time
# install.packages("dplyr")
# install.packages("magrittr")
# install.packages("reshape2")
# install.packages("tidyr")
# install.packages("png")

## load
library(dplyr)
library(magrittr)
library(reshape2)
library(tidyr)
library(png)

# A3: Specify data and saveloc ----
saveloc <- "//Sharktank/Science/Projects_current/Andros_Hammerheads/Data_output/TATs/" # Adjust this

### ....................................................................................................
### [B] Data import and housekeeping ----
### ....................................................................................................

# B1: Import TAT-data ----

tat_all <- "//Sharktank/Science/Projects_current/Andros_Hammerheads/Data_input"

all_csv <- dir(tat_all, recursive = T, full.names = T, pattern = "\\-Histos.csv$") # import files in folders in path directory all at once

myhistos <- lapply(all_csv, read.csv, sep = ",", dec = ".", stringsAsFactor = F, header = T) # import all .csv files containing TAT-Hiso data, but skip header lines

df_tat <- do.call("rbind", myhistos)

# sort(unique(df_tat$Ptt))
# > sort(unique(df_tat$Ptt)) # 9 individuals, 1 pprm [20240825]
# [1] 183623 200368 200369 209020 222133 235283 244607 244608
# [9] 261743

# B2: housekeeping, tidy data and define column classes ----

# str(df_tat)
## general tidy, select needed columns
df_tat %<>%
  dplyr::select( # select relevant columns
    Ptt,
    HistType,
    Date,
    Count,
    NumBins,
    Sum,
    Bin1, Bin2, Bin3, Bin4, Bin5, Bin6, Bin7, Bin8, Bin9, Bin10, Bin11, Bin12
  ) %>%
  mutate( # define Date format: ATTENTION! MONTH SEEMS TO BE IN 3 LETTER CODE, USE "%b" IF SO
    Date = as.POSIXct(Date, format = "%H:%M:%S %d-%b-%Y", tz = "UTC"),
    month = format(Date, format = "%b", tz = "UTC"), # get rid of year for later season variable definitions
    Ptt = as.character(Ptt) # define tag id as character class
  ) %>%
  filter( # every df starts with a row containing TAT limits - remove them
    !(HistType %in% c("TATLIMITS")
    )
  ) %>%
  dplyr::select( # get rid of HistType, count, nr. bins and sum column
    Ptt,
    Date,
    month,
    Bin1, Bin2, Bin3, Bin4, Bin5, Bin6, Bin7, Bin8, Bin9, Bin10, Bin11, Bin12
  ) %>%
  rename(
    elasmo = Ptt,
    datetime = Date
  ) %>%
  mutate( # add a grouping variable for the shark's home island, wet/dry season and meteorological season
    group = with(., case_when( # home island
      (elasmo == 177940 |
        elasmo == 177941 |
        elasmo == 177942 |
        elasmo == 23596) ~ "Bimini",
      is.na(elasmo) ~ "somethingWrong", # there should only be observations of sharks with tag ids
      TRUE ~ "Andros"
    )),
    season_bahamas = with(., case_when( # summer/winter based on temperature by van Zinnicq Bergmann et al. 2022 ; summer =  1st Jun to 30th Nov, winter = 1st Dec to 31st May
      month %in% c("May", "Jun", "Jul", "Aug", "Sep", "Oct") ~ "summer",
      month %in% c("Nov", "Dec", "Jan", "Feb", "Mar", "Apr") ~ "winter",
      TRUE ~ "seasonnonexistent"
    )),
    season_meteorol = with(., case_when( # Winter: 1st december - 28th february, Spring: 1st march - 31 may. #Summer: 1st june - 31 august, autumn: 1st september - 30th november
      month %in% c("Dec", "Jan", "Feb") ~ "winter",
      month %in% c("Mar", "Apr", "May") ~ "spring",
      month %in% c("Jun", "Jul", "Aug") ~ "summer",
      month %in% c("Sep", "Oct", "Nov") ~ "autumn", # Guttridge et al. 2017 season definitions are moved by 1 month later, e.g. winter Jan-March
      TRUE ~ "seasonnonexistent"
    ))
  )

## Now, the data is in a wide format with each bin being its own column. For summaries it is easier to have
## the data in the long format. Use reshape package to convert from wide to long data frame format.
wide <- df_tat

long <- melt(wide, id.vars = c("elasmo", "datetime", "month", "group", "season_bahamas", "season_meteorol"))
colnames(long)[7:8] <- c("bin", "percent")

df_tat <- long

## Data is ready for further summaries and visualization

### ....................................................................................................
### [C] Data summaries, means and more ----
### ....................................................................................................

# C1: Summarise the data by individual ----

smry_individual <- df_tat %>%
  dplyr::select( # selec the columns that are directly relevant
    elasmo,
    season_bahamas,
    bin,
    percent
  ) %>%
  group_by( # group by individual and season
    elasmo,
    season_bahamas,
    bin
  ) %>%
  summarize(
    count = n(),
    mean = mean(percent),
    sd = sd(percent)
  )

## add bin limits
smry_individual %<>%
  mutate( # add temperature labels for bin nrs
    tempC = case_when(
      bin == "Bin1" ~ "<16",
      bin == "Bin2" ~ "16-18",
      bin == "Bin3" ~ "18-20",
      bin == "Bin4" ~ "20-22",
      bin == "Bin5" ~ "22-24",
      bin == "Bin6" ~ "24-25",
      bin == "Bin7" ~ "25-26",
      bin == "Bin8" ~ "26-27",
      bin == "Bin9" ~ "27-28",
      bin == "Bin10" ~ "28-30",
      bin == "Bin11" ~ "30-34",
      bin == "Bin12" ~ ">34",
      is.na(bin) ~ "test", # there should only be observations of sharks with tag ids
      TRUE ~ "test"
    )
  )

## save
write.csv(smry_individual, paste0(saveloc, "TAT_summary_individuals.csv"))


# C2: Summarise the data by home island ----

smry_island <- df_tat %>%
  dplyr::select( # selec the columns that are directly relevant
    group,
    season_bahamas,
    bin,
    percent
  ) %>%
  group_by( # group by individual and season
    group,
    season_bahamas,
    bin
  ) %>%
  summarize(
    count = n(),
    mean = mean(percent),
    sd = sd(percent)
  )

## add bin limits
smry_island %<>%
  mutate( # add temperature labels for bin nrs
    tempC = case_when(
      bin %in% c("Bin1") ~ "<16",
      bin %in% c("Bin2") ~ "16-18",
      bin == "Bin3" ~ "18-20",
      bin == "Bin4" ~ "20-22",
      bin == "Bin5" ~ "22-24",
      bin == "Bin6" ~ "24-25",
      bin == "Bin7" ~ "25-26",
      bin == "Bin8" ~ "26-27",
      bin == "Bin9" ~ "27-28",
      bin == "Bin10" ~ "28-30",
      bin == "Bin11" ~ "30-34",
      bin == "Bin12" ~ ">34",
      is.na(bin) ~ "test", # there should only be observations of sharks with tag ids
      TRUE ~ "test"
    )
  )

## save
write.csv(smry_island, paste0(saveloc, "TAT_summary_island.csv"))


## make a list of all summaries for easier plotting later
summaries <- list(smry_individual, smry_island)

### ....................................................................................................
### [D] Statistical comparisons ----
### ....................................................................................................

## There are different ways to compare histograms and/or barplots. One option is to assess histograms by
## calculating a variety of distance measures. See here: https://www.mva-org.jp/Proceedings/2015USB/papers/14-15.pdf

## We can also use chi-square test as we are interested in comparing the nr. "samples" within each bin between
## summer and winter.

## However, using percentages in a chi-square test is tricky as as the reliability of a percentage outcome
## depends on the number of observations it was based on. Therefore, we transform the time % into actual
## counts.

## The here used SPOT tags were programmed to create a time at temperature summary across 24 hours and
## the temperature within this summary period was sampled every 3 seconds. This means, that per day, i.e.
## summary, a total of 28 800 sampled were taken based on which the perencetages were calculated.

## This means we can transform the percentages by multiplying them by 288 (28800 / 100 x percent <=> 288 x percent)

## We then can do chi-square tests on two-way contingency tables of bin number versus summer/winter.
## For each animal, we add up the counts for each bin across all days separately for summer versus
## winter. Latly, we omit temperature bins that have zero counts when summed over summer and winter
## for that animal.

## We can do the comparisons for all tags that have data within both seasons, i.e. 8 SPOT tags.
## Given that we run multiple comparisons, we use a Holm-Bonferroni adjustment for the 8 individual p-values.
## For more information see here:
## https://stats.stackexchange.com/questions/657695/compare-time-spent-within-different-temperature-bins-between-seasons/657813#657813

# D1: prepare your count data ----

df_chi <- df_tat %>%
  dplyr::select( # select needed columns only
    elasmo,
    datetime,
    season_bahamas,
    bin,
    percent
  ) %>%
  dplyr::arrange( # arrange by animal id and datetime for easier visualisation
    elasmo,
    datetime,
    bin
  ) %>%
  dplyr::mutate( # convert percentages to actual counts using temperature sampling interval (3s) and summary period (24h)
    counts = round(288 * percent, 0)
  )

# D2: Summarise count data ----

## we want to add up counts for each bin across all days separately for summer and winter for each animal
count_sums <- df_chi %>%
  dplyr::group_by(
    elasmo,
    season_bahamas,
    bin
  ) %>%
  dplyr::summarize(
    n_days = n(),
    total_counts = sum(counts)
  ) %>%
  dplyr::ungroup() %>% # ungroup again
  dplyr::group_by(
    elasmo,
    season_bahamas
  ) %>%
  dplyr::mutate( # calculate the total nr. counts for each season
    total_counts_season = sum(total_counts)
  ) %>%
  dplyr::ungroup() %>% # ungroup to work with the entire data set again
  dplyr::mutate( # calculate the percentage spent in each bin based on total counts per season
    season_percent = (total_counts/total_counts_season)*100
  )


## omit all rows per individual where a bin count is 0 during summer AND winter
count_sums %<>%
  dplyr::group_by(
    elasmo,
    bin
  ) %>%
  dplyr::filter(
    !(all(season_percent == 0 & season_bahamas %in% c("winter", "summer")))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(
    elasmo
    ) %>%
  dplyr::filter(
    !n_distinct(season_bahamas) == 1
  )


# D3: run a chi-square test for each animal ----

## make a list for your individual animals
fishlist <- unique(count_sums$elasmo)

## initiate a dataframe to store your chi-square results
final_chisq <- data.frame(
  elasmo = as.character(),
  x2_statistic = as.numeric(),
  degfreedom = as.integer(),
  p_value = as.numeric()
)

## run a chisquare test for each animal using a for loop
for (i in fishlist) { # i <- fishlist[1]

  df_i <- count_sums[which(count_sums$elasmo == i),] #subset to each fish

  # Create a contingency table with rows as season and columns as bin
  contingency_table_i <- xtabs(percent_season ~ season_bahamas + bin, data = df_i, drop.unused.levels = T)

  # Perform the Chi-square test for the subsetted animal
  chi_square_test_i <- chisq.test(contingency_table_i)

  # Print the results for the subsetted animal
  print(chi_square_test_i)

  # Store the results in the results dataframe
  final_chisq <- rbind(final_chisq, data.frame(
    elasmo = i,
    x2_statistic = chi_square_test_i$statistic,
    degfreedom = chi_square_test_i$parameter,
    p_value = chi_square_test_i$p.value
  ))
}

## adjust p-values for multiple comparisons using holm-bonferroni
pvalues_raw <- final_chisq$p_value
final_chisq$p_value_corr <- p.adjust(pvalues_raw, method = "holm")


## save your chi square statistics
write.csv(final_chisq, paste0(saveloc, "Chisquare_results_individual_total_counts.csv"), row.names = F)


### ....................................................................................................
### [E] Visualise data ----
### ....................................................................................................

# The TAT histograms can be visualised by group and season using mirrored, horizontal barplots
# base-R barplots need either a vector or a wide format df for their height argument
# we can filter the data before we plot to not have to worry about dataframe formatting

# The mirrored barplot will have a left side with bars running from the center to the left (xlim = c(100,0))
# and a right side with bars running from the center to the right (xlim = c(0,100))
# Additionally the plot will have a centered vertical y-axis (axis at side 4)

# To get the plots mirrored we can use th par(mfrow) option and define 1 row, 2 columns

# E1: choose your data subsets you want to plot ----

# ........................................ CHOOSE YOUR SUMMARY DATA FRAME FOR THE PLOT
sd <- 2 # 1 = individual summaries, 2 = island summaries
# ...................................................................................

## if you chose 2 or 3 on line 196 ##
# ................................... CHOOSE YOUR GROUP FOR THE LEFT SIDE OF THE PLOT
g1 <- 1 # choose home island: 1 = andros, 2 = bimini
sb1 <- 1 # choose bahamian season: 1 = summer, 2 = winter
# sm1 = 1 # choose meteorological season: 1 = autumn, 2 = spring, 3 = summer, 4 = winter
# ...................................................................................

# ................................... CHOOSE YOUR GROUP FOR THE RIGHT SIDE OF THE PLOT
g2 <- 1 # choose home island: 1 = andros, 2 = bimini
sb2 <- 2 # choose bahamian season: 1 = summer, 2 = winter
# sm1 = 1 # choose meteorological season: 1 = autumn, 2 = spring, 3 = summer, 4 = winter
# ...................................................................................

# E2: prepare filter groups, design/layout parameters and  data frame ----

## define your dataframe for plotting
df_plot <- summaries[[sd]]

## define individual ptts
ids <- if (sd == 1) {
  sort(unique(df_plot$elasmo))
} else {
  ("All")
}

## define groups
groups <- if (sd == 1) {
  "NA"
} else {
  (sort(unique(df_plot$group)))
}

## define seasons
seasons <- sort(unique(df_plot$season_bahamas))
#> seasons
# [1] "summer" "winter"

## define the colours for plotting
summercol <- "#F9DF44"
wintercol <- "#01AAC7"

## define temperature bin labels
bins <- c(
  "  < 16",
  " 16-18",
  " 18-20",
  " 20-22",
  " 22-24",
  " 24-25",
  " 25-26",
  " 26-27",
  " 27-28",
  " 28-30",
  " 30-34",
  "  > 34"
)

# E3: create the barplot for individuals ----

if (sd == 1) {
  for (i in ids) {
    ## Open plot window
    tiff(paste0(saveloc, "TAT_Histo_", i, "_summer_winter_", Sys.Date(),".tiff"), width = 12, height = 10, units = "cm", res = 300)

    ## We want two columns, 1 row
    par(mfrow = c(1, 2))

    ## *E3.1: left side barplot ----
    # i <- "261743"
    ## Left side plot data frame
    df_left <- filter(df_plot, elasmo == i, season_bahamas == seasons[sb1])

    ## Margins should be minimal on the left, and top, close to default bottom and right
    par(
      mar = c(3, 0.75, 1.5, 2.5), # A numeric vector of length 4, which sets the margin sizes in the following order: bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1).
      mgp = c(2, 0.75, 0), # A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
      las = 1
    ) # numeric value indicating the orientation of the tick mark labels and any other text added to a plot after its initialization. The options are as follows: always parallel to the axis (the default, 0), always horizontal (1), always perpendicular to the axis (2), and always vertical (3).

    ## Barplot
    bp_l <- barplot(
      height = df_left$mean,
      horiz = T,
      xlim = c(100, 0),
      las = 1,
      xlab = "", ylab = ""
      # , xaxt = "n"
      , yaxt = "n",
      cex.axis = 0.65,
      border = "black",
      col = ifelse(df_left$season_bahamas == "summer", summercol, wintercol),
    )

    ## add error bars
    segments(df_left$mean - df_left$sd, bp_l, df_left$mean + df_left$sd, bp_l,
      lwd = 1
    )

    arrows(df_left$mean - df_left$sd, bp_l, df_left$mean + df_left$sd, bp_l,
      lwd = 1, angle = 90,
      code = 3, length = 0.05
    )

    ## label vertical axis
    axis(4, at = bp_l, labels = bins, cex.axis = 0.65, tck = -0.035, las = 1) # tickmarks and labels

    ## label horizontal axis
    mtext(expression(bold("[%] of time")), side = 1, line = 1.6, cex = 0.65)

    ## *E3.2: right side barplot ----

    ## right side plot data frame
    df_right <- filter(df_plot, elasmo == i & season_bahamas == seasons[sb2])

    ## Margins should be minimal on the left, and top, close to default bottom and right
    par(
      mar = c(3, 0.5, 1.5, 2.75), # A numeric vector of length 4, which sets the margin sizes in the following order: bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1).
      mgp = c(2, 0.75, 0), # A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
      las = 1
    ) # numeric value indicating the orientation of the tick mark labels and any other text added to a plot after its initialization. The options are as follows: always parallel to the axis (the default, 0), always horizontal (1), always perpendicular to the axis (2), and always vertical (3).

    ## Barplot (account for the scenario where no data for winter/summer)
    if (nrow(df_right) == 0 || is.null(df_right)) {
      # Create an empty plot if data_right is empty
      bp_r <- barplot(
        height = df_left$mean,
        horiz = T,
        xlim = c(0, 100),
        las = 1,
        xlab = "", ylab = ""
        # , xaxt = "n"
        , yaxt = "n",
        cex.axis = 0.65,
        border = "white",
        col = "white"
      )

      ## add tickmarks and labels
      axis(2, at = bp_r, labels = NA, cex.axis = 0.65, tck = -0.035, las = 1) # tickmarks and labels
    } else {
      bp_r <- barplot(
        height = df_right$mean,
        horiz = T,
        xlim = c(0, 100),
        las = 1,
        xlab = "", ylab = ""
        # , xaxt = "n"
        , yaxt = "n",
        cex.axis = 0.65,
        border = "black",
        col = ifelse(df_right$season_bahamas == "summer", summercol, wintercol)
      )

      ## add error bars
      segments(df_right$mean - df_right$sd, bp_r, df_right$mean + df_right$sd, bp_r,
        lwd = 1
      )

      arrows(df_right$mean - df_right$sd, bp_r, df_right$mean + df_right$sd, bp_r,
        lwd = 1, angle = 90,
        code = 3, length = 0.05
      )

      ## add tickmarks to vertical axis
      axis(2, at = bp_r, labels = NA, cex.axis = 0.65, tck = -0.035, las = 1) # tickmarks and labels
    }

    ## label horizontal axis
    mtext(expression(bold("[%] of time")), side = 1, line = 1.6, cex = 0.65)


    # *E3.3: add some optical elements ----

    ## Add unit of vertical axis above and centered
    mtext(expression(bold("[°C]")), side = 3, line = -0.5, at = -17, cex = 0.65)

    ## legend
    legend("bottomright",
      legend = c("wet", "dry"),
      fill = c(summercol, wintercol),
      cex = 0.65,
      text.font = 3,
      bty = "n"
    )

    ## add the PTT ID
    shape <- readPNG("//Sharktank/Science/Projects_current/Andros_Hammerheads/Data_input/hammerhead_shape.png")
    rasterImage(shape, 50, 1.2, 73, 2.5, interpolate = T) # hammerhead shape
    id_label <- as.character(i)
    text(84.5, 1.7, paste0(id_label), cex = .65, font = 4) # add ptt label

    dev.off()
  }
}


# E4: Create the barplots for the entire island group ----

if (sd == 2) {
  ## Open plot window
  tiff(paste0(saveloc, "TAT_Histo_", groups[g1], "_summer_winter_", Sys.Date(),".tiff"), width = 12, height = 10, units = "cm", res = 300)

  ## We want two columns, 1 row
  par(mfrow = c(1, 2))

  ## *E4.1: left side barplot ----

  ## Left side plot data frame
  df_left <- filter(df_plot, group == groups[g1] & season_bahamas == seasons[sb1])

  ## Margins should be minimal on the left, and top, close to default bottom and right
  par(
    mar = c(3, 0.75, 1.5, 2.5), # A numeric vector of length 4, which sets the margin sizes in the following order: bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1).
    mgp = c(2, 0.75, 0), # A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
    las = 1
  ) # numeric value indicating the orientation of the tick mark labels and any other text added to a plot after its initialization. The options are as follows: always parallel to the axis (the default, 0), always horizontal (1), always perpendicular to the axis (2), and always vertical (3).

  ## Barplot
  bp_l <- barplot(
    height = df_left$mean,
    horiz = T,
    xlim = c(100, 0),
    las = 1,
    xlab = "", ylab = ""
    # , xaxt = "n"
    , yaxt = "n",
    cex.axis = 0.65,
    border = "black",
    col = ifelse(df_left$season_bahamas == "summer", summercol, wintercol),
  )

  ## add error bars
  segments(df_left$mean - df_left$sd, bp_l, df_left$mean + df_left$sd, bp_l,
    lwd = 1
  )

  arrows(df_left$mean - df_left$sd, bp_l, df_left$mean + df_left$sd, bp_l,
    lwd = 1, angle = 90,
    code = 3, length = 0.05
  )

  ## label vertical axis
  axis(4, at = bp_l, labels = bins, cex.axis = 0.65, tck = -0.035, las = 1) # tickmarks and labels

  ## label horizontal axis
  mtext(expression(bold("[%] of time")), side = 1, line = 1.6, cex = 0.65)

  ## *E4.2: right side barplot ----

  ## right side plot data frame
  df_right <- filter(df_plot, group == groups[g2] & season_bahamas == seasons[sb2])

  ## Margins should be minimal on the left, and top, close to default bottom and right
  par(
    mar = c(3, 0.5, 1.5, 2.75), # A numeric vector of length 4, which sets the margin sizes in the following order: bottom, left, top, and right. The default is c(5.1, 4.1, 4.1, 2.1).
    mgp = c(2, 0.75, 0), # A numeric vector of length 3, which sets the axis label locations relative to the edge of the inner plot window. The first value represents the location the labels (i.e. xlab and ylab in plot), the second the tick-mark labels, and third the tick marks. The default is c(3, 1, 0).
    las = 1
  ) # numeric value indicating the orientation of the tick mark labels and any other text added to a plot after its initialization. The options are as follows: always parallel to the axis (the default, 0), always horizontal (1), always perpendicular to the axis (2), and always vertical (3).

  ## Barplot
  bp_r <- barplot(
    height = df_right$mean,
    horiz = T,
    xlim = c(0, 100),
    las = 1,
    xlab = "", ylab = ""
    # , xaxt = "n"
    , yaxt = "n",
    cex.axis = 0.65,
    border = "black",
    col = ifelse(df_right$season_bahamas == "summer", summercol, wintercol),
  )

  ## add error bars
  segments(df_right$mean - df_right$sd, bp_r, df_right$mean + df_right$sd, bp_r,
    lwd = 1
  )

  arrows(df_right$mean - df_right$sd, bp_r, df_right$mean + df_right$sd, bp_r,
    lwd = 1, angle = 90,
    code = 3, length = 0.05
  )

  ## add tickmarks to vertical axis
  axis(2, at = bp_r, labels = NA, cex.axis = 0.65, tck = -0.035, las = 1) # tickmarks and labels

  ## label horizontal axis
  mtext(expression(bold("[%] of time")), side = 1, line = 1.6, cex = 0.65)


  # *E4.3: add some optical elements ----

  ## Add unit of vertical axis above and centered
  mtext(expression(bold("[°C]")), side = 3, line = -0.5, at = -17, cex = 0.65)

  ## legend
  legend("bottomright",
    legend = c("wet", "dry"),
    fill = c(summercol, wintercol),
    cex = 0.65,
    text.font = 3,
    bty = "n"
  )

  ## add the island shape
  shape <- readPNG(ifelse(df_left$group == "Andros",
    "//Sharktank/Science/Projects_current/Andros_Hammerheads/Data_input/Andros_shape.png",
    "//Sharktank/Science/Projects_current/Andros_Hammerheads/Data_input/Bimini_shape.png"
  ))

  rasterImage(shape, ifelse(df_left$group == "Andros", 55, 67.5), 1.25, 100, 5.75, interpolate = T)

  dev.off()
}
