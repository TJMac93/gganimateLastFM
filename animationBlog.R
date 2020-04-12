library(scrobbler)
library(ggplot2)
library(gganimate)
library(lubridate)
library(gifski)
library(gganimate)
library(magrittr)
library(tidyr)
library(dplyr)


### 1. Api/ scrobbler part ----
username <- "Username"
apiKey <- "XXXXX"
apiSecret <- "XXXXX"

lastFM <- download_scrobbles(username = username, api_key = apiKey)
write.csv(lastFM, file = "lastFMfromAPI.csv")

# API method takes ages - use below if needed
lastFM <- read.csv("lastFMfromAPI.csv", stringsAsFactors = F)
lastFM <- lastFM[,-1]

# subset to only relevant data - song, artist, album, date
lastFM2 <- lastFM[,c(1,4,6,8)]



## 2. Set date column to date type ----
# 2.1 remove time from output
lastFM2$date <- gsub(",.*","",lastFM2$date)            # regex - removes everything after a comma

# 2.2 Convert to date
class(lastFM2[1,4])
lastFM2$date <- lubridate::dmy(lastFM2$date)
class(lastFM2[1,4])



# 2.3 Date imputation
# Songs scrobbled from iTunes have 1970 as their date. 
# Set to a random date in the window 2010/01/01 - 2011/04/06 (ymd format)
# 14610 is UNIX date for 2010/01/01
# 15070 is UNIX date for 2011/04/06
as_date(floor(runif(n = 1, min = 14610, max = 15070)))                         # test to get date

# Count number of entries with 1970 as date
count1970 <- length(lastFM2[lastFM2$date <= "1971-01-01",4])

# replace them with random date in scope above
lastFM2[lastFM2$date <= "1971-01-01",4]<- as_date(floor(runif(n = count1970,  min = 14610, max = 15070)))


# 3. group by month including number of plays ----
# 3.1 order from oldest to newest
lastFM2 <- lastFM2[order(lastFM2$date),]

# 3.2 adds total number of plays for each artist
lastFM2 <- lastFM2 %>% group_by(artist) %>%
  mutate(count=row_number())


# 3.3 split date into year and month columns
lastFM2$year <- year(lastFM2$date)
lastFM2$month <- month(lastFM2$date)

# 3.4 Add monthID column so same month in different years has different chronology
# Jan 2010 is month 1, Feb 2010 is month 2
# Jan 2011 should be month 13, Feb 2011 month 14 etc.
lastFM2$monthID <- lastFM2$month + ((lastFM2$year - 2010)*12)


# 3.5 Change date to remove day var
lastFM2$dateMY <- format(lastFM2$date, format="%m-%y")


# 3.6 Grouped by month
lastFMGrouped <- group_by(lastFM2, artist, monthID, dateMY) %>%
  summarise(count = max(count))

# 3.7 Order grouped df chronologically
lastFMGrouped <- lastFMGrouped[order(lastFMGrouped$monthID),]



# 4. Problem solving ----
# If an artist wasn't played in a month they disappear.
# Need to include their max if this is the case
# e.g. RHCP absent from August 2012 despite being highest overall in Jul/Sep
# add every artist and every month with 0 in count?

# 4.1 Get every artist name and all dates/monthIDs
allBands <- unique(lastFMGrouped$artist)
allMonthIDs <- unique(lastFMGrouped$monthID)
allDates <- unique(lastFMGrouped$dateMY)

# 4.2 Create new df with all artists gaving 0 plays every month
newDF <- data.frame(artist = rep(allBands, 124),            # add every artist 124 times (length(allDates))
                    monthID = rep(allMonthIDs, 3425),       # add every monthID (length(allBands)) times
                    dateMY = rep(allDates, 3425),           # add every date (length(allBands)) times
                    count = rep(0,424700))                  # set count = 0 for all of these

# 4.3 combine this with grouped dataset
allArtistsAllDates <- full_join(lastFMGrouped, newDF)

# 4.4 group by artist and month/date again
allArtistsAllDates <- group_by(allArtistsAllDates, artist, monthID, dateMY, .drop = F) %>%
  summarise(count = max(count))

# 4.5 save t otake into excel
write.csv(allArtistsAllDates, file = "allArtistsAllDates.csv")

# 4.6 load in new csv manually created above/edited in excel
lastFMGrouped2 <- read.csv(file = "cleanedGrouped.csv", stringsAsFactors = F)


# 5. Create table for use in plot ----
# 5.1 Add data to table
# Takes the top 10 most all time played artists each month - adds ranking column
lastFmTable <- lastFMGrouped2 %>%
  group_by(monthID) %>%
  mutate(rank = min_rank(-maxcount) *1) %>%
  filter(rank <= 10) %>%
  ungroup()

#5.2 Reorder by month
lastFmTable <- lastFmTable[order(lastFmTable$monthID),]


# 5.3 Manually remove ties from Dec 13
lastFmTable[465,]
lastFmTable <- lastFmTable[-465,]
lastFmTable[475,]
lastFmTable <- lastFmTable[-475,]

# 5.4 remove weird column at start
lastFmTable <- lastFmTable[,-1]



# 5.5 too many ties at begininng, remove first year
lastFmTable <- subset(lastFmTable, monthID >= 13)

# 6. Animation ----
# BSC, MCR, RHCP, A7x, QOTSA names too long to fit in animation side by side - add line break
lastFmTable$artist <- gsub(pattern = "Queens of the", replacement = "Queens of the \n", lastFmTable$artist)
lastFmTable$artist <- gsub(pattern = "Black Stone Cherry", replacement = "Black Stone \nCherry", lastFmTable$artist)
lastFmTable$artist <- gsub(pattern = "Red Hot ", replacement = "Red Hot \n", lastFmTable$artist)
lastFmTable$artist <- gsub(pattern = "Avenged ", replacement = "Avenged \n", lastFmTable$artist)
lastFmTable$artist <- gsub(pattern = "My Chemical ", replacement = "My Chemical \n", lastFmTable$artist)
lastFmTable$artist <- gsub(pattern = "\n ", replacement = "\n ", lastFmTable$artist)

# 6.2 Add Ordering column - inverse of rank column
# Use ggplot(aes(rank, ...)) for biggest value at left
    # ggplot(aes(ordering, ...)) for biggest value at right
# https://stackoverflow.com/questions/52623722/how-does-gganimate-order-an-ordered-bar-time-series/52652394#52652394 
lastFmTable <- lastFmTable %>%
  group_by(monthID) %>%
  mutate(ordering = min_rank(maxcount) * 1.0) %>%
  ungroup()



# 6.3 plot 
plot <- lastFmTable %>%
  
  ggplot(aes(ordering,
             group = artist)) +
  
  geom_tile(aes(y = maxcount/2,
                height = maxcount,
                width = 0.9,
                fill = artist),
            alpha = 0.9,
            colour = "Black",
            size = 0.75) +
  
  # text on top of bars
  geom_text(aes(y = maxcount, 
                label = artist), 
            vjust = -0.5) +
  
  # text in x-axis (requires clip = "off" in coord_cartesian)
  geom_text(aes(y = 0,
                label = artist),
            vjust = 1.1) +
  
  geom_label(aes(x=1.5,
                 y=4300,
                 label=paste0(date)),
             size=8,
             color = 'black') +
  
  coord_cartesian(clip = "off",
                  expand = FALSE) +
  
  labs(title = 'My most played artist over time.',
       subtitle = "My top lastFM artists from January 2011 to April 2020.",
       caption = '\n \n\nsource: lastFM | plot by @statnamara',
       x = '',
       y = '') +
  
  
  theme(
    plot.title = element_text(size = 18),
    axis.ticks.x = element_blank(),
    axis.text.x  = element_blank(),
    legend.position = "none",
  ) +    
  
  transition_states(monthID,
                    transition_length = 20,
                    state_length = 2,
                    wrap = F) +
  
  ease_aes('cubic-in-out')


animate(plot,
        duration = 48,
        fps = 20,
        detail = 30,
        width = 1000,
        height = 700,
        end_pause = 90)

anim_save(filename = "lastFM.gif", animation = last_animation())
