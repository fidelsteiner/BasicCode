################################################################################
# fixDate - fix switched date columns in csv files
# 
# fixDate.R
# Fix mixed YYYY-MM-DD and YYYY-DD-MM date column ins station data (probably not completely fail proof method!)
#
# Author: Philip Kraaijenbrink
# Created:          2020/05/05
# Latest Revision:  2020/05/05
#
# mountainhydrology group UU |  www.mountainhydrology.org 
################################################################################
# clear entire workspace (excl. packages)
rm(list = ls())
gc()

# define &-sign for pasting string-elements
'&' <- function(...) UseMethod('&')
'&.default' <- .Primitive('&')
'&.character' <- function(...) paste(...,sep='')

fn                     <- './data/Kyanjing_ICIMOD.csv'       # fill path to csv
first_date_row_wrong   <- F                                  # is the first date row already wrong, check manually?

# read data
meteo                  <- read.csv(fn, stringsAsFactors=F)  

# construct date column of individual days
d   <- data.frame(date=as.Date(rle(meteo$DATE)$values))
d$y <- as.integer(format(d$date, '%Y'))          
d$m <- as.integer(format(d$date, '%m'))  
d$d <- as.integer(format(d$date, '%d'))

# identify wrong positions and correct the date
d$wrongdate <- c(first_date_row_wrong, ((diff(d$date) > 1) & (diff(d$m) == 1)) | ((diff(d$date) < 1) & (d[-1,]$d != 13)))
d$newdate   <- as.Date(ifelse(d$wrongdate, as.Date(as.character(d$date), format='%Y-%d-%m'), d$date), origin='1970-01-01')

# check if sorted new date equals date, i.e. all dates are sequentially and (probably) formatted correctly
cat('Newdates sort properly?',!sum(sort(d$newdate)!=d$newdate),'\n')

# add the new dates to the original meteo data
meteo$DATE <- format(d$newdate[rep(1:nrow(d),rle(meteo$DATE)$length)],'%Y-%m-%d')

# write corrected output
write.csv(meteo, file.path('./fixed-data',paste0(tools::file_path_sans_ext(basename(fn)),'_corrected.csv')), row.names=F, quote=F)
