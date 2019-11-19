
###########################################
# Kai Kresek
# 4/9/2019
# Change Point Classification
###########################################

#_________________________________#
#           Overview              #
#_________________________________#

# This script takes a matrix time series, calculates change points,
# sums these change points into positive and negative changes, and
# categorizes them based on these sums.

# Primary use case is for classifying tree cover loss and gain cycles using
# tree cover height and canopy cover for the mekong region.

# Raster data was converted into a matrix where 1 indicates tree cover height
# greater than 5m and tree canopy cover greater than 30% in a 30x30m pixel. 
# 0 indicates no tree cover.

#_________________________________#
#           Settings              #
#_________________________________#

# Set working directory
setwd("C:/wd")
getwd()

# Read in the data
fname <- read.csv(file = 'fname.csv')
head(fname)


# Convert to matrix
my_matrix <- data.matrix(fname)
head(my_matrix)

#_________________________________#
#    Counting Change Points       #  
#     Step One: Sum (-1) & 1      #
#_________________________________#

# Here we're comparing columns to see when there are changes from 1 --> 0 or 0 --> 1
matrix_diffs <-my_matrix[, -1, drop = FALSE] - my_matrix[, -ncol(my_matrix), drop = FALSE]  
df_diffs <- as.data.frame(my_matrix)

# Writing to a .csv to reference later.
write.csv(df_diffs, file = "col_diffs_matrix.csv" )

# Now we're going to look for permanent loss and gain, or identify churn. 
# We will sum all of the (-1), and the (1).
# If there is only one (-1), then permanent loss
# If there is only one (1), then permanent gain
# If there is a gain and a loss, then?
# If there is a two gain or two loss, then?
# If there is a combo of three, then churn. 

# Operating with a dataframe because rowSums doesn't work properly on matrices.

my_diffs <- df_diffs
head(my_diffs)

# Identifying the number of (-1) and adding a column
my_diffs$loss_count <- rowSums(my_diffs[c("tcc_2001", "tcc_2002", "tcc_2003", "tcc_2004", "tcc_2005", "tcc_2006", "tcc_2007",
                                            "tcc_2008", "tcc_2009", "tcc_2010", "tcc_2011", "tcc_2012", "tcc_2013", "tcc_2014",
                                            "tcc_2015", "tcc_2016", "tcc_2017")] == -1)


# Identifying the number of (1) and adding a column
my_diffs$gain_count <- rowSums(my_diffs[c("tcc_2001", "tcc_2002", "tcc_2003", "tcc_2004", "tcc_2005", "tcc_2006", "tcc_2007",
                                            "tcc_2008", "tcc_2009", "tcc_2010", "tcc_2011", "tcc_2012", "tcc_2013", "tcc_2014",
                                            "tcc_2015", "tcc_2016", "tcc_2017")] == 1)

# Checking in on the data
head(my_diffs)
write.csv(my_diffs, file = "my_diffs_qa.csv")

#_________________________________#
#    Counting Change Points       #  
# Step Two: Sum the Abs Values    #
#_________________________________#

# Now we add the loss_count and gain_count to get change_count.
my_diffs$change_count <- rowSums(my_diffs[c("loss_count", "gain_count")])

# Checking in on the data again.
head(my_diffs)

# Make a csv to preserve the version
write.csv(my_diffs, file = "my_diffs.csv" )

# Adding a column for these assignments.
my_diffs$tcc_category <- NA

#_________________________________#
#       Categorizing              #
#_________________________________#

# The logic is basically like:
# (A) if gain count = 1 and loss count = 0, then permanent gain
# (B) If loss count = 1 and gain count = 0, then permanent loss
# (C) If gain + loss = 2 with loss taking place after gain, then permanent loss
# (D) If gain + loss = 2 with loss taking place before gain then permanent gain
# (C&D Alternate) If gain + loss = 2 then uncategorized
# (E) If gain+loss is greater than 2 = churn 

# This uses C&D alternate  

my_diffs$tcc_category = ifelse(my_diffs$gain_count == 1 & my_diffs$loss_count == 0, "permanent gain",
                         ifelse(my_diffs$gain_count == 0 & my_diffs$loss_count == 1, "permanent loss",
                         ifelse(my_diffs$gain_count + my_diffs$loss_count == 2, "uncategorized",
                         ifelse(my_diffs$gain_count + my_diffs$loss_count > 2, "churn",
                         ifelse(my_diffs$gain_count + my_diffs$loss_count == 0, "stable", NA)))))

write.csv(my_diffs, file = "my_diffs_categorized.csv" )

# Now I'm going to try to use indexing to put our uncategorized data into categories of permanent gain or loss. 

# Subset the tcc_diffs data to get only the uncategorized data.
my_diffs_uncategorized <- subset(my_diffs, tcc_category == "uncategorized", select = c("tcc_2001", "tcc_2002", "tcc_2003", "tcc_2004", "tcc_2005", "tcc_2006", "tcc_2007",
                                                                           "tcc_2008", "tcc_2009", "tcc_2010", "tcc_2011", "tcc_2012", "tcc_2013", "tcc_2014",
                                                                           "tcc_2015", "tcc_2016", "tcc_2017"))

# Now we find index of -1 and 1

# Then we compare the index. If index of -1 is greater than index of 1, then permanent loss.
# If index of 1 is greater than index of -1, than permanent gain. There's probably a more 
# elegant way to do this, but would need to spend more time researching. 


my_diffs_uncategorized$loss_pos <- apply(my_diffs_uncategorized, 1, function(x) match(-1, x[1:17], nomatch = 99))
my_diffs_uncategorized$gain_pos <- apply(my_diffs_uncategorized, 1, function(x) match(1, x[1:17], nomatch = 99))

# Check answers by writing to csv

write.csv(my_diffs_uncategorized, file = "my_diffs_uncategorized_qa.csv" )

# Now compare loss_pos and gain_pos to assign values.
my_diffs_uncategorized$tcc_category = ifelse(my_diffs_uncategorized$loss_pos > my_diffs_uncategorized$gain_pos, "permanent loss", "permanent gain")
 
# Check answers again
write.csv(tcc_uncat, file = "my_diffs_uncategorized_qa2.csv" )  

#_________________________________#
#       Did some stuff            #
#       in Excel because          #
#       it was faster             #
#_________________________________#

# The steps I did in excel was assign the uncategorized data the new categoreis based on index of loss and gain years.
# I also assigned "stable forest" and "stable non forest" since the method didn't distinguish between the two.

#_________________________________#
#         Same Analysis           # 
#     But Last Year Status is     #
#        Incorporated into        #
#         Categorization          #
#_________________________________#


# We may want to assign pixels as "permanent loss" if they lost in the most recent year and same. 

# Here we have to add another category where we assign permanent loss and permanent gain based on 2017 value. 
# One concern I had was that it would assign stable forest to permanent gain, but it doesn't look like that 
# happened, due to order of the ifelse statement.

my_diffs_lastyear <- my_diffs
my_diffs_lastyear $tcc_category = ifelse(tcc_diffs$gain_count == 1 & my_diffs_lastyear$loss_count == 0, "permanent gain",
                                ifelse(my_diffs_lastyear$gain_count == 0 & my_diffs_lastyear$loss_count == 1, "permanent loss",
                                ifelse(my_diffs_lastyear$tcc_2017 == -1, "permanent loss",
                                ifelse(my_diffs_lastyear$tcc_2017 == 1, "permanent gain",
                                ifelse(my_diffs_lastyear$gain_count + my_diffs_lastyear$loss_count == 2, "uncategorized",
                                ifelse(my_diffs_lastyear$gain_count + my_diffs_lastyear$loss_count > 2, "churn",
                                ifelse(my_diffs_lastyear$gain_count + my_diffs_lastyear$loss_count == 0, "stable", NA)))))))

# Saving to csv to check if code is doing the right thing.
write.csv(my_diffs_lastyear , file = "my_diffs_lastyear_qa.csv" ) 

# Subsetting the uncategorized data again.
my_diffs_lastyear <- subset(my_diffs_lastyear , tcc_category == "uncategorized", select = c("tcc_2001", "tcc_2002", "tcc_2003", "tcc_2004", "tcc_2005", "tcc_2006", "tcc_2007",
                                                                           "tcc_2008", "tcc_2009", "tcc_2010", "tcc_2011", "tcc_2012", "tcc_2013", "tcc_2014",
                                                                           "tcc_2015", "tcc_2016", "tcc_2017"))

# Getting the index of gain and loss 
my_diffs_lastyear $loss_pos <- apply(my_diffs_lastyear , 1, function(x) match(-1, x[1:17], nomatch = 99))
my_diffs_lastyear $gain_pos <- apply(my_diffs_lastyear , 1, function(x) match(1, x[1:17], nomatch = 99))

# Assigning categories based on indexing
my_diffs_lastyear $tcc_category = ifelse(my_diffs_lastyear $loss_pos > my_diffs_lastyear $gain_pos, "permanent loss", "permanent gain")

# Saving to csv again.
write.csv(my_diffs_lastyear , file = "tcc_uncat_check6.csv" )  

