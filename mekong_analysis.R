
###########################################
#Kai Kresek
#4/9/2019
#Mekong Classification (Take 2)
###########################################

#_________________________________#
#           Overview              #
#_________________________________#

#We're looking at tree cover loss patterns over time, paired with gain
#for the Mekong baseline layer. The goal is to verify
#patterns of loss and gain that we see in Mekong data.

#_________________________________#
#           Settings              #
#_________________________________#

# Set working directory (better to do manually I think, or set to file directory)
setwd("C:/Users/kai.kresek/OneDrive - World Resources Institute/Desktop/mekong_data_manip")
getwd()

# Here we are reading in the csv, tcc_gain, which contains the data in a data frame. 
tcc_gain <- read.csv(file = 'tcc_gain_data.csv')
head(tcc_gain)


# Here we are converting the data frame into a matrix so we can do matrix operations on it.
matrix_tcc <- data.matrix(tcc_gain)
head(matrix_tcc)

#_________________________________#
#           Testing               #
#_________________________________#

# Here we're testing some matrix opeations.

# log test
logs <- log(matrix_tcc)
head(logs)

# diffs test - problem is that these compare rows not columns.
diffs_matrix <- diff(matrix_tcc, lag = 1)
head(diffs_matrix)
diffs_matrix_df <- as.data.frame(diffs_matrix)
write.csv(diffs_matrix_df, file = "diffs_matrix.csv")

#_________________________________#
#    Counting Change Points       #  
#     Step One: Sum (-1) & 1      #
#_________________________________#

# Here we're comparing columns. This worked!
col_diffs_matrix <-matrix_tcc[, -1, drop = FALSE] - matrix_tcc[, -ncol(matrix_tcc), drop = FALSE]  
head(col_diffs_matrix)
col_diffs_df <- as.data.frame(col_diffs_matrix)
write.csv(col_diffs_df, file = "col_diffs_matrix.csv" )

# Now we're going to look for permanent loss and gain, or identify churn. 
# We will sum all of the (-1), and the (1).
# Ideas (not final):
# If there is only one (-1), then permanent loss
# If there is only one (1), then permanent gain
# If there is a gain and a loss, then?
# If there is a two gain or two loss, then?
# If there is a combo of three, then churn. 

# We are giving our data a better naming scheme because it was getting confusing.
# Official data set for the rest of the analysis is tcc_diffs.
# Operating with a dataframe because rowSums doesn't work properly on matrices.
# Also, it looks like the previous calculation dropped the first column. This doesn't matter right?

tcc_diffs <- col_diffs_df
head(tcc_diffs)

# Identifying the number of (-1) and adding a column
tcc_diffs$loss_count <- rowSums(tcc_diffs[c("tcc_2001", "tcc_2002", "tcc_2003", "tcc_2004", "tcc_2005", "tcc_2006", "tcc_2007",
                                            "tcc_2008", "tcc_2009", "tcc_2010", "tcc_2011", "tcc_2012", "tcc_2013", "tcc_2014",
                                            "tcc_2015", "tcc_2016", "tcc_2017")] == -1)


# Identifying the number of (1) and adding a column
tcc_diffs$gain_count <- rowSums(tcc_diffs[c("tcc_2001", "tcc_2002", "tcc_2003", "tcc_2004", "tcc_2005", "tcc_2006", "tcc_2007",
                                            "tcc_2008", "tcc_2009", "tcc_2010", "tcc_2011", "tcc_2012", "tcc_2013", "tcc_2014",
                                            "tcc_2015", "tcc_2016", "tcc_2017")] == 1)

# Checking in on the data
head(tcc_diffs)
write.csv(tcc_diffs, file = "tcc_diffs_debug.csv")

#_________________________________#
#    Counting Change Points       #  
# Step Two: Sum the Abs Values    #
#_________________________________#

# Now we add the loss_count and gain_count to get change_count.
tcc_diffs$change_count <- rowSums(tcc_diffs[c("loss_count", "gain_count")])

# Checking in on the data again! Looks good to me.
head(tcc_diffs)

# Make a csv to preserve the data.
write.csv(tcc_diffs, file = "tcc_diffs.csv" )

# Now I'm going to try to assign variable names based on all of these counts.

# Adding a column for these assignments
tcc_diffs$tcc_category <- NA

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

# This uses C&D alternate until I can figure out how to find indexes in data frames. 
# I'm thinking I'll need to go back and index while the data is still a matrix

tcc_diffs$tcc_category = ifelse(tcc_diffs$gain_count == 1 & tcc_diffs$loss_count == 0, "permanent gain",
                         ifelse(tcc_diffs$gain_count == 0 & tcc_diffs$loss_count == 1, "permanent loss",
                         ifelse(tcc_diffs$gain_count + tcc_diffs$loss_count == 2, "uncategorized",
                         ifelse(tcc_diffs$gain_count + tcc_diffs$loss_count > 2, "churn",
                         ifelse(tcc_diffs$gain_count + tcc_diffs$loss_count == 0, "stable", NA)))))

tcc_diffs

write.csv(tcc_diffs, file = "tcc_diffs_with_cats.csv" )

# Calculated statistics for each category in excel because it was faster.

# Now I'm going to try to use indexing to put our uncategorized data into categories of permanent gain or loss. 

# Subset the tcc_diffs data
tcc_uncat <- subset(tcc_diffs, tcc_category == "uncategorized", select = c("tcc_2001", "tcc_2002", "tcc_2003", "tcc_2004", "tcc_2005", "tcc_2006", "tcc_2007",
                                                                           "tcc_2008", "tcc_2009", "tcc_2010", "tcc_2011", "tcc_2012", "tcc_2013", "tcc_2014",
                                                                           "tcc_2015", "tcc_2016", "tcc_2017"))
# Now we find index of -1 and 1

# Then we compare the index. If index of -1 is greater than index of 1, then permanent loss.
# If index of 1 is greater than index of -1, than permanent gain. There's probably a more 
# elegant way to do this, but would need to spend more time researching. 


tcc_uncat$loss_pos <- apply(tcc_uncat, 1, function(x) match(-1, x[1:17], nomatch = 99))
tcc_uncat$gain_pos <- apply(tcc_uncat, 1, function(x) match(1, x[1:17], nomatch = 99))
tcc_uncat

# Check answers by writing to csv and then checking!

write.csv(tcc_uncat, file = "tcc_uncat_check3.csv" )

# Now compare loss_pos and gain_pos to assign values.
tcc_uncat$tcc_category = ifelse(tcc_uncat$loss_pos > tcc_uncat$gain_pos, "permanent loss", "permanent gain")
 
# Check answers again
write.csv(tcc_uncat, file = "tcc_uncat_check7.csv" )  

#_________________________________#
#       Did some stuff            #
#       in Excel because          #
#       it was faster             #
#_________________________________#

# The steps I did in excel was assign the uncategorized data the new categoreis based on index of loss and gain years.
# I also assigned "stable forest" and "stable non forest" since the method didn't distinguish between the two.
# I also ran the final counts and percentages in excel using a pivot table. 


#_________________________________#
#         Same Analysis           # 
#     But Last Year Status is     #
#        Incorporated into        #
#         Categorization          #
#_________________________________#


# One thing that I forgot about was that we want to assign pixels "permanent loss" if they lost in the most recent year and same. 


# Here we have to add another category where we assign permanent loss and permanent gain based on 2017 value. 
# One concern I had was that it would assign stable forest to permanent gain, but it doesn't look like that 
# happened, due to order of the ifelse statement. Also checked totals in excel, looks ok. 

tcc_diffs_2 <- tcc_diffs
tcc_diffs_2$tcc_category = ifelse(tcc_diffs$gain_count == 1 & tcc_diffs_2$loss_count == 0, "permanent gain",
                                ifelse(tcc_diffs_2$gain_count == 0 & tcc_diffs_2$loss_count == 1, "permanent loss",
                                ifelse(tcc_diffs_2$tcc_2017 == -1, "permanent loss",
                                ifelse(tcc_diffs_2$tcc_2017 == 1, "permanent gain",
                                ifelse(tcc_diffs_2$gain_count + tcc_diffs_2$loss_count == 2, "uncategorized",
                                ifelse(tcc_diffs_2$gain_count + tcc_diffs_2$loss_count > 2, "churn",
                                ifelse(tcc_diffs_2$gain_count + tcc_diffs_2$loss_count == 0, "stable", NA)))))))

# Saving to csv to check if code is doing the right thing.
write.csv(tcc_diffs_2, file = "tcc_diffs_2.csv" ) 

# Subsetting the uncategorized data again.
tcc_uncat_2 <- subset(tcc_diffs_2, tcc_category == "uncategorized", select = c("tcc_2001", "tcc_2002", "tcc_2003", "tcc_2004", "tcc_2005", "tcc_2006", "tcc_2007",
                                                                           "tcc_2008", "tcc_2009", "tcc_2010", "tcc_2011", "tcc_2012", "tcc_2013", "tcc_2014",
                                                                           "tcc_2015", "tcc_2016", "tcc_2017"))

# Getting the index of gain and loss 
tcc_uncat_2$loss_pos <- apply(tcc_uncat_2, 1, function(x) match(-1, x[1:17], nomatch = 99))
tcc_uncat_2$gain_pos <- apply(tcc_uncat_2, 1, function(x) match(1, x[1:17], nomatch = 99))

# Assigning categories based on indexing
tcc_uncat_2$tcc_category = ifelse(tcc_uncat_2$loss_pos > tcc_uncat_2$gain_pos, "permanent loss", "permanent gain")

# Saving to csv again.
write.csv(tcc_uncat_2, file = "tcc_uncat_check6.csv" )  

#_________________________________#
#       Did some stuff            #
#       in Excel because          #
#       it was faster             #
#_________________________________#

# The steps I did in excel was assign the uncategorized data the new categories based on index of loss and gain years (just
# a manual copy and paste).
# I also assigned "stable forest" and "stable non forest" since the method didn't distinguish between the two.
# I also ran the final counts and percentages in excel using a pivot table. 

#_________________________________#
#       Calculating Avg           #
#       Number of 0's             #
#       Between Change Points     #
#_________________________________#

# We will first want a subset of the data where -1 goes to 1. Do we want to incorporate all the churns in there too?
# Once we have this subset, we should count indices from where -1 starts to where 1 appears, end count. 
# Then we want to subtract one because we are counting -1 which isn't a growth year. 

# For this analysis, I'm using the tcc_diffs data, subset to filter out where change count >= to 2. 


tcc_zeroes_prelim <- read.csv(file = 'tcc_diffs.csv')
head(tcc_zeroes_prelim)


# Subset to get just rows where change_count == 2 for simplification. Also, we're subsetting in a way that drops the 
# change count from the new data set because it is not relevent for this analysis moving forward. 
tcc_zeroes <- subset(tcc_zeroes_prelim, change_count == 2, select = c("tcc_2001", "tcc_2002", "tcc_2003", "tcc_2004", "tcc_2005", "tcc_2006", "tcc_2007",
                                                                         "tcc_2008", "tcc_2009", "tcc_2010", "tcc_2011", "tcc_2012", "tcc_2013", "tcc_2014",
                                                                         "tcc_2015", "tcc_2016", "tcc_2017"))
head(tcc_zeroes)
nrow(tcc_zeroes)

#############################
#       METHOD One          #
#############################

# So, the idea is to get get the index of -1 and 1 and then subtract them to get the difference (then subtracting one 
# from this difference because we need to account for the -1 being factored into the subtraction) I will try this! I'm not sure if 
# this accounts for multiple loss intervals. We will see! Also, the problem is that it pulls the index of the first occurrence. 


tcc_zeroes$loss_init <- apply(tcc_zeroes, 1, function(x) match(-1, x[1:17], nomatch = 99)) 
tcc_zeroes$gain <- apply(tcc_zeroes, 1, function(x) match(1, x[1:17], nomatch=99))

write.csv(tcc_zeroes, file = "tcc_zeroes_check2.csv" )

# Just a note, but negative differences will need to be filtered out later, because this indicates when gain happened first
# then a loss, not helpful in this analysis. 

tcc_zeroes$difference <- apply(tcc_zeroes, 1, function(x) (tcc_zeroes$gain - tcc_zeroes$loss_init)-1)
write.csv(tcc_zeroes, file = "tcc_zeroes_check3.csv" )

head(tcc_zeroes)

# This *kind* of worked. For some reason, apply function is posting a lot of columns. Will need to investigate if I 
# Want to keep going with this method.
# Looked at csv file (tcc_zeroes_check3), there were 136 pixel values that fell into our category of
# having a change count == 2, and a (gain - loss_init) that was positive. 
# Mean = 5 and Mode = 0, n = 136

# This method works alright, but there a couple problems with it - not very versatile. Will try another method. 

#############################
#       METHOD TWO          #
#############################

# I'm going to try to use a function instead! I think I'll be able to add conditionals that can sort out 
# some of the issues that we're having here. 

# First convert to matrix!
matrix_zeroes <- data.matrix(tcc_zeroes)
head(matrix_zeroes)

# Now we have a preliminary function.

get_interval = function(x){
  init = match(-1, x[-1])
  interval = match(1, x[-(1:(init+1))]) - 1
  return(interval)}

# Tried this! Doesn't work properly either.


#############################
#       METHOD Three        #
#############################


# *THIS WORKED! VERY TIME INTENSIVE THOUGH*
# Subsetting in excel
# Divided up the data by change counts.
# ColCount Macro
# Get index of every -1 and 1 in the data.
# Subtract higher index from lower index across rows?
# Get summary statistics


#############################
#       METHOD Four         #
#############################

# Use which? 

