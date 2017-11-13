require(dplyr)

###############
# Environment #
###############

# Set working directory to UCI HAR Dataset
# This for reference only -
# Please paste directory for testing

# setwd("/coursera/data_cleaning_project/UCI HAR Dataset")

#####################
# Utility Functions #
#####################

# Get file path under directory into data frame
# e.g. f="subject", d="train"
# returns path to subject_train.txt under train folder
uci.path <- function(file, dir) {
  paste(getwd(),"/",dir,"/",file,"_",dir,".txt",sep="")
}

# Helper load method for data frame
# Loads by do.call with vector argument
# e.g. c("subject", "train")
# Loads data frame from subject_train.txt
uci.load <- function(x) { read.table(do.call("uci.path",as.list(x))) }

#######################
# Data Initialization #
#######################

# Step 1.

# Create directory-file permutations
uci.files <- c("X", "y", "subject")
uci.dirs <- c("train", "test")
uci.fd <- expand.grid(uci.files, uci.dirs)

colnames(uci.fd) <- c("file", "dir")

# Load file under each directory/file
# Combine training & testing data into
# X, Y, subject data frames
uci.X <- bind_rows(apply(filter(uci.fd, file == "X"), 1, uci.load))
uci.Y <- bind_rows(apply(filter(uci.fd, file == "y"), 1, uci.load))
uci.subject <- bind_rows(apply(filter(uci.fd, file == "subject"), 1, uci.load))

# Load features from table with column names
uci.ft <- read.table("features.txt", col.names = c("id", "type"))
# Load activity labels from table with column names
uci.acty <- read.table("activity_labels.txt", col.names = c("id", "activity"))

#################
# Data Cleaning #
#################

# Step 2
# Grep column indices for means/standard deviations
uci.cols <- which(grepl(".+\\-((std)|(mean))\\(\\).*",uci.ft$type))
# Filter out columns which are not means/std. dev. from data frame
# Convert to numeric
uci.X <- uci.X[,uci.cols]

# Step 3
# Assign column names to Y and subject
colnames(uci.Y) <- "id"
colnames(uci.subject) <- "subject"
# Join Y and activity labels
uci.Y <- uci.Y %>% inner_join(uci.acty, "id") %>% select(activity)

# Step 4
# Assign column names according to features
colnames(uci.X) <- uci.ft$type[uci.cols]

# Merge tables & clear unused memory
uci.df <- cbind(uci.subject, uci.Y, uci.X)
rm(uci.X, uci.Y, uci.subject)
rm(uci.cols, uci.ft, uci.acty)
rm(uci.files, uci.dirs, uci.fd)

# Create table from data frame
uci.df <- tbl_df(uci.df)

# This is the output of Step 4
# Un-comment to view
# View(uci.df)

#########################
# Calculate & Tidy Data #
#########################

require(tidyr)

# Separate each variable from mean/std. dev. and direction
uci.output <- uci.df %>%
  # Group by subject+activity and average measurement
  group_by(subject, activity) %>%
  summarise_all(mean) %>%
  # convert measurement columns into rows
  gather(measure, average, -(subject:activity)) %>%
  # Find all magnitude measurements and convert to
  # the same format as XYZ directional measurements
  # e.g. tBodyAccJerkMag -> tBodyAccJerk-mean()-M
  mutate(measure = gsub("AccJerk","Jerk",gsub("(Body|Gravity)(.+)","\\1-\\2",gsub("(.+)Mag(\\-.+)","\\1\\2-Mag", measure)))) %>%
  # Seprate measurements into different types of signals
  separate(measure, c("type", "signal", "measure", "dim")) %>%
  separate(type, c("domain", "type"), sep = 1) %>%
  spread(measure, average)

# uci.output is the final result
# to view please run below View script
uci.output

# View(uci.output)
