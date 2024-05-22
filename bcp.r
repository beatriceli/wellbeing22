library(bcp)
library(arrow)
library(dplyr)


# read in all the feather files from the feather_exports directory
feather_dir <- "feather_exports"
# files are in subdirectories of feather_dir: merged_nobcp
subdir <- "merged_nobcp"
feather_files <- list.files(file.path(feather_dir, subdir), full.names = TRUE)


# create a list of lists of dataframes, one list for each participant
pid_dates <- list()

for (file in feather_files) {
  df <- arrow::read_feather(file)
  # Parse participant ID and date from the file name
  f <- tools::file_path_sans_ext(basename(file))
  pid <- sub("^([^_]+).*", "\\1", f)
  date <- sub("^.*_([^_]+)$", "\\1", f)
  
  if (!(pid %in% names(pid_dates))) {
    pid_dates[[pid]] <- list()
  }
  
  if (!(date %in% names(pid_dates[[pid]]))) {
    pid_dates[[pid]][[date]] <- list()
  }
  
  # Extract only the date component from the datetime column
  df$date <- as.Date(df$date)

  pid_dates[[pid]][[date]] <- do.call(rbind, c(pid_dates[[pid]][[date]], list(df)))
}

# sort by number so convert the day number (name of each frame) to numeric
for (pid in names(pid_dates)) {
  numeric_dates <- as.numeric(names(pid_dates[[pid]]))
  sorted_indices <- order(numeric_dates)
  pid_dates[[pid]] <- pid_dates[[pid]][sorted_indices]
}


# run bcp on steps, co2_smoothed, noise_smoothed, voc_smoothed
# for each participant and each day
# append posterior mean and posterior prob to the dataframe
for (pid in names(pid_dates)) {
  # for each day, run bcp on steps, co2ppm, noisedb, vocppb
  # and append posterior mean and posterior prob to the dataframe
  for (day in names(pid_dates[[pid]])) {
    bcp.stps <- bcp(pid_dates[[pid]][[day]]$steps)
    bcp.co2 <- bcp(pid_dates[[pid]][[day]]$co2_smoothed)
    bcp.noisedb <- bcp(pid_dates[[pid]][[day]]$noise_smoothed)
    bcp.voc <- bcp(pid_dates[[pid]][[day]]$voc_smoothed)
    
    pid_dates[[pid]][[day]]$stps.posterior_mean <- as.vector(bcp.stps$posterior.mean)
    pid_dates[[pid]][[day]]$stps.posterior_prob <- as.vector(bcp.stps$posterior.prob)
    pid_dates[[pid]][[day]]$co2.posterior_mean <- as.vector(bcp.co2$posterior.mean)
    pid_dates[[pid]][[day]]$co2.posterior_prob <- as.vector(bcp.co2$posterior.prob)
    pid_dates[[pid]][[day]]$noise.posterior_mean <- as.vector(bcp.noisedb$posterior.mean)
    pid_dates[[pid]][[day]]$noise.posterior_prob <- as.vector(bcp.noisedb$posterior.prob)
    pid_dates[[pid]][[day]]$voc.posterior_mean <- as.vector(bcp.voc$posterior.mean)
    pid_dates[[pid]][[day]]$voc.posterior_prob <- as.vector(bcp.voc$posterior.prob)
  }
}


# Export results into feather files
# merge all the date frames into one frame for each participant
pids_frames <- list()

for (pid in names(pid_dates)) {
  pids_frames[[pid]] <- do.call(rbind, pid_dates[[pid]])
}

# write each participant's dataframe to a feather file
for (pid in names(pids_frames)) {
  arrow::write_feather(pids_frames[[pid]], paste0("feather_exports/", pid, ".feather"))
}

