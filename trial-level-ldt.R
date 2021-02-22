library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# function to prefix the year with "19" or "20" if only (and only if only) single or double digit
# (2004 is the max possible date for the ELP data)
prefix_year <- function(y) {
  y <- as.numeric(y)
  y <- ifelse(
    nchar(y) %in% c(1, 2),
    ifelse(y<4, as.numeric(sprintf("20%02d", y)), as.numeric(sprintf("19%02d", y))),
    y
  )
  as.character(y)
}

# hacky function to handle the umpteen hundred ways people seem to write dates
# returns object of class date
read_elp_date <- function(vec) {
  vec <- as.character(vec)
  
  dmy_str <- sapply(vec, function(x) {
    # cat(sprintf("%s\n", x))
    if (is.na(x)) return(NA)
    
    # remove alphabetic characters
    x <- gsub("[a-z]", "", x)
    
    # less than 4 or more than 9 characters is completely ambiguous
    if (nchar(x)<4 | nchar(gsub("[^0-9]", "", x))>9) return(NA)
    
    # get separators (if any)
    sep_chars <- strsplit(x, "[0-9]")[[1]]
    sep_chars <- sep_chars[sep_chars!=""]
    
    # for each, I assume month, day, year as original order as this seems most common in these subjects
    # obvious exceptions (e.g. when month>12) are handled
    if (length(sep_chars) == 2) {
      
      sep_chars_regex <- ifelse(sep_chars=="\\", "\\\\", sep_chars)
      vals <- strsplit(x, sprintf("%s|%s", sep_chars_regex[1], sep_chars_regex[2]))[[1]]
      nums <- as.numeric(vals[grepl("^[0-9]+$", vals)])
      
    } else if (length(sep_chars) > 2) {
      stop("More than two date separators!")
    } else if (length(sep_chars) == 1) {
      stop("One one date separator!")
    } else {
      
      vals <- strsplit(x, "", fixed=TRUE)[[1]]
      
      # keep in original order (usually m/d/y)
      # if odd number coerce into likely m/d/y, assuming the year is always 2 or 4 characters
      nums <- if (nchar(x)==6) {
        c(paste(vals[1:2], collapse=""), paste(vals[3:4], collapse=""), paste(vals[5:6], collapse=""))
      } else if (nchar(x)==8) {
        c(paste(vals[1:2], collapse=""), paste(vals[3:4], collapse=""), paste(vals[5:8], collapse=""))
      } else if (nchar(x)==4) {
        c(vals[1], vals[2], paste(vals[3:4], collapse=""))
      } else if (nchar(x)==5) {
        if (as.numeric(paste(vals[2:3], collapse=""))>12) {
          c(paste(vals[1], collapse=""), paste(vals[2:3], collapse=""), paste(vals[4:5], collapse=""))
        } else if (as.numeric(paste(vals[1:2], collapse=""))>31) {
          c(paste(vals[1], collapse=""), paste(vals[2:3], collapse=""), paste(vals[4:5], collapse=""))
        } else {
          c(paste(vals[1:2], collapse=""), paste(vals[3], collapse=""), paste(vals[4:5], collapse=""))
        }
      } else if (nchar(x)==7) {
        if (as.numeric(paste(vals[2:3], collapse=""))>12) {
          c(paste(vals[1], collapse=""), paste(vals[2:3], collapse=""), paste(vals[4:7], collapse=""))
        } else if (as.numeric(paste(vals[1:2], collapse=""))>31) {
          c(paste(vals[1], collapse=""), paste(vals[2:3], collapse=""), paste(vals[4:7], collapse=""))
        } else {
          c(paste(vals[1:2], collapse=""), paste(vals[3], collapse=""), paste(vals[4:7], collapse=""))
        }
      }
      
      nums <- as.numeric(nums)
      
    }
    
    if (nums[1] > 12 & nums[2] > 12) return(NA)
    if (any(nums==0)) return(NA)
    
    if (nums[1] > 12) {
      sprintf("%02d/%02d/%s", nums[1], nums[2], prefix_year(nums[3]))
    } else {
      sprintf("%02d/%02d/%s", nums[2], nums[1], prefix_year(nums[3]))
    }
  })
  
  if (all(is.na(dmy_str))) return(rep(NA, length(vec)))
  
  parse_date(dmy_str, "%d/%m/%Y")
}

# function to read a .LDT file
read_ldt <- function(file) {
  # cat(sprintf("Reading %s\n", basename(file)))
  file_dat <- readLines(file, n=-1, warn=FALSE)
  
  # get the subject information from the bottom within the equals sign markers
  sess_info_start_end <- sapply(
    file_dat,
    # the markers should be a line of just '=' characters
    function(line_i) grepl("^(=)\\1*$", line_i),
    USE.NAMES=FALSE
  ) %>%
    which()
  
  if (length(sess_info_start_end) > 2) {
    warning(sprintf("%s markers for session info found in %s (expected 2) - will only use first 2", length(sess_info_start_end), basename(file)))
    sess_info_start_end <- sess_info_start_end[1:2]
  }
  
  if (length(sess_info_start_end) == 1) {
    warning(sprintf("1 marker for session info found in %s (expected 2) - will use end of file as 2nd marker", basename(file)))
    sess_info_start_end <- c(sess_info_start_end, length(file_dat))
  } else {
    if (diff(sess_info_start_end) != 9) warning(sprintf("Unexpected length of session info in %s", basename(file)))
  }
  
  if (length(sess_info_start_end) == 0) stop(sprintf("No markers for session info found in %s", basename(file)))
  
  
  demog <- file_dat[(sess_info_start_end[1]+1) : (sess_info_start_end[1]+2)] %>%
    read_csv(
      col_types = list(col_character(), col_character(), col_character(), col_double(), col_character(), col_character())
    ) %>%
    mutate(
      Time = parse_time(if_else(
        length(strsplit(Time, split=":", fixed=TRUE)[[1]]) == 2,
        sprintf("%s:00", Time),
        Time
      ), format="%H:%M:%S")
    ) %>%
    rename(Time_Demog = Time, Date_Demog = Date)
  
  shipley_dat <- file_dat[(sess_info_start_end[1]+4) : (sess_info_start_end[1]+5)] %>%
    read_csv(col_types = "ddddd") %>%
    rename_all(function(x) sprintf("Shipley_%s", x))
  
  health_info <- file_dat[(sess_info_start_end[1]+7) : (sess_info_start_end[1]+8)] %>%
    read_csv(
      col_types = list(col_integer(), col_integer(), col_integer(), col_integer(), col_character())
    )
  
  # join subject and session info into a single dataframe, info
  info <- bind_cols(
    select(demog, -Subject),
    shipley_dat,
    health_info
  )
  if (nrow(info)>1) warning(sprintf("More than one line in info for %s", basename(file)))
  
  # find the start and end of each session
  sess_starts <- which(grepl("^Univ.*$", file_dat))
  
  sess_ends <- if (length(sess_starts) > 1) {
    c(sess_starts[2:length(sess_starts)]-1, sess_info_start_end[1]-1)
  } else {
    sess_info_start_end[1]-1
  }
  
  sess_starts_ends <- lapply(1:length(sess_starts), function(i) c(start=sess_starts[i], end=sess_ends[i], session_nr=i))
  
  # get actual trials from each session
  trial_dat <- map_df(sess_starts_ends, function(start_end) {
    # get session info as character vector
    sess_info <- file_dat[start_end[["start"]] : (start_end[["start"]]+1)] %>%
      read_csv(
        col_types = list(col_integer(), col_character(), col_date("%m-%d-%Y"), col_character(), col_character(), col_integer())
      ) %>%
      mutate(
        Time = parse_time(if_else(
          length(strsplit(Time, split=":", fixed=TRUE)[[1]]) == 2,
          sprintf("%s:00", Time),
          Time
        ), format="%H:%M:%S"),
        DOB = read_elp_date(DOB)
      )
    # get trial data as character vector
    trial_char <- file_dat[(start_end[["start"]]+2) : start_end[["end"]]]
    # handle weird cases
    if (all(sess_info$Subject==756)) {
      trial_char[trial_char=="1744,35819,0,1,91745,23822,0,1,1278,nondryind"] <- "1745,23822,0,1,1278,nondryind"
    }
    # get trial data as dataframe
    trial_dat <- trial_char %>%
      read_csv(
        skip_empty_rows = TRUE,
        col_names = c("Trial_Order", "Item_Serial_Number", "Lexicality", "Accuracy", "LDT_RT", "Item"),
        col_types = list(col_integer(), col_integer(), col_integer(), col_integer(), col_double(), col_character())
      )
    if (nrow(trial_dat) == 0) return(NULL)
    # return result
    bind_cols(sess_info, trial_dat) %>%
      mutate(Session_nr = start_end[["session_nr"]])
  }) %>%
    rename(Orig_Subject = Subject) %>%
    # record university name (based on OSF wiki)
    mutate(
      Univ_Name = recode(
        Univ,
        `1` = "Morehead State University",
        `2` = "SUNY-Albany",
        `3` = "University of Kansas",
        `4` = "University of South Florida",
        `5` = "Washington University",
        `6` = "Wayne State University"
      ),
      "Lexicality_Label" = if_else(Lexicality==1, "w", "nw"),
      "Subject_ID" = paste(Univ, Orig_Subject, sep="_")
    )
  # return the joined data
  bind_cols(trial_dat, info) %>%
    mutate(file = basename(file)) %>%
    select(
      Univ, Univ_Name, Date, Time, Orig_Subject, Subject_ID, DOB, Education, Trial_Order, Item_Serial_Number, Lexicality, Lexicality_Label, Accuracy, LDT_RT, Item, Session_nr, Gender, Task, Date_Demog, Time_Demog, MEQ, Shipley_numCorrect, Shipley_rawScore, Shipley_vocabAge, Shipley_shipTime, Shipley_readTime, presHealth, pastHealth, vision, hearing, firstLang, file
    )
}

# download and unzip data
temp_zip <- tempfile()
temp_unzip <- tempfile()
download.file("https://osf.io/eu5ca/download", temp_zip, mode="wb")
unzip(zipfile = temp_zip, exdir = temp_unzip)

# import all files and read as tidy data
elp <- list.files(temp_unzip, full.names = TRUE) %>%
  map_df(read_ldt)

# save the result locally
write_csv(elp, "elp.csv")
