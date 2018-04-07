# README ------------------------------------------------------------------

# This script is written by Smogon user P Squared. Send me a VM on the 
# Smogon forums if you have any questions!

# LOAD PACKAGES -----------------------------------------------------------

if(!require(stringr))
{
    install.packages("stringr")
    library(stringr)
}
if(!require(dplyr))
{
    install.packages("dplyr")
    library(dplyr)
}

# DEFINE FUNCTIONS --------------------------------------------------------

# similar to grepl but returns "y" and "" instead of TRUE and FALSE, as
# required by the auction bot
mentions_meta <- function(cell, regex_meta){
    if(grepl(regex_meta, cell)){
        # if text matches the user-specified regular expression
        return("y")
    }else if(grepl("(?<!\\w)(all|any|everything)(?! \\w)", cell, perl = TRUE)){
        # if text is exclusively "all", "any", or "everything"
        return("y")
    }else
        # not a match
        return("")
}

# trims trailing commas arising from users not answering every part of the
# signup format
trim_comma <- function(cell){
    # verify cell has content
    if(!is.na(cell))
        # keep trimming trailing commas until none remain
        while(substr(cell, nchar(cell), nchar(cell)) == ",")
            cell <- substr(cell, 1, nchar(cell) - 1)
    return(cell)
}

# converts scraped thread data into a CSV with added columns for each part
# of the signup format and each metagame in the tournament
process_signups <- function(file_in, info, regex_info, metas, regex_metas){

# UPKEEP ------------------------------------------------------------------
    
    # read input file; remove OP and specific timestamp; set URL
    signups <- read.csv(filepath, stringsAsFactors = FALSE)
    signups <- signups %>% filter(num != 1) %>% select(-time2)
    signups$link <- paste0("https://www.smogon.com", signups$link)
    
    # keep track of numbers of columns
    n_default <- ncol(signups)
    n_info <- length(info)
    n_metas <- length(metas)

# POPULATE NEW COLUMNS ----------------------------------------------------

    # fill info columns
    for(i in 1:n_info){
        # match text with section
        signups[, info[i]] <- str_match(signups$text, regex_info[i])[,2]
        # trim trailing commas
        signups[, info[i]] <- sapply(signups[, info[i]], trim_comma) %>% unname
    }
    
    # fill meta columns
    for(i in 1:n_metas)
    {
        signups[, metas[i]] <- sapply(tolower(signups$tiers), mentions_meta, regex_meta = regex_metas[i]) %>% unname
    }
    
    # keep track of newly created columns
    cols_info <- n_default + 1:n_info
    cols_metas <- (ncol(signups) - n_metas + 1):(ncol(signups))

# IDENTIFY PROBLEMATIC SIGNUPS --------------------------------------------
    
    # posts that may say something like "everything except old gens"
    negations <- c(" but ", " besides ", " except ", " not ", " minus ", 
                   " aside ", " bar ", " excluding ", " save for ", " other than ")
    rows_negated <- rowSums(sapply(negations, grepl, tolower(signups$tiers))) > 0
    # posts with no meta matches
    rows_empty <- apply(signups[,cols_metas], 1, function(x) all(x == ""))
    # posts that quote other posts
    rows_quoted <- grepl(" said:", signups$text)
    
    # overwrite problematic signups with question marks
    signups[(rows_negated | rows_empty | rows_quoted), cols_metas] <- "?"
    signups[rows_quoted, cols_info] <- "?"
    
# REPORT PROBLEMS ---------------------------------------------------------

    # print-friendly list of posts with no metas listed
    # missing_tiers <- signups$num[(rows_empty | rows_negated | rows_quoted)] %>% paste(collapse = ", ")
    # post numbers that are missing (unscraped) or duplicated by Xenforo
    problem_postnums <- factor(signups$num, levels = 1:max(signups$num)) %>% table %>% data.frame
    colnames(problem_postnums) <- c("postnum", "freq")
    unscraped_posts <- problem_postnums %>% filter(freq < 1 & postnum != 1)
    duplicate_nums <- problem_postnums %>% filter(freq > 1)
    
    # print-friendly list of unscraped post numbers
    if(nrow(unscraped_posts) > 0){
        print_unscraped <- paste(unscraped_posts$postnum, collapse = ", ")
    }else{
        print_unscraped <- "none"
    }
    
    # print-friendly list of duplicate post numbers
    if(nrow(duplicate_nums) > 0){
        print_duplicate <- paste(duplicate_nums$postnum, collapse = ", ")
    }else{
        print_duplicate <- "none"
    }
    
    # print_friendly list of users who posted multiple times
    multi_posters <- signups %>% group_by(user) %>% summarise(n = n()) %>% filter(n > 1) %>% select(user)
    if(nrow(multi_posters) > 0){
        print_multi_posters <- paste(multi_posters$user, collapse = ", ")
    }else{
        print_multi_posters <- "none"
    }
    
    # count signups for each metagame
    signups_by_meta <- data.frame(meta = metas,
                                  num_signups = unname(colSums(signups[, cols_metas] == "y"))) %>%
                 rbind(data.frame(meta = "other / bad format", 
                                  num_signups = sum(rows_empty | rows_negated | rows_quoted)))
    print(signups_by_meta)
    
    # calculate percentage of posts with missing tiers
    print_missing <- round(signups_by_meta$num_signups[n_metas + 1] / nrow(signups), 4)
    
    # print stats
    cat(sep = "", "\n",
        "# unique signups: ", length(unique(signups$user)), "\n",
        "Users who posted multiple times: ", print_multi_posters, "\n\n",
        "% rows with missing tiers: ", print_missing, "\n",
        # "Post #s with missing tiers: ", missing_tiers, "\n\n",
        "Post #s not scraped due to unnatural characters: ", print_unscraped, "\n",
        "Post #s duplicated by Xenforo: ", print_duplicate, "\n")

# OUTPUT ------------------------------------------------------------------
    
    # set output file name
    file_parts <- unlist(strsplit(file_in, "\\."))
    file_out <- paste0(file_parts[1], "_out.", file_parts[2])
    
    write.csv(signups, file_out, row.names = FALSE)
    
    # return signup data frame
    return(signups)
}

# INPUT ------------------------------------------------------------------
                        
# example: UUPL
# set file i/o
file_in <- "uupl.csv"
filepath <- paste0("~/Programming Folders/My Python Files/Scraping/smog/", file_in)
setwd("~/Smogon")

# signup components and their regular expressions
info <- c("signup_name", "tiers", "inactivity")
regex_info <- c("[Nn]ame[\\s]*:[\\s,]*(.*)",
                "[Tt]ier[s Pplayedr]*[\\s:,]*(.*)",
                "[Ii]nactivity[\\s:,]*(.*)")

# metagames in the format and their regular expressions
metas <- c("SM UU", "ORAS UU", "BW UU", "DPP UU", "ADV UU", "GSC UU")
regex_metas <- c("su?m\\s?", "\\boras", "\\bbw", "\\bdpp", "adv\\b", "gsc")

# create spreadsheet
signups <- process_signups(file_in, info, regex_info, metas, regex_metas)
