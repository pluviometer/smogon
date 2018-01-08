######################
# setup
######################
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

# functions like grepl/agrepl but returns "y" / "" instead of TRUE / FALSE, as required by auction bot
mentions_meta <- function(cell, meta, fuzzy = FALSE, max.distance = NA)
{
    if(fuzzy == TRUE){
        mentioned <- agrepl(meta, cell, max.distance = max.distance)
    }else
        mentioned <- grepl(meta, cell)
    
    if(mentioned){
        return("y")
    }else
        return("")
}

######################
# take inputs
######################
infile <- "exhibition.csv"
outfile <- "exhibition_signups.csv"
metas <- c("Ubers", "PU", "LC", "Monotype")
regex <- c("uber", "m?pu\\b", "m?l[ittle ]*c[up]*\\b", "\\bmono")
n_metas <- length(metas)

filepath <- paste0("~/Programming Folders/My Python Files/Scraping/smog/", infile)
meta_columns <- 7 + 1:n_metas

######################
# processing
######################
# read in file, get rid of OP
signups <- read.csv(filepath, stringsAsFactors = FALSE)
signups <- signups %>% filter(num != 1)

# separate post body
signups$playername <- str_match(signups$text, "[Pplayer]*[Nn]ame[\\s:,]*(.*),")[,2]
signups$tiers <- str_match(signups$text, "[Tt]iers? [Pp]layed[\\s:]*(.*),")[,2]
signups$timezone <- str_match(signups$text, "[Tt]ime\\s?zone[\\s:]*(.*),")[,2]
signups$timemissed <- str_match(signups$text, "[Tt]ime [Mm]issed[\\s:]*(.*)")[,2]

# if playername didn't separate properly, replace with forum name
no_names <- which(is.na(signups$playername))
quote_rows <- which(grepl(" said:", tolower(signups$text)))
bad_names <- union(no_names, quote_rows)
signups$playername[bad_names] <- signups$user[bad_names]

# separate tiers
meta_text <- tolower(signups$tiers)
for(i in 1:n_metas)
{
    signups[, metas[i]] <- sapply(meta_text, mentions_meta, meta = regex[i]) %>% unname
}

signups$Monotype <- sapply(meta_text, mentions_meta, meta = "mono", fuzzy = TRUE, 
                         max.distance = list(sub = 1, ins = 4)) %>% unname # special case for Monotype

# overwrite meta lists that say "all"
cells_all <- union(which(grepl("(?<!\\w)all(?! \\w)", meta_text, perl = TRUE)),
                   which(grepl("\\ball tiers", meta_text)))
signups[cells_all, meta_columns] <- "y"

# overwrite suspicious meta lists, posts that quote other posts, and rows with no tiers
bad_words <- c(" but ", " besides ", " except ", " not ", " minus ", " aside ", " bar ", " excluding ", " save for ", " other than ")
bad_rows <- sapply(bad_words, grepl, meta_text) %>% apply(1, sum) 
bad_cells <- which(bad_rows > 0)
empty_cells <- which(apply(signups[,meta_columns], 1, function(x) all(x == "")))
signups[union(bad_cells, empty_cells), meta_columns] <- "?"
signups[quote_rows, 5:ncol(signups)] <- "?"

######################
# diagnostics
######################
missing <- signups$num[signups[,meta_columns[1]] == "?"] %>% paste(collapse = ", ")

bad_nums <- factor(signups$num, levels = 1:max(signups$num)) %>% table %>% 
    data.frame %>% filter(Freq != 1 & `.` != 1)
colnames(bad_nums) <- c("postnum", "freq")
bad_nums$postnum <- as.numeric(bad_nums$postnum)
bad_nums <- bad_nums %>% mutate(pg_estimate = floor(postnum / 26) + 1)
unscraped <- bad_nums %>% filter(freq < 1)
duplicate <- bad_nums %>% filter(freq > 1)

if(nrow(unscraped) > 0){
    unscraped_print <- paste(unscraped$postnum, " (pg ", floor(unscraped$postnum / 26) + 1, ")", sep = "") %>% paste(collapse = ", ")
}else{
    unscraped_print <- "none"
}

if(nrow(duplicate) > 0){
    duplicate_print <- paste(duplicate$postnum, " (pg ", floor(duplicate$postnum / 26) + 1, ")", sep = "") %>% paste(collapse = ", ")
}else{
    duplicate_print <- "none"
}

n_signups <- rep(NA, length(metas) + 1)
for(i in 1:length(metas)){
    n_signups[i] <- nrow(signups[signups[, meta_columns[i]] == "y",])
}
n_signups[length(metas) + 1] <- nrow(signups[signups[,meta_columns[1]] == "?",]) 
data.frame(meta = c(metas, "ERROR"), n_signups) %>% arrange(desc(n_signups)) 

cat(sep = "",
    "% rows with bad data: ", nrow(signups[signups[,meta_columns[1]] == "?",]) / nrow(signups), "\n\n",
    "Post #s with missing tiers info: ", missing, "\n\n",
    "Post #s that were not scraped due to unnatural characters: ", unscraped_print, "\n\n",
    "Post #s that were duplicated by Xenforo: ", duplicate_print)

######################
# send to spreadsheets
######################
signups <- signups[,c(1:4, meta_columns, 5:7)]
write.csv(signups, outfile, row.names = FALSE)