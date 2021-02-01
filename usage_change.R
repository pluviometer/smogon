if(!require(dplyr))
{ 
   install.packages("dplyr")
   library(dplyr)
}

# pull in stats files -----------------------------------------------------

# this converts a month name to a number
all_months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
month_to_num <- function(month){
   index <- match(month, all_months)
   num <- ""
   
   if(index < 10){
      num <- paste0("0", index)
   }else{
      num <- toString(index)
   }
   return(num)
}

# this creates files based on txts in the smogon.com/stats directory
make_stats_files <- function(months, tiers, skill){
   for(tier in tiers){
      for(month in months){
         # piece together the link for the desired month/tier/skill
         url <- paste0("https://www.smogon.com/stats/2020-", month_to_num(month), "/gen8", tier, "-", skill, ".txt")
         # read data from link into R table
         stats <- read.table(url, sep = "|", stringsAsFactors = FALSE, skip = 5, nrows = 100, quote = "")
         # we only want the rank, pokemon, and usage %
         stats <- stats[,2:4]
         # rename / reformat columns
         colnames(stats) <- c(paste0("rank_", month), "pokemon", paste0("usage_", month))
         stats[,3] <- gsub("%", "", stats[,3]) %>% as.numeric
         
         # write to a file in my /Smogon/eisenherz directory
         write.csv(stats, file = paste0("~/Smogon/eisenherz/", month, "20", tier, "-", skill, ".txt"), row.names = FALSE)
         
         #assign(paste0(month, "20", tier), stats)
         #rm(stats)
      }
   }
}

# this reads data from the files created above into an R object
get_stats <- function(filename)
{
   return(read.csv(paste0("~/Smogon/eisenherz/", filename, ".txt")))
}

# process -----------------------------------------------------------------

get_diff <- function(start, end, tier, skill = 1825, num = 3){
   # create objects for starting and ending months
   assign(start, get_stats(paste0(start, tier, "-", skill)))
   assign(end, get_stats(paste0(end, tier, "-", skill)))
   
   # merge start and end into one object
   stats <- merge(get(start), get(end), by = "pokemon")
   
   # calculate changes
   stats$rank_diff <- stats[,2] - stats[,4]
   stats$pc_diff <- stats[,5] - stats[,3]
   stats$pc_pc_diff <- round(stats$pc_diff / stats[,3], 3) * 100
   
   # reorder columns
   stats <- stats[,c(1,3,5,7,8,2,4,6)]
   
   # print top and bottom [num] changes
   cat(toupper(tier), "----------------------------------------\n")
   cat("\nBiggest rises in usage:\n")
   stats %>% arrange(desc(pc_diff)) %>% head(num) %>% print
   cat("\nBiggest drops in usage:\n")
   stats %>% arrange(pc_diff) %>% filter(rank_diff > -50) %>% head(num) %>% print
   cat("\n")
   
   return(invisible(stats))
}

# how to use --------------------------------------------------------------

# pick out stuff that you want to create files for
months <- c("sep", "oct", "nov", "dec")
tiers <- c("uu", "ru", "nu", "pu", "zu", "lc", "nationaldex", "ubers")
skill <- "1630"
make_stats_files(months, tiers, skill)

# ou and dou use 1695 instead of 1630
make_stats_files(months, c("ou", "doublesou"), "1695")

# pick out two months you want to compare
start <- "nov20"
end <- "dec20"
skill <- 1695

get_diff(start, end, "ou", 1695, num = 10)
get_diff(start, end, "doublesou", 1695, num = 5)

for(tier in tiers){
   get_diff(start, end, tier, 1630, num = 5)
}
