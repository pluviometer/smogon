if(!require(dplyr))
{ 
    install.packages("dplyr")
    library(dplyr)
}

# pull in stats files -----------------------------------------------------

month_to_num <- function(month){
    if(month == "jan"){
        return("01")
    }else if(month == "feb"){
        return("02")
    }else if(month == "mar"){
        return("03")
    }else if(month == "apr"){
        return("04")
    }else if(month == "may"){
        return("05")
    }else if(month == "jun"){
        return("06")
    }else if(month == "jul"){
        return("07")
    }else if(month == "aug"){
        return("08")
    }else if(month == "sep"){
        return("09")
    }else if(month == "oct"){
        return("10")
    }else if(month == "nov"){
        return("11")
    }else if(month == "dec"){
        return("12")
    }
}

get_stats <- function(months, tiers, skill){
    for(tier in tiers){
        for(month in months){
            url <- paste0("https://www.smogon.com/stats/2018-", month_to_num(month),
                          "/gen7", tier, "-", skill, ".txt")
            stats <- read.table(url, sep = "|", stringsAsFactors = FALSE, skip = 5, nrows = 100)
            stats <- stats[,2:4]
            colnames(stats) <- c(paste0("rank_", month), "pokemon", paste0("usage_", month))
            #stats$pokemon <- gsub(" ", "", stats$pokemon)
            stats[,3] <- gsub("%", "", stats[,3]) %>% as.numeric
            
            write.csv(stats, file = paste0("~/Smogon/eisenherz/", month, "18", tier, "-", skill, ".txt"), row.names = FALSE)
            
            assign(paste0(month, "18", tier), stats)
            rm(stats)
        }
    }
}

months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug")
tiers <- c("uu", "ru", "nu", "pu", "lc", "ubers")
skill <- "1760"
get_stats(months, tiers, skill)
get_stats(months, c("ou", "doublesou"), "1825")

# process -----------------------------------------------------------------

get_diff <- function(start, end, tier, skill = 1825, num = 3){
    assign(start, clean_stats(paste0(start, tier, "-", skill)))
    assign(end, clean_stats(paste0(end, tier, "-", skill)))
    
    stats <- merge(get(start), get(end), by = "pokemon")
    stats$rank_diff <- stats[,2] - stats[,4]
    stats$pc_diff <- stats[,5] - stats[,3]
    
    cat(toupper(tier), "----------------------------------------\n")
    # cat("\nBiggest rises in rank:\n")
    # stats %>% arrange(desc(rank_diff)) %>% head(3) %>% 
    #     select(pokemon, apr, may, pc_diff, rank_apr, rank_may, rank_diff) %>% print
    cat("\nBiggest rises in usage:\n")
    stats <- stats[,c(1,3,5,7,2,4,6)]
    stats %>% arrange(desc(pc_diff)) %>% head(num) %>% print
    # cat("\nBiggest drops in rank:\n")
    # stats %>% arrange(rank_diff) %>% head(3) %>% 
    #     select(pokemon, apr, may, pc_diff, rank_apr, rank_may, rank_diff) %>% print
    cat("\nBiggest drops in usage:\n")
    stats %>% arrange(pc_diff) %>% filter(rank_diff > -50) %>% head(num) %>% print
    cat("\n")
    
    return(invisible(stats))
}

start <- "jan18"
end <- "aug18"
skill <- 1825

get_diff(start, end, "ou", num = 10)
get_diff(start, end, "doublesou", num = 5)

for(tier in c("uu", "ru", "nu", "pu", "lc", "ubers")){
    get_diff(start, end, tier, 1760, num = 5)
}



get_diff("jul18", "aug18", "ou", 1825, num = 10) %>% View


# OU: mega mawile up; mega latios down
# UU: togekiss, hydreigon up; swampert down
# RU: mandibuzz, florges up
# PU: togedemaru, qwilfish up
# LC: clamperl up


# OU gliscor, mega mawile
# UU rotom-h, kommo-o
# VGC 19 sun series: incineroar
