if(!require(dplyr))
{
    install.packages("dplyr")
    library(dplyr)
}

get_ppm <- function(infile)
{
    # read file
    filepath <- paste0("~/Programming Folders/My Python Files/Scraping/smog/", infile)
    signups <- read.csv(filepath, stringsAsFactors = FALSE)
    signups <- signups %>% filter(num != 1)
    
    # problem post numbers
    bad_posts <- factor(signups$num, levels = 1:max(signups$num)) %>% table %>% data.frame %>% filter(Freq != 1) 
    colnames(bad_posts) <- c("postnum", "freq")
    bad_posts$postnum <- as.numeric(bad_posts$postnum)
    bad_posts <- bad_posts %>% filter(postnum != 1) %>% mutate(pg_estimate = floor(postnum / 26) + 1)
    print(bad_posts)
    
    # plot signups per minute
    signups$ts <- as.POSIXct(signups$time, format="%b %d, %Y at %I:%M %p")
    ppm <- signups %>% group_by(ts) %>% summarise(n())
    colnames(ppm) <- c("time", "n")
    ppm$ts <- (ppm$time - min(ppm$time)) / (60 * 60)
    ppm$cumn <- cumsum(ppm$n)
    return(ppm)
}

ost10 <- get_ppm("ost10.csv")
ost11 <- get_ppm("ost11.csv")
ost12 <- get_ppm("ost12.csv")
ost13 <- get_ppm("ost13.csv")
ost14 <- get_ppm("ost14.csv")

maxx <- max(ost10$ts, ost11$ts, ost12$ts, ost13$ts, ost14$ts) %>% as.numeric
maxy <- max(ost10$cumn, ost11$cumn, ost12$cumn, ost13$cumn, ost14$cumn) %>% as.numeric

plot(data = ost10, cumn ~ ts, type = "l", col = "black", lwd = 2,
     xlim = c(0, 0.5),
     ylim = c(0, 300),
     xlab = "hours after thread posted", 
     ylab = "number of signups", 
     main = "OST signups over time")
lines(data = ost11, cumn ~ ts, type = "l", col = "tomato", lwd = 2)
lines(data = ost12, cumn ~ ts, type = "l", col = "gold", lwd = 2)
lines(data = ost13, cumn ~ ts, type = "l", col = "limegreen", lwd = 2)
lines(data = ost14, cumn ~ ts, type = "l", col = "blue", lwd = 2)

legend("bottomright", c("OST 10", "OST 11", "OST 12", "OST 13", "OST 14"),
       lwd = 2, 
       col = c("black", "tomato", "gold", "limegreen", "blue"))
