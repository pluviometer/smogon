# RScript formatCheck.R hajime 1
# RScript formatCheck.R ladybug 2 blue red darkorchid

# deal with arguments from command line
args <- commandArgs(TRUE)
vargs <- strsplit(args, " ")
stamp <- as.character(vargs[[1]])
num <- vargs[[2]]

# if there are more than two arguments, set colors
if(length(vargs) > 2){
    add <- vargs[[3]]
    remove <- vargs[[4]]
    comment <- vargs[[5]]
}else{ # otherwise use these defaults
    add <- "deepskyblue"
    remove <- "red"
    comment <- "limegreen"
}

cat("Formatting... ")

# make header
# embed stamp
if(stamp == "ladybug"){
    stampbb <- "[img]http://i.imgur.com/ivvxQRa.gif[/img]"
}else if(stamp == "hajime"){
    stampbb <- "[img]http://i.imgur.com/p4pcPhv.gif[/img]"
}else if(stamp == "misdreavus"){
    stampbb <- "[img]http://i.imgur.com/89J1CeD.gif[/img]"
}else if(stamp == "mismagius"){
    stampbb <- "[img]http://i.imgur.com/d1YIfXz.gif[/img]"
}else if(stamp == "azurill"){
    stampbb <- "[img]http://imgur.com/475w8h7.gif[/img]"
}else if(stamp == "latios"){
    stampbb <- "[img]http://imgur.com/IWR8ikR.gif[/img]"
}else
    stampbb <- ""

# GP x/2
if(num == 1){
    stampnum <- "[b]GP 1/2[/b]"
}else if(num == 2){
    stampnum <- "[b]GP 2/2[/b]"
}else
    stampnum <- ""

# color key
key <- paste0("[b][color=", add, "]add[/color][/b] ",
              "[b][color=", remove, "][s]remove[/s][/color][/b] ",
              "[b][color=", comment, "][i]comment[/i][/color][/b]")

# paste the above together into the header
header <- paste(stampnum, key, stampbb, sep = "\n")

# read in file
filename <- "gpcheck.txt"
check <- readChar(filename, file.info(filename)$size)

# substitute bbcode
check <- gsub("\\[B\\]", paste0("[b][color=", add, "]"), check, ignore.case = TRUE)
check <- gsub("\\[/B\\]", "[/color][/b]", check, ignore.case = TRUE)
check <- gsub("\\[S\\]", paste0("[b][color=", remove, "][s]"), check, ignore.case = TRUE)
check <- gsub("\\[/S\\]", "[/s][/color][/b]", check, ignore.case = TRUE)
check <- gsub("\\[U\\]", paste0("[b][color=", comment, "][i]"), check, ignore.case = TRUE)
check <- gsub("\\[/U\\]", "[/i][/color][/b]", check, ignore.case = TRUE)
check <- gsub("\r\n", "\n", check)

# write to output
write(header, file = "gpoutput.txt", append = FALSE)
write(check, file = "gpoutput.txt", append = TRUE)
cat("Done!\nOutput sent to gpoutput.txt")