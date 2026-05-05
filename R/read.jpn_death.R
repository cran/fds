read.jpn_death <- function(region, label = region)
{
    path <- paste("https://www.ipss.go.jp/p-toukei/JMD_004_002/", region, "/STATS/",   "Deaths_1x1.txt", sep = "")
    mx <- try(read.table(path, skip = 2, header = TRUE, na.strings = "."), TRUE)
    if(inherits(mx, "try-error"))
        stop("Connection error at www.mortality.org. Please check username, password and country label.")
    path <- paste("https://www.ipss.go.jp/p-toukei/JMD_004_002/", region, "/STATS/",   "Exposures_1x1.txt", sep = "")
    pop <- try(read.table(path, skip = 2, header = TRUE, na.strings = "."), TRUE)
    if(inherits(pop, "try-error"))
        stop("Exposures file not found at www.mortality.org")
    return(list(Deaths = mx, pop = pop))
}
