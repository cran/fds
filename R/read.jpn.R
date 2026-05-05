read.jpn <- function (region,  label = region) 
{
    path <- paste("https://www.ipss.go.jp/p-toukei/JMD_004_002/", region, "/STATS/",   "Mx_1x1.txt", sep = "")
    mx <- try(read.table(path, skip = 2, header = TRUE, na.strings = "."),
              TRUE)
    if(inherits(mx, "try-error"))
        stop("Connection error at the Japanese Human Mortality Database.")
    path <- paste("https://www.ipss.go.jp/p-toukei/JMD_004_002/", region, "/STATS/",   "Exposures_1x1.txt", sep = "")
    pop <- try(read.table(path, skip = 2, header = TRUE, na.strings = "."),
               TRUE)
    if(inherits(pop, "try-error"))
        stop("Exposures file not found at www.mortality.org")
    obj <- list(type = "mortality", label = label, lambda = 0)
    obj$year <- sort(unique(mx[, 1]))
    n <- length(obj$year)
    m <- length(unique(mx[, 2]))
    obj$age <- mx[1:m, 2]
    mnames <- names(mx)[-c(1, 2)]
    n.mort <- length(mnames)
    obj$rate <- obj$pop <- list()
    for (i in 1:n.mort) {
        obj$rate[[i]] <- matrix(mx[, i + 2], nrow = m, ncol = n)
        obj$rate[[i]][obj$rate[[i]] < 0] <- NA
        obj$pop[[i]] <- matrix(pop[, i + 2], nrow = m, ncol = n)
        obj$pop[[i]][obj$pop[[i]] < 0] <- NA
        dimnames(obj$rate[[i]]) <- dimnames(obj$pop[[i]]) <- list(obj$age, 
                                                                  obj$year)
    }
    names(obj$pop) = (names(obj$rate) <- tolower(mnames))
    obj$age <- as.numeric(as.character(obj$age))
    if (is.na(obj$age[m])) 
        obj$age[m] <- 2 * obj$age[m - 1] - obj$age[m - 2]
    return(obj)
}
