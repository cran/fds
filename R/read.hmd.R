read.hmd <-
function(country, sex, file = "Mx_1x1.txt", username, password)
{
    path <- paste("http://www.mortality.org/hmd/", country, "/STATS/", file, sep="")
    userpwd <- paste(username, ":", password, sep = "")
    txt <- getURL(path, userpwd = userpwd)
    con <- textConnection(txt)
    hmd <- read.table(con, skip = 2, header = TRUE, na.strings = ".")
    close(con)
    j <- hmd[,"Year"] == hmd[1, "Year"]
    x <- as.numeric(gsub("\\+", "" ,as.character(hmd[j,"Age"])))
    col <- match(tolower(sex), tolower(colnames(hmd)))
    y <- matrix(as.numeric(hmd[,col]), nrow = length(x))
    colnames(y) <- unique(hmd[, "Year"])
    rownames(y) <- as.character(hmd[j, "Age"])
    if(substr(file, 1, 2) == "Mx")
        yname <- "Mortality rate"
    else
        yname <- "Unknown"
    return(structure(list(x = x, y = y, time = sort(unique(hmd[, "Year"])),
        xname = "Age", yname = yname), class = c("fts", "fds")))
}

