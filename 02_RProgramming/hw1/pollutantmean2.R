pollutantmean2 <- function(directory, pollutant, id = 1:332){
    datalist<-list.files(directory,full.names = TRUE)
    dat <- data.frame()
    for (i in id) {
        dat <-rbind(dat,read.csv(datalist[i]))
    }
    mean(dat_subset[[pollutant]],na.rm=TRUE)
    
}