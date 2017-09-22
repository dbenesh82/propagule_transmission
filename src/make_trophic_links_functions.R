# functions for creating trophic link combinations from the hosts table in the life cycle database

make.trophic.links <- function(species){
  # returns all potential trophic links for a parasite species; every higher level host eats every lower level host
  # start by filtering hosts for given parasite species
  sp<-filter(dataH, Parasite.species == species)%>%
    select(Host.species, Host.no, Facultative)
  
  # make all possible links
  links<-expand.grid(sp$Host.sp, c(sp$Host.sp, 'propagule'))
  names(links)<-c('cons','res')
  links <- mutate_all(links, funs(as.character(.)))
  
  # label the hosts as first host, second, etc.
  links <- left_join(links, sp, by = c("cons" = "Host.species"))
  links <-left_join(links, sp, by = c("res" = "Host.species"))
  names(links) <- c("cons", "res", "cons.host.no", "cons.fac", "res.host.no", "res.fac")
  links$res.host.no[is.na(links$res.host.no)] <- 0
  
  # eliminate links where lower level hosts consume higher level hosts, e.g. 1st host consumes 2nd host
  links <- filter(links, cons.host.no > res.host.no)
  
  return(links)
}


link.labeller <- function(num){
  # function to give hosts a name instead of number
  if(num == 0) {
    "propagule"
  } else if (num == 1) {
    "1st host"
  } else if (num == 2) {
    "2nd host"
  } else if (num == 3) {
    "3rd host"
  } else if (num == 4) {
    "4th host"
  } else if (num == 5) {
    "5th host"
  }
}

label.trophic.links <- function(species) {
  # make links df for given species
  links <- make.trophic.links(species)
  
  # create labels for consumer and resource host
  lab1 <- sapply(links$cons.host.no, link.labeller)
  lab2 <- sapply(links$res.host.no, link.labeller)
  
  # combine labels and add them to df
  links$trans.step <- paste(lab2, "to", lab1)
  
  return(links)
}
