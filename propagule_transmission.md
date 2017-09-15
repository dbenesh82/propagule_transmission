Parasite propagule transmission
================

**Background**: Here I examine how the propagules of parasitic worms fit into food webs. I combine data on [parasite life cycles](http://onlinelibrary.wiley.com/doi/10.1002/ecy.1680/suppinfo), propagule sizes, host body masses, and [size-structured food webs](http://onlinelibrary.wiley.com/doi/10.1890/0012-9658(2006)87%5B2411:CBRINF%5D2.0.CO;2/full). I then plot the data to gauge how propagule-consumer size relationships compare to prey-predator ones.

**Analysis** First, import the libraries and host data.

``` r
library(dplyr)
library(ggplot2)
library(tidyr)

options(stringsAsFactors = FALSE) #never have species lists as factors; always as character vectors

#import host data
setwd("C:/Users/phosp/OneDrive/Documents/Benesh/Proposals_ideas/DFG_eigenestelle/prelim_analyses/propagule_transmission")
dataH <- read.csv(file="CLC_database_hosts.csv", header = TRUE, sep=",")
```

For each parasite species, there is a list of hosts. We need to rearrange the host lists as trophic links exploited by (or potentially exploited by) the parasite. For example, all the host species known to be 1st hosts for parasite A may be consumed by all the 2nd hosts of parasite A. The next code blocks create functions to do this on a species by species basis. The first function creates all the potential trophic links exploited by a parasite. The next two functions label the links, e.g. "propagule to 1st host", "1st host to 2nd host", etc.

``` r
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
```

``` r
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
```

``` r
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
```

Now we loop through all the parasite species, create the trophic links in its life cycle, and then collate them together into a large data table.

``` r
# loop through parasite species to get trophic links dataset
para.sp <- sort(unique(dataH$Parasite.species))

for(i in seq_along(para.sp)){
  if(i==1) {
    tl.out <- label.trophic.links(para.sp[i])
    tl.out$Parasite.species <- para.sp[i]
  } else {
    tl.out2 <- label.trophic.links(para.sp[i])
    tl.out2$Parasite.species <- para.sp[i]
    tl.out <- rbind(tl.out,tl.out2)
  }
}
rm(para.sp, i, tl.out2)
```

Add parasite life cycle length to trophic links table.

``` r
lcl <- group_by(dataH, Parasite.species)%>%
  summarize(lcl = max(Host.no))
tl.out <- left_join(tl.out, lcl)
```

    ## Joining, by = "Parasite.species"

Import host mass data (~20,000 species), compiled from a variety of sources, and then add host masses to the trophic link table.

``` r
# load size data
host.size <- read.csv(file="collated_host_mass_data.csv", header = TRUE, sep=",")

# host size either a length or a mass;restrict to just masses
host.size <- filter(host.size, !is.na(body.mass))%>%
  select(binomial, body.mass)%>%
  group_by(binomial)%>%
  summarize(body.mass = mean(body.mass)) # some species have multiple entries, so avg them

# add consumer mass
tl.out$cons.mass<-
  host.size$body.mass[match(tl.out$cons, host.size$binomial)]
# add resource mass
tl.out$res.mass<-
  host.size$body.mass[match(tl.out$res, host.size$binomial)]
```

The trophic link table also includes 'parasite propagule to host consumer' entries, and we want to add parasite propagule masses to these trophic links. We import the parasite body size data from the life cycle database, restrict it to the propagules, and then convert lengths and widths to biovolume. Here the assumption is that biovolume (mm3) can be converted to mass (mg) assuming a tissue density of 1.1 mg/mm3. This is then converted to g, as that is the unit used in the host mass data.

``` r
# load helminth life history data
dataL <- read.csv(file="CLC_database_lifehistory.csv", header = TRUE, sep=",")

# filter to just propagule stages
dataL <- filter(dataL, Host.no == 0)

# calculate a biovolume (mm3) for worm stages from lengths, widths, and shapes
  dataL$biovolume <- NA
  dataL$biovolume <- as.numeric(dataL$biovolume)
  
  # if shape is cylinder, elongate, whip, calculate volume as a cylinder
  sv <- which(dataL$Shape == "cylinder" | dataL$Shape == "thread-like" | dataL$Shape == "whip")
  dataL$biovolume[sv] <- 
    pi * (dataL$Width[sv]/2)^2 * dataL$Length[sv]
  
  # if shape is coiled, sphere, ellipsoid, calculate volume as an ellipsoid
  sv <- which(dataL$Shape == "coiled" | dataL$Shape == "sphere" | dataL$Shape == "ellipsoid")
  dataL$biovolume[sv] <-
    4/3 * pi * dataL$Length[sv]/2 * dataL$Width[sv]/4

dataL <- mutate(dataL, biovolume = biovolume * .0011)%>% # convert mm3 to g (assumes tissue density of 1.1 g/cm3)
  select(Parasite.species, Stage, Egg.hatch, biovolume)
```

Several measurements may be given for a parasite propagule, such as the egg, embryo, or hatched larave. We should extract the propagules measurments respresenting what is ingested by the consumer. That is the egg if it does not hatch or the free larva if the egg does hatch.

``` r
#distinguish species transmitted by either eggs or hatched propagules
prop <- select(dataL, Parasite.species, Egg.hatch)%>%
  filter(!is.na(Egg.hatch))%>%distinct()
prop$selector <- "free larva"
prop$selector[which(prop$Egg.hatch == "eaten")] <- "egg"

#this tags rows that are propagules, but are not the transmission stage (e.g. embryos)
dataL$propagule.selector[
  is.na(match(
    paste(dataL$Parasite.species, dataL$Stage),
    paste(prop$Parasite.species, prop$selector))
    )
  ] <- 1
```

Then, we can average propagule measurements for each parasite species.

``` r
dataL <- filter(dataL, is.na(propagule.selector))%>%
  select(-propagule.selector, -Egg.hatch)%>%
  group_by(Parasite.species, Stage)%>%
  summarize(biovolume = mean(biovolume, na.rm=T))%>%
  filter(!is.nan(biovolume))
```

Then we add it to the trophic link dataset.

``` r
# match propagule masses to parasite species in trophic link data
eggies <- dataL$biovolume[match(tl.out$Parasite.species, dataL$Parasite.species)]
# restrict to just propagule stages and add to trophic link data
eggies <- eggies[which(tl.out$res=='propagule')]
tl.out$res.mass[which(tl.out$res=='propagule')] <- eggies
```

To compare how parasite-exploited trophic links compare to the normal links in food-webs, we import a database of food web links [(Brose et al. 2006)](http://onlinelibrary.wiley.com/doi/10.1890/0012-9658(2006)87%5B2411:CBRINF%5D2.0.CO;2/full). The data is mostly predator-prey, and quite a lot of mass data is missing (4400 of 16800 links, ~ 26%)

``` r
brose<-read.csv(file="Brose_bodysizes_2008_DB.csv", header = TRUE,sep = ",")

# plot data to show different kinds of trophic interactions
ggplot(data = brose,
       aes(x = log10(Mean.mass..g..resource),
           y = log10(Mean.mass..g..consumer),
           color = Type.of.feeding.interaction)) +
  geom_point(alpha = 0.2) +
  labs(x = "\nLog(resource mass)", y = "Log(consumer mass)\n") +
  scale_x_continuous(limits = c(-8, 6), breaks = seq(from = -6, to = 6, by = 3))
```

    ## Warning: Removed 4399 rows containing missing values (geom_point).

![](propagule_transmission_files/figure-markdown_github/unnamed-chunk-12-1.png)

The links with missing mass data nonetheless have consumer-resource mass ratios. Brose et al. say that these masses were calculated from length allometries, but I am unsure about how to back calculate masses, as the allometric relationships were not given. The links with missing masses show a wider spread in mass ratios, especially large ratios (i.e. big consumers, small resources). Thus, grazers or detritivores are probably overrepresented here.

``` r
# look at missing data
brose <- mutate(brose, missing_mass = if_else(is.na(Mean.mass..g..resource), 1, 
                                              if_else(is.na(Mean.mass..g..consumer), 1, 0)))

ggplot(brose, aes(x = factor(missing_mass), y = log10(Consumer.resource.body.mass.ratio))) + geom_boxplot()
```

![](propagule_transmission_files/figure-markdown_github/unnamed-chunk-13-1.png)

For the sake of simplicity, we'll ignore the missing mass data for now, but we should keep it in mind, and maybe consider larger [datasets](http://www.globalbioticinteractions.org/about.html) in the future.

Let's eliminate a few interaction types (e.g. parasitoid) that are obviously not exploited by trophically-transmitted helminths and then combine the food web and parasite trophic links data into a single data frame for plotting.

``` r
# remove certain feeding type
brose <- filter(brose, !(Type.of.feeding.interaction %in% c("parasitic", "parasitoid", "pathogen")))
# add var for combining with tl.out
brose$Parasite.species <- "not applicable"
brose$trans.step <- "food web"
brose$lcl <- "not applicable"
brose <- select(brose, Parasite.species, cons.mass = Mean.mass..g..consumer,
                 res.mass = Mean.mass..g..resource, trans.step, lcl)
# combine into single data frame
plot.dat <- select(tl.out, Parasite.species, cons.mass, res.mass, trans.step, lcl)
plot.dat <- rbind(plot.dat, brose)
```

Create a theme for plots.

``` r
theme.o <- theme_update(axis.text = element_text(colour="black", size = 15),
                        axis.title = element_text(colour="black", size = 18, face = "bold", lineheight=0.25),
                        axis.ticks = element_line(colour="black"),
                        panel.border = element_rect(colour = "black",fill=NA),
                        panel.grid.minor=element_blank(),
                        panel.grid.major=element_line(color="gray",linetype = "dotted"),
                        panel.background= element_rect(fill = NA))
```

Now, we'll create a series of plots with parasite data overlaid on the food web data. The basic plot is consumer/predator size on the y-axis and resource/prey size on the x-axis. First, we plot links where the parasite propagule is transmitted to the 1st host. Parasite propagules are tiny, but they infect just about anything; they cover 10 orders of magnitude on the y-axis!). The smallest ones overlap the food web data well.

``` r
ggplot(data = filter(plot.dat, trans.step=="food web"),
       aes(x = log10(res.mass), y = log10(cons.mass))) +
  geom_point(color = "gray", alpha = 0.2) +
  labs(x = "\nLog(prey/propagule mass)", y = "Log(predator/1st host mass)\n") +
  geom_point(data = filter(plot.dat, trans.step=="propagule to 1st host"), 
             aes(x = log10(res.mass), y = log10(cons.mass)),
             color = 'red', alpha = 0.2) +
  annotate("text", x = 2.75, y = -3.75, label = "Food web trophic links\n Brose et al. 2006",
           size = 6, color = "darkgray") +
  annotate("text", x = 2.75, y = -5.5, label = "Propagule to 1st host", 
           size = 6, color = "red") +
  scale_x_continuous(limits = c(-8, 6), breaks = seq(from = -6, to = 6, by = 3)) +
  scale_y_continuous(limits = c(-8, 6), breaks = seq(from = -6, to = 6, by = 3))
```

    ## Warning: Removed 4216 rows containing missing values (geom_point).

    ## Warning: Removed 2484 rows containing missing values (geom_point).

![](propagule_transmission_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
#export 700 x 500 px as svg; change color of part of axis titles
```

Now imagine that complex life cycle parasites skipped their first hosts, i.e. their propagules are consumed by the second hosts instead. Now there is much less overlap between the parasite and food web data, indicating that parasite propagules are usually too small to be considered a normal food item for their hosts.

``` r
ggplot(data = filter(plot.dat, trans.step=="food web"),
       aes(x = log10(res.mass), y = log10(cons.mass))) +
  geom_point(color = "gray", alpha = 0.2) +
  labs(x = "\nLog(prey/propagule mass)", y = "Log(predator/1st host mass)\n") +
  geom_point(data = filter(plot.dat, trans.step=="propagule to 2nd host"), 
             aes(x = log10(res.mass), y = log10(cons.mass)),
             color = 'blue', alpha = 0.2) +
  annotate("text", x = 2.75, y = -3.75, label = "Food web trophic links\n Brose et al. 2006",
           size = 6, color = "darkgray") +
  annotate("text", x = 2.75, y = -5.5, label = "Propagule to 2nd host", 
           size = 6, color = "blue") +
  scale_x_continuous(limits = c(-8, 6), breaks = seq(from = -6, to = 6, by = 3)) +
  scale_y_continuous(limits = c(-8, 6), breaks = seq(from = -6, to = 6, by = 3))
```

    ## Warning: Removed 4216 rows containing missing values (geom_point).

    ## Warning: Removed 1854 rows containing missing values (geom_point).

![](propagule_transmission_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
#export 700 x 500 px as svg; change color of part of axis titles
```

The role of the first host is not only to eat the propagule, but also to transmit the parasite to the 2nd host. Thus, we expect trophic links of the first host to the second host to fall solidly within the range of the food web data. That is mostly the case, but there are also some cases with small 1sts hosts being transmitted to especially large 2nd hosts.

``` r
ggplot(data = filter(plot.dat, trans.step=="food web"),
       aes(x = log10(res.mass), y = log10(cons.mass))) +
  geom_point(color = "gray", alpha = 0.2) +
  labs(x = "\nLog(prey/propagule mass)", y = "Log(predator/2nd host mass)\n") +
  geom_point(data = filter(plot.dat, trans.step=="1st host to 2nd host"), 
             color = "purple", alpha = 0.2) +
  annotate("text", x = 2.75, y = -3.75, label = "Food web trophic links\n Brose et al. 2006",
           size = 6, color = "darkgray") +
  annotate("text", x = 2.75, y = -5.5, label = "1st host to 2nd host", 
           size = 6, color = "purple") +
  scale_x_continuous(limits = c(-8, 6), breaks = seq(from = -6, to = 6, by = 3)) +
  scale_y_continuous(limits = c(-8, 6), breaks = seq(from = -6, to = 6, by = 3))
```

    ## Warning: Removed 4216 rows containing missing values (geom_point).

    ## Warning: Removed 15162 rows containing missing values (geom_point).

![](propagule_transmission_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
#export 700 x 500 px as svg; change color of part of axis titles
```

We could play with these plots, e.g. by changing the food web data to a heat map or a contour plot. But these plots are sufficient for now.

It seems reasonable that small first hosts are both likely to eat propagules and be eaten by a 2nd host. But starting in small hosts may have a downside: longer life cycles. Let's check. First, we'll combine the host data with the body mass data. Then we can run a linear regression. There is a significant negative correlation.

``` r
lclvsprop <- left_join(dataH, host.size, by = c("Host.species" = "binomial"))%>% # add host mass
  group_by(Parasite.species, Host.no)%>%
  summarize(host.mass = mean(body.mass, na.rm=T))%>% # calc avg host size for each parasite stage
  left_join(lcl) # add lcl
```

    ## Joining, by = "Parasite.species"

``` r
# sig neg correlation between life cycle length and 1st host mass
summary(lm(lcl ~ log(host.mass), filter(lclvsprop, Host.no == 1)))
```

    ## 
    ## Call:
    ## lm(formula = lcl ~ log(host.mass), data = filter(lclvsprop, Host.no == 
    ##     1))
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -0.9576 -0.3431 -0.1396  0.3585  1.8107 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     1.951656   0.023401   83.40   <2e-16 ***
    ## log(host.mass) -0.056595   0.002759  -20.51   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.5335 on 538 degrees of freedom
    ##   (433 observations deleted due to missingness)
    ## Multiple R-squared:  0.4389, Adjusted R-squared:  0.4378 
    ## F-statistic: 420.8 on 1 and 538 DF,  p-value: < 2.2e-16

And then we visualize the correlation.

``` r
ggplot(filter(lclvsprop, Host.no == 1),
       aes(x = log10(host.mass), y = lcl)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = 'lm', se = F, color = 'black') +
  labs(y = "Life cycle length", x = "1st host mass")
```

    ## Warning: Removed 433 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 433 rows containing missing values (geom_point).

![](propagule_transmission_files/figure-markdown_github/unnamed-chunk-20-1.png)

Propagules were consistently small. But maybe their size variation affects the type of first host. We can imagine larger propagules being more likely to be eaten by bigger consumers. Consistent with this, there is a significant positive correlation between propagule mass and 1st host mass.

``` r
lclvsprop <- left_join(lclvsprop, dataL) # combine host size and propagule data
```

    ## Joining, by = "Parasite.species"

``` r
# sig pos correlation between propagule size and 1st host mass
summary(lm(log(host.mass) ~ log(biovolume), filter(lclvsprop, Host.no == 1)))
```

    ## 
    ## Call:
    ## lm(formula = log(host.mass) ~ log(biovolume), data = filter(lclvsprop, 
    ##     Host.no == 1))
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -13.8441  -5.9802  -0.5884   5.9149  16.5128 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     13.1243     4.2685   3.075 0.002246 ** 
    ## log(biovolume)   1.0403     0.2985   3.485 0.000544 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 7.672 on 417 degrees of freedom
    ##   (554 observations deleted due to missingness)
    ## Multiple R-squared:  0.0283, Adjusted R-squared:  0.02597 
    ## F-statistic: 12.15 on 1 and 417 DF,  p-value: 0.000544

When we visualize the correlation, though, we see that it is rather weak. Parasite propagules of a given size can go into hosts that range over 10 orders of magnitude in size. Thus, I am not ready to claim that propagule sizes increase when the first host is larger.

``` r
ggplot(filter(lclvsprop, Host.no == 1),
       aes(x = log10(biovolume), y = log10(host.mass))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = 'lm', se = F, color = 'black') +
  labs(x = "Propagule mass", y = "1st host mass")
```

    ## Warning: Removed 554 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 554 rows containing missing values (geom_point).

![](propagule_transmission_files/figure-markdown_github/unnamed-chunk-22-1.png)