# rm(list = ls())
## Loading libraries
library(tidyverse)
library(RColorBrewer)


# Functions ---------------------------------------------------------------

## Function to find the gain from aggregating
findGain <- function(x) {
  baseline <- x %>% 
    filter(scenario == "0percent Baseline") %>% 
    dplyr::select(median) %>% unlist
  x <- x %>% mutate(gain = ((baseline - median)/baseline)) 
  return(x)
}
## prepare the function for a nested list
map_findGain <- function(y) {
  map(y, function(z) findGain(z)) %>% 
    return()
}

# Loading and checking the data -------------------------------------------
boxplot_df <- readRDS(url("https://zenodo.org/record/7308221/files/boxplot_df.RDS?download=1"))

boxplot_df %>% head
boxplot_df %>% dim

boxplot_df$year %>% table()
## Year is the year simulated
boxplot_df$stat %>% table()
## Stat is how data is being summarised for each single generation in the simulations
## Each simulations includes approximately 100 farms, and the simultaions produce lice counts for every farm every week, which amounts to 100 farms mulitplied by 208 weeks in the simulations, multiplid by 3 (for the to stages and whether they treat or not), amounts to 62400 data points from each simulation. Further, we run 45000 simulations in this exercise, amounting to 2808000000 data points. The data is therefore aggregated to the following statistics: max_lice, which is the maximum number of lice counted during the simulation/year; min_lice, the minimum number of lice counted during the simulation/year; spring mean, denoting the mean number of lice during the period in the spring when theallowed number of lice pr fish in the farm cannot exceed 0.2; yearly mean, the yearly average number of lice during the simulation.
boxplot_df$lice %>% table()
## The lice column holds information about whether 
boxplot_df$scenario %>% table()
## Denotes the scenarios described in the paper


# Preparing the data for plotting ------------------------------------------
table_df <- boxplot_df %>% ## tabell means table in Norwegian
  filter (stat == "year_mean") %>%
  group_by(scenario, lice, year) %>%
  summarise(mean     = mean(value, na.rm  =T),
            median   = median(value, na.rm  =T),
            SD       = sd(value, na.rm  =T),
            quant0   = quantile(value, na.rm = T)[1],
            quant25  = quantile(value, na.rm = T)[2],
            quant75  = quantile(value, na.rm = T)[4],
            quant100 = quantile(value, na.rm = T)[5]
            ) %>% arrange("year") %>% ungroup
table_df %>% dim
table_df %>% as.data.frame()

table_df <- table_df %>% split(.$lice) %>% 
  map(function(x) split(x, x$year)) %>% 
  map(function(w) map_findGain(w)) %>% 
  map(function(xx) do.call(rbind, xx)) %>% 
  do.call(rbind, .) %>% 
  dplyr::select(lice, scenario, year, median, gain)


table_short <- table_df %>% 
  group_by(scenario, lice) %>% 
  summarise(median_gain   = median(gain)
            ) %>% 
  ungroup() %>% 
  mutate(remove = as.numeric(substring(scenario, 1, 1))*10) 
table_short <- table_short %>% mutate(Removal = "Random removal")
table_short$Removal[nchar(table_short$scenario) > 20] <- 'Strategic removal'
table_short$Removal[table_short$scenario == "0percent Baseline"] <- 'Baseline'
table_short


## Removing 10 % in connectivity scenarios, because they were not reported in the paper
table_short %>% head
table_short$scenario %>% unique
table_short <- table_short %>% 
  filter(scenario != "10 percent connectivity")

# New facet label names for lice variable
lice_labs <- c("Adult females", "Other motile", "Treatments")
names(lice_labs) <- c("AF", "OM", "treatment")

## Set colour palette for the median gain plot
color_palette <- c(brewer.pal(7, 'YlOrRd')[7],
                   brewer.pal(4, 'Greens'),
                   brewer.pal(7, 'Blues')[3:7]
)

ggplot(table_short, 
        aes(x=remove, y=median_gain, label = scales::percent(median_gain, accuracy = 1), colour=Removal)) +
  facet_wrap(vars(lice), labeller = labeller(lice = lice_labs)) +
  geom_line() +
  geom_point() + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y= "Median reduction", x = "Percentage of farms removed")  +
geom_text(    # move to center of bars
          vjust = -0.5,    # nudge above top of bar
          size = 3) +
  theme_bw()  

table_treatment <- table_df %>% filter(lice == 'treatment')
table_AF        <- table_df %>% filter(lice == 'AF')
table_OM        <- table_df %>% filter(lice == 'OM')
table_treatment %>% arrange(year)
table_AF
table_OM

## Color palette for the boxplots
color_palette2 <- c(brewer.pal(7, 'YlOrRd')[7],
                  # brewer.pal(7, 'Blues')[3],
                   brewer.pal(7, 'Blues')[4],
                   brewer.pal(4, 'Greens')[1],
                   brewer.pal(7, 'Blues')[5],
                   brewer.pal(4, 'Greens')[2],
                   brewer.pal(7, 'Blues')[6],
                   brewer.pal(4, 'Greens')[3],
                   brewer.pal(7, 'Blues')[7],
                   brewer.pal(4, 'Greens')[4]
)
boxplot_df %>% tibble


## Boxplot for the adult female lice
AF_boxplot = ggplot(boxplot_df %>% 
                        filter (stat == "year_mean") %>% 
                        filter (lice == "AF") %>% 
                        filter (scenario != "10 percent connectivity") %>% 
                        filter(), aes(x=year, y=value, fill=scenario)) +
  geom_boxplot(outlier.size = 0.5) +
  scale_fill_manual(values = color_palette2) +
  labs(y = "", x = "Year", fill = "Scenario") +
  theme_bw() + 
  theme(text=element_text(size=16)) +
  labs(title="Mean number of adult female lice per fish",
       x ="Year", y = "Yearly average")
AF_boxplot

## Boxplot for the other mobile lice
OM_boxplot = ggplot(boxplot_df %>% 
                      filter (stat == "year_mean") %>% 
                      filter (lice == "OM") %>% 
                      filter (scenario != "10 percent connectivity") %>% 
                      filter(), aes(x=year, y=value, fill=scenario)) +
  geom_boxplot(outlier.size = 0.5) +
  scale_fill_manual(values = color_palette2) +
  labs(y = "", x = "Year", fill = "Scenario") +
  theme_bw() + 
  theme(text=element_text(size=16)) +
  labs(title="Mean number of other motile lice per fish",
       x ="Year", y = "Yearly average")
OM_boxplot

## Boxplot illustrating the number of fish in farms treated
Treatment_boxplot = ggplot(boxplot_df %>% 
                      filter (stat == "year_mean") %>% 
                      filter (lice == "treatment") %>% 
                      filter (scenario != "10 percent connectivity") %>% 
                      filter(), aes(x=year, y=value, fill=scenario)) +
  geom_boxplot(outlier.size = 0.5) +
  scale_fill_manual(values = color_palette2) +
  labs(y = "", x = "Year", fill = "Scenario") +
  theme_bw() + 
  theme(text=element_text(size=16)) +
  labs(title="Treatments",
       x ="Year", y = "Yearly average")
Treatment_boxplot

# ## boxplot for both stages and the treatments
# year_boxplot = ggplot(boxplot_df %>%
#                         filter (stat == "year_mean") %>%
#                         filter(), aes(x=year, y=value, fill=scenario)) +
#   geom_boxplot(outlier.size = 0.5) +
#   #geom_boxplot(outlier.shape = NA) +
#   scale_fill_manual(values = color_palette) +
#   facet_wrap(stat~lice, scales="free", nrow=1) +
#   labs(y = "", x = "Year", fill = "Population") +
#   theme_bw() +
#   theme(text=element_text(size=18))
# year_boxplot
# ###
