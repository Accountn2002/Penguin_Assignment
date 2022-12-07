## Script name: penguin_assignment.r

## Purpose of script:
## Loading Palmer Penguins data, cleaning it to see if there is a significant difference between female and 
## male body mass in Gentoo penguins. Create figures for this statistical test that can be used for different purposes
## Research Question: Is body mass correlated to gender in Gentoo penguins? 

#1. Set working directory and load the data 

library(palmerpenguins)
library(ggplot2)
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(ragg)
library(svglite)
library(car)

## set working directory 
setwd("~/PenguinProject")

##load the data 
penguins_raw_1 <- read.csv("~/PenguinProject//palmerpenguins/extdata/penguins_raw.csv")

# 2. Appropriately clean the data 
## after creating a function to remove empty rows and remove "delta" and "comment" columns

cleaning <- function(data_raw){
  data_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)
  }

penguins_clean_1 <- cleaning(penguins_raw_1)

# 3. Subset the data to only include the Gentoo penguins that are not NA for Sex 
## remove data that is NA for sex 

remove_empty_Sex <- function(data_clean){
  data_clean %>%
    filter(!is.na(sex)) %>%
    select(species, sex, body_mass_g)
}


penguins_sex_mass <- remove_empty_Sex(penguins_clean_1)

##create  data set with only Gentoo penguins and visualize it 
Gentoo_sex_mass <- filter(penguins_sex_mass, species == ("Gentoo penguin (Pygoscelis papua)"))

Gentoo_sex_mass

# 4. Run a statistical test, in this case an unpaired t-test between sex and body mass in Gentoo penguins 

##First check assumptions

##Assumption 1: Because we are not given any indication of the opposite, we will assume the samples were random

##Assumption 2:check for normality in each group 

normality_check = ggplot(Gentoo_sex_mass, aes(x = body_mass_g)) + geom_histogram(bins=15) + facet_wrap(~sex)

plot(normality_check)

###data looks roughly normally distributed 

##Assumption 3: check for variance homogeneity through a Levene's test

leveneTest(data = Gentoo_sex_mass, body_mass_g ~ sex, centre = mean)

##P-value is >0.05 so the variances are not significantly different from each other, meaning we can go ahead with the test

##Run T-test

t.test(data = Gentoo_sex_mass, body_mass_g ~ sex, var.equal = TRUE)
  
  ## We are given a p-value of < 2.2e-16, meaning our results are statistically significant
  ##Body mass and gender are correlated in Gentoo penguins 

# 5. Create an appropriate figure, in this case a histogram 

plot_Gentoo_figure <- function(Gentoo_sex_mass) {
  Gentoo_sex_mass %>%
    ggplot(aes(x = body_mass_g, fill=sex)) + 
    geom_histogram(alpha=0.3, color="black", bins = 18, position = "identity") + 
    labs(x = "Body mass (g)", y = "Number of penguins") + theme_classic()
}
dev.off()

Gentoo_histogram <- plot_Gentoo_figure(Gentoo_sex_mass)
plot(Gentoo_histogram)

# 6. Save figure for different purposes 
## Save the figure for a report  
library(ragg)

save_Gentoo_plot_png <- function(Gentoo_sex_mass, filename, size, res, scaling){
  agg_png(filename, width = size, 
          height = size, 
          units = "cm", 
          res = res, 
          scaling = scaling)
  print(Gentoo_histogram)
  dev.off()
}

save_Gentoo_plot_png(Gentoo_histogram, "figures/fig01_report.png", 
                      size = 15, res = 600, scaling = 1)

## Save the plot for a presentation
save_Gentoo_plot_png(Gentoo_histogram, "figures/fig01_powerpoint.png", 
                      size = 15, res = 600, scaling = 1.4)

## Save the plot for a poster
save_Gentoo_plot_png(Gentoo_histogram, "figures/fig01_poster.png", 
                      size = 30, res = 600, scaling = 2.8)

## Save the plot as a vector (no resolution needed)

library(svglite)

save_Gentoo_plot_svg <- function(Gentoo_sex_mass, filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width = size_inches, height = size_inches, scaling = scaling)
  print(Gentoo_histogram)
  dev.off()
}

save_Gentoo_plot_svg(Gentoo_histogram, "figures/fig01_vector.svg", 
                      size = 15, scaling = 1)

