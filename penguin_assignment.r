
#Load the data 

library(palmerpenguins)
library(ggplot2)
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(ragg)
library(svglite)
library(car)
setwd("~/PenguinProject")

penguins_raw_1 <- read.csv("~/PenguinProject//palmerpenguins/extdata/penguins_raw.csv")

#Appropriately clean the data
cleaning <- function(data_raw){
  data_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments)
  }

penguins_clean_1 <- cleaning(penguins_raw_1)

#Subset the data to only include the penguins that are not NA for Sex

remove_empty_Sex <- function(data_clean){
  data_clean %>%
    filter(!is.na(sex)) %>%
    select(species, sex, body_mass_g)
}


penguins_sex_mass <- remove_empty_Sex(penguins_clean_1)

Gentoo_sex_mass <- filter(penguins_sex_mass, species == ("Gentoo penguin (Pygoscelis papua)"))

#New: Run a statistical test, unpaired t-test between sex and body mass in Gentoo penguins 

#First check assumptions
##Assumption 1: Because we are not given any indication of the opposite, we will assume the samples were random

##Assumption 2:check for normality in each group 

normality_check = ggplot(Gentoo_sex_mass, aes(x = body_mass_g)) + geom_histogram(bins=15) + facet_wrap(~sex)

plot(normality_check)
##data looks roughly normally distributed 

##Assumption 3: check for variance homogeneity through a Levene's test

leveneTest(data = Gentoo_sex_mass, body_mass_g ~ sex, centre = mean)

##P-value is >0.05 so the variances are not significantly different from each other 

##Run T-test

t.test(data = Gentoo_sex_mass, body_mass_g ~ sex, var.equal = TRUE)

#Create a figure
plot_Gentoo_figure <- function(Gentoo_sex_mass) {
  Gentoo_sex_mass %>%
    ggplot(aes(x = body_mass_g, fill=sex)) + 
    geom_histogram(alpha=0.3, color="black", bins = 18, position = "identity") + 
    labs(x = "Body mass (g)", y = "Number of penguins") + theme_classic()
}
dev.off()

Gentoo_histogram <- plot_Gentoo_figure(Gentoo_sex_mass)
plot(Gentoo_histogram)

#Save the figure for a report  
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

# Save the plot for a presentation
save_Gentoo_plot_png(Gentoo_histogram, "figures/fig01_powerpoint.png", 
                      size = 15, res = 600, scaling = 1.4)

# Save the plot for a poster
save_Gentoo_plot_png(Gentoo_histogram, "figures/fig01_poster.png", 
                      size = 30, res = 600, scaling = 2.8)
# Save the plot as a vector (no resolution needed)

library(svglite)

save_Gentoo_plot_svg <- function(Gentoo_sex_mass, filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width = size_inches, height = size_inches, scaling = scaling)
  print(Gentoo_histogram)
  dev.off()
}

save_Gentoo_plot_svg(Gentoo_histogram, "figures/fig01_vector.svg", 
                      size = 15, scaling = 1)

