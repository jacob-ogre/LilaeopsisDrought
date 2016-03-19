# Basic exploration of the data because it's been nine years.
# Copyright (c) 2016 Jacob Malcom, jmalcom@defenders.org

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program; if not, see <http://www.gnu.org/licenses/>.
#

library(dplyr)
library(FactoMineR)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(pscl)
library(readxl)

###############################################################################
# Data import and setup
###############################################################################

# Set the data paths ------------------------------
field_fil <- "data/field_ecology_data.xlsx"
resil_fil <- "data/resilience_experiment_data.xlsx"
crit_fil <- "data/resilience_days_to_critical.xlsx"
dens_fil <- "data/resilience_leaf_density_data.xlsx"

# Load the data ------------------------------
field_dat <- read_excel(field_fil)
resil_dat <- read_excel(resil_fil)
crit_dat <- read_excel(crit_fil)
dens_dat <- read_excel(dens_fil)

# Take a look at the heads
head(field_dat)
head(resil_dat)
head(crit_dat)
head(dens_dat)

# Set data types ------------------------------
# field_dat
field_dat$Competitor <- as.factor(field_dat$Competitor)
field_dat$Distance <- as.factor(field_dat$Distance)
field_dat$LSRPA <- as.factor(field_dat$LSRPA)

# resil_dat
resil_dat$Treat <- as.factor(resil_dat$Treat)
resil_dat$Density <- as.factor(resil_dat$Density)

# crit_dat
crit_dat$Treat <- as.factor(crit_dat$Treat)
crit_dat$Density <- as.factor(crit_dat$Density)

# dens_dat
dens_dat$Density <- as.factor(dens_dat$Density)

# Change some variable names for ease-of-use ------------------------------
# field_dat
names(field_dat)[7] <- "45perp"
names(field_dat)[8] <- "135perp"
names(field_dat)[9] <- "45para"
names(field_dat)[10] <- "135para"
names(field_dat)[11] <- "90deg"

###############################################################################
# Analyses and plots: Field ecology
###############################################################################

# Need to check multicollinearity of canopy vars--------------------
cor(field_dat[,7:11])
can_pca <- PCA(field_dat[,7:11], graph = FALSE)
can_pca$eig
# PCs 1 & 2 account for 58% of variance, so will use those
field_dat$canopy_PC1 <- can_pca$ind$coord[,1]
field_dat$canopy_PC2 <- can_pca$ind$coord[,2]

# Some basic plots --------------------
plot.PCA(can_pca, choix = "var", col.var = "gray50",
         title = "Canopy Cover Biplot")

# distribution of LSR densities
qplot(data = field_dat, x = LSRD, geom = "histogram") + theme_hc()

# soil moisture as a function of distance from the creek
qplot(data = field_dat, x = Distance, y = Soilmoist, geom = "boxplot") + 
theme_hc()

# LSR density as a function of competitors
ggplot(data = field_dat, aes(x = Competitor, y = LSRD)) +
    geom_violin(fill = "lightsteelblue2", color = "lightsteelblue2") +
    geom_jitter(alpha = 0.4, size = 4) +
    theme_hc()

# Now for the models -------------------
mod1 <- zeroinfl(LSRD ~ Soilmoist + canopy_PC1 + canopy_PC2 + Centroid,
                 data = field_dat, dist = "negbin", EM = TRUE)
summary(mod1)
AICc(mod1)

mod2 <- zeroinfl(LSRD ~ Distance + Soilmoist + canopy_PC1 + canopy_PC2 + Centroid | Centroid,
                 data = field_dat)
summary(mod2)
AICc(mod2)

mod3 <- zeroinfl(LSRD ~ Distance + Soilmoist + Canavg + Centroid | Centroid,
                 data = field_dat)
summary(mod3)
AICc(mod3)

mod2 <- zeroinfl(LSRD ~ Distance + Centroid,
                 data = field_dat, dist = "negbin", EM = TRUE)
summary(mod2)

mod22 <- zeroinfl(LSRD ~ Competitor,
                 data = field_dat, dist = "negbin", EM = TRUE)
summary(mod22)

###############################################################################
# Analyses and plots: Drought resistance and resilience
###############################################################################

# check the experimental leaf density factors
apl <- ggplot(data=dens_dat, aes(x=Density, y=`Leaf count`)) +
        geom_violin(fill = "lightsteelblue2", color = "lightsteelblue2") +
        geom_jitter(width = 0.2, alpha = 0.4, size = 4) +
        labs(x="\nLeaf Density Treatment",
             y=expression("Leaf count (25 "*cm^-2*")")) +
        theme_hc()
apl








