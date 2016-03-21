# Prepare the LSR data for analysis.
# Copyright (c) 2016 Defenders of Wildlife, jmalcom@defenders.org

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
library(lubridate)
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
dens_dat <- read_excel(dens_fil)
crit_dat <- read_excel(crit_fil)
resil_dat <- read_excel(resil_fil)

# Take a look at the heads
head(field_dat)
head(dens_dat)
head(crit_dat)
head(resil_dat)

# Set data types ------------------------------
# field_dat
field_dat$Competitor <- as.factor(field_dat$Competitor)
field_dat$Distance <- as.factor(field_dat$Distance)

# dens_dat
dens_dat$Density <- ifelse(dens_dat$Density == "Hi",
                           "High",
                           "Low")
dens_dat$Density <- as.factor(dens_dat$Density)

# crit_dat
crit_dat$Density <- ifelse(crit_dat$Density == "H",
                           "High",
                           "Low")
crit_dat$Treat <- as.factor(crit_dat$Treat)
crit_dat$Density <- as.factor(crit_dat$Density)

# resil_dat
resil_dat$Density <- ifelse(resil_dat$Density == "Hi",
                            "High",
                            "Low")
resil_dat$Treat <- as.factor(resil_dat$Treat)
resil_dat$Density <- as.factor(resil_dat$Density)

# Change some variable names for ease-of-use ------------------------------
# field_dat
names(field_dat)[7] <- "perp45"
names(field_dat)[8] <- "perp135"
names(field_dat)[9] <- "para45"
names(field_dat)[10] <- "para135"
names(field_dat)[11] <- "deg90"

# dens_dat
names(dens_dat)[3] <- "Leaf_count"

# crit_dat
names(crit_dat)[4] <- "Days_to_Crit"
names(crit_dat)[5] <- "Resist_start"
names(crit_dat)[6] <- "Critical_date"
names(crit_dat)[7] <- "Resilience_end"


save(field_dat, file="data/field_dat.RData")
save(dens_dat, file="data/dens_dat.RData")
save(crit_dat, file="data/crit_dat.RData")
save(resil_dat, file="data/resil_dat.RData")

