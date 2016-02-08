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
library(ggplot2)
library(ggthemes)
library(readxl)

# Set the data paths ------------------------------
resil_fil <- "data/resilience_experiment_data.xlsx"
crit_fil <- "data/resilience_days_to_critical.xlsx"
dens_fil <- "data/resilience_leaf_density_data.xlsx"

# Load the data ------------------------------
resil_dat <- read_excel(resil_fil)
crit_dat <- read_excel(crit_fil)
dens_fil <- read_excel(dens_fil)

# Take a look at the heads
head(resil_dat)
head(crit_dat)
head(dens_fil)

# Some basic plots --------------------
apl <- ggplot(data=dens_fil, aes(x=Density, y=`Leaf count`)) +
        geom_violin(fill = "lightsteelblue2", color = "white") +
        labs(x="\nLeaf Density Treatment",
             y=expression("Leaf count (25 "*cm^-2*")")) +
        theme_pander()
apl








