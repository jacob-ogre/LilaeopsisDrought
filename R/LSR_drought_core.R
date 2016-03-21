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

source("R/multiplot.R")

###############################################################################
# Load the data; prepped in data_prep.R
###############################################################################
load(file="data/field_dat.RData")
load(file="data/dens_dat.RData")
load(file="data/crit_dat.RData")
load(file="data/resil_dat.RData")

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


###############################################################################
# Now for the models -------------------
# first the global model
mod1 <- zeroinfl(LSRD ~ Distance + Soilmoist + canopy_PC1 + canopy_PC2 + Centroid,
                 data = field_dat, dist = "negbin", EM = TRUE)
summary(mod1)
AICc(mod1)

# Two models dropping one of the soil moisture / creek distance measures
mod2 <- zeroinfl(LSRD ~ Soilmoist + canopy_PC1 + canopy_PC2 + Centroid,
                 data = field_dat, dist = "negbin", EM = TRUE)
summary(mod2)
AICc(mod2)

mod3 <- zeroinfl(LSRD ~ Distance + canopy_PC1 + canopy_PC2 + Centroid,
                 data = field_dat, dist = "negbin", EM = TRUE)
summary(mod3)
AICc(mod3)

# two models for moisture and history, slightly different
mod4 <- zeroinfl(LSRD ~ Distance + Centroid,
                 data = field_dat, dist = "negbin", EM = TRUE)
summary(mod4)
AICc(mod4)

mod5 <- zeroinfl(LSRD ~ Soilmoist + Centroid,
                 data = field_dat, dist = "negbin", EM = TRUE)
summary(mod5)
AICc(mod5)

# a model ignoring our proxy for history
mod6 <- zeroinfl(LSRD ~ Soilmoist + canopy_PC1 + canopy_PC2,
                 data = field_dat, dist = "negbin", EM = TRUE)
summary(mod6)
AICc(mod6)

# two models for just the canopy (unlikely)
mod7 <- zeroinfl(LSRD ~ canopy_PC1 + canopy_PC2,
                 data = field_dat, dist = "negbin", EM = TRUE)
summary(mod7)
AICc(mod7)

# two models for just the canopy (unlikely)
mod8 <- zeroinfl(LSRD ~ perp45 + perp135 + para45 + para135 + deg90,
                 data = field_dat, dist = "negbin", EM = TRUE)
summary(mod8)
AICc(mod8)

# Now to do the weighting and formal comparison
candidates <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8)
AICc_table <- aictab(candidates)
to_write_tab <- data.frame(AICc_table)
write.table(to_write_tab,
            file = "results/field_ecol_models_AICc.tsv",
            sep = "\t",
            quote = FALSE,
            row.names = FALSE)

###################################
# a couple of post-hoc model considerations given the results above
modP1 <- zeroinfl(LSRD ~ Soilmoist + Centroid | Centroid,
                 data = field_dat, dist = "negbin", EM = TRUE)
summary(modP1)
AICc(modP1)

# this is the model with the lowest AIC
modP2 <- zeroinfl(LSRD ~ Distance + Centroid | Centroid,
                 data = field_dat, dist = "negbin", EM = TRUE)
summary(modP2)
AICc(modP2)

candidates <- list(mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, modP1, modP2)
AICc_table <- aictab(candidates)
to_write_tab <- data.frame(AICc_table)
write.table(to_write_tab,
            file = "results/field_ecol_models_AICc_withPost.tsv",
            sep = "\t",
            quote = FALSE,
            row.names = FALSE)


###############################################################################
# And now for the plots for the paper, I think

# First, presence/absence as a function of distance from the nearest centroid
pdf(file = "results/Figure1_prob_occurrence.pdf", height = 6, width = 6)
logi.hist.plot(field_dat$Centroid, 
               field_dat$LSRPA, 
               boxp = FALSE,
               type = "hist",
               xlabel = "Distance to patch centroid (m)",
               ylabel = "Probability of presence",
               ylabel2 = "(Absent)       Obs. Frequency       (Present)",
               col = "lightsteelblue2")
dev.off()

# Second, LSR density as a function of distance to the creek
all_dens <- ggplot(data = field_dat, aes(x = Distance, y = LSRD)) +
                geom_violin(fill = "lightsteelblue2", color = "lightsteelblue2") +
                geom_jitter(alpha = 0.4, size = 3, width = 0.4) +
                labs(title = "All sampled sites",
                     x = "Distance to edge of creek (m)",
                     y = expression("Leaf count (25 "*cm^-2*")")) +
                theme_hc()

pos_dens <- field_dat[field_dat$LSRPA == 1, ]
pos_dens <- ggplot(data = pos_dens, aes(x = Distance, y = LSRD)) +
                geom_violin(fill = "lightsteelblue2", color = "lightsteelblue2") +
                geom_jitter(alpha = 0.4, size = 4, width = 0.4) +
                labs(title = "Present only",
                     x = "Distance to edge of creek (m)",
                     y = expression("Leaf count (25 "*cm^-2*")")) +
                theme_hc()

dist_moist <- ggplot(data = field_dat, aes(x = Distance, y = Soilmoist * 100)) +
                  geom_violin(fill = "lightsteelblue2", color = "lightsteelblue2") +
                  geom_jitter(alpha = 0.4, size = 4, width = 0.4) +
                  labs(title = "Moisture vs. distance",
                       x = "Distance to edge of creek (m)",
                       y = "Soil moisture (percent)") +
                  theme_hc()

pdf(file = "results/Figure2_density.pdf", height = 6, width = 12)
multiplot(all_dens, pos_dens, dist_moist, cols = 3)
dev.off()


###############################################################################
# Analyses and plots: Drought resistance and resilience
###############################################################################

# check the experimental leaf density factors
t.test(Leaf_count ~ Density, data = dens_dat)

###################################
# Drought resistance
# Days-to-critical analysis
sub_crit <- crit_dat[crit_dat$Treat != "Control", ]
mod <- lm(Days_to_Crit ~ Treat + Density, data = sub_crit)
summary(mod)
hist(resid(mod))
aov(mod)
AICc(mod1)

mod2 <- lm(Days_to_Crit ~ Treat * Density, data = sub_crit)
summary(mod2)
hist(resid(mod2))
summary(aov(mod2))
AICc(mod2)

mod2_coef <- data.frame(summary(mod2)$coefficients)
write.table(mod2_coef, 
            file = "results/resistance_coef_table.tsv")

# Days-to-critical figure
crit_panel <- ggplot(data = sub_crit, aes(x = Density, y = Days_to_Crit)) +
                  geom_violin(fill = "lightsteelblue2", color = "lightsteelblue2") +
                  geom_jitter(alpha = 0.4, size = 4, width = 0.4, height = 0) +
                  labs(x = "\nLeaf density treatment",
                       y = "Time to critical level (days)\n") +
                  facet_wrap(~Treat) +
                  theme_hc()
crit_panel

pdf(file = "results/Figure3_resist.pdf", height = 6, width = 6)
crit_panel
dev.off()

###################################
# Drought resilience
# Analyze resilience
sub_resil <- resil_dat[resil_dat$Treat != "Control", ]
mod <- lm(Condition ~ Density + Treat + Day, data = sub_resil)
summary(mod)
summary(aov(mod))
hist(resid(mod))
AICc(mod)

mod2 <- lm(Condition ~ Density * Day + Treat, data = sub_resil)
summary(mod2)
summary(aov(mod2))
hist(resid(mod2))
AICc(mod2)

mod3 <- lm(Condition ~ Density * Day * Treat, data = sub_resil)
summary(mod3)
aov(mod3)
hist(resid(mod3))
AICc(mod3)

mod3_coef <- data.frame(summary(mod3)$coefficients)
write.table(mod3_coef, 
            file = "results/resilience_coef_table.tsv")

# Plot resilience
resil_panel <- ggplot(data = sub_resil, aes(x = Day, y = Condition, colour = Density)) +
                   geom_smooth(method = "lm", alpha = 0.2) +
                   geom_jitter(alpha = 0.4, size = 2, width = 0.2, height = 0.2) +
                   scale_colour_brewer(palette = "Set1",
                                       guide = guide_legend(title = "Leaf density")) +
                   labs(x = "Days post-critical",
                        y = "Condition index\n") +
                   facet_wrap(~Treat) +
                   theme_hc() +
                   theme(legend.position = "top" )
resil_panel

pdf(file = "results/Figure4_resilience.pdf", height = 6, width = 10)
resil_panel
dev.off()



