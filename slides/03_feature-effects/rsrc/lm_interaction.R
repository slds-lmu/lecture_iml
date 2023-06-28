# PREREQ -----------------------------------------------------------------------

load("../../../data/bike.RData")

# DATA -------------------------------------------------------------------------

lm.mod = lm(cnt ~ temp*season, data = bike) 

# PLOT -------------------------------------------------------------------------

pdf(file = "../figure/lm_interaction.pdf", width = 8, height = 3)
plot(effects::allEffects(lm.mod), layout = c(4, 1))
dev.off()
