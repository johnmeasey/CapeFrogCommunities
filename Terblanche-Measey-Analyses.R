# Naas' survey data
# Analysis as of October 2022


library(vegan)
library(ggplot2)
library(ggvegan)
library(ggpubr)
library(plot3D)
library(RColorBrewer)
library(vegan3d)
library(scatterplot3d)
library(dplyr)
library(readr)
library(rlist)
library(tibble)
library(rgl)
library(GGally)

data <- read.csv("Naas_T-data.csv")
frog_data <- data[,19:29] 
va_data <- data[,4:17]

summary(frog_data)
summary(va_data)

# First test for correlations between pairs of habitat variables

GGally::ggpairs(va_data)

# Conductivity, Tilapia, Lm Bass, area,  removed

var_data <- va_data[, -c(6, 9, 10, 11, 13, 14)]
summary(var_data)
str(var_data)


#We used decostand to transform the anuran data to presence/absence
frog_pa <- decostand(frog_data, "pa")


# We used Jaccard dissimilarity because of the presence absence nature of data. We tried k2, 3 and 4
pa.nmds.k2 <- metaMDS(frog_pa, trymax = 100, k = 2, distance = "jaccard", autotransform = F)
pa.nmds.k3 <- metaMDS(frog_pa, trymax = 100, k = 3, distance = "jaccard", autotransform = F)
pa.nmds.k4 <- metaMDS(frog_pa, trymax = 100, k = 4, distance = "jaccard", autotransform = F)

#Test to see how many dimensions the nmds should have
pa.nmds.k2$stress
pa.nmds.k3$stress
pa.nmds.k4$stress

ismorethan0.05 <- (pa.nmds.k4$stress - pa.nmds.k3$stress)
ismorethan0.05
ismorethan0.050 <- (pa.nmds.k3$stress - pa.nmds.k2$stress)
ismorethan0.050
#  3D is better than 2D as stress drops by 0.062, but 4D only decreases stress by 0.03. Critical change acceptable is 0.05

rm(pa.nmds.k4)
rm(pa.nmds.k2)

# Plot stress accumulation for pa.nmds.k3

stressplot(pa.nmds.k3)

# Fortify pa.nmds.k3
# gives co-ordinates for site and species centroids useful for plotting (later)
fort <- fortify(pa.nmds.k3)
view(fort)


# Test for environmental variables against the best nmds result
fit3D <- envfit(pa.nmds.k3, var_data, permu=9999)
choices=c(1:3)
fit3D
scores(fit3D, "vectors", choices = 1:2)
# Outputs can be found in Table S2 of Suppl Info


#Test for which species are driving the differences
fit.spp <- envfit(pa.nmds.k3, frog_data, permu = 9999)
fit.spp
scores(fit.spp, "vectors")
# Outputs can be found in Table S3 of Suppl Info

######################## Figure 2 ##############################
# Plot the sites with the species by Wetland Type

Fig2a <- ggplot() +
    stat_ellipse(data = subset(fort, Score == 'sites'), 
                 aes(x = NMDS1, y = NMDS2, 
                     fill=var_data$Wetland.type,
                     colour = var_data$Wetland.type), 
                 geom="polygon", level = 0.5, alpha=0.2) +
    geom_point(data = subset(fort, Score == 'sites'),
               mapping = aes(x = NMDS1, y = NMDS2, colour = var_data$Wetland.type),
               alpha = 0.5)+
    geom_segment(data = subset(fort, Score == 'species'),
                 mapping = aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                 arrow = arrow(length = unit(0.015, "npc"),
                               type = "closed"),
                 colour = "darkgray",
                 size = 0.4,
                 alpha = 0.5)+
    geom_text(data = subset(fort,Score == 'species'),
              mapping = aes(label = Label, x = NMDS1 * 1.1, y = NMDS2 * 1.1)) +
    geom_abline(intercept = 0,slope = 0,linetype = "dashed", size=0.8, colour="gray") +
    geom_vline(aes(xintercept=0), linetype ="dashed", size=0, colour="gray") +
    theme(legend.position = "none")
  
  Fig2a
  
  Fig2b <- ggplot() +
    stat_ellipse(data = subset(fort, Score == 'sites'), 
                 aes(x = NMDS2, y = NMDS3, 
                     fill=var_data$Wetland.type,
                     colour = var_data$Wetland.type), 
                 geom="polygon", level = 0.5, alpha=0.2) +
    geom_point(data = subset(fort, Score == 'sites'),
               mapping = aes(x = NMDS2, y = NMDS3, colour = var_data$Wetland.type),
               alpha = 0.5)+
    geom_segment(data = subset(fort, Score == 'species'),
                 mapping = aes(x = 0, y = 0, xend = NMDS2, yend = NMDS3),
                 arrow = arrow(length = unit(0.015, "npc"),
                               type = "closed"),
                 colour = "darkgray",
                 size = 0.4)+
    geom_text(data = subset(fort,Score == 'species'),
              mapping = aes(label = Label, x = NMDS2 * 1.1, y = NMDS3 * 1.1)) +
    geom_abline(intercept = 0,slope = 0,linetype = "dashed", size=0.8, colour="gray") +
    geom_vline(aes(xintercept=0), linetype ="dashed", size=0, colour="gray") +
    theme(legend.position = "none")
  
  Fig2b
  


# Plot temporary vs permanent wetland types

Fig2c <- ggplot() +
  stat_ellipse(data = subset(fort, Score == 'sites'), 
               aes(x = NMDS1, y = NMDS2, 
                   fill=var_data$temporary,
                   colour = var_data$temporary), 
               geom="polygon", level = 0.5, alpha=0.2) +
  geom_point(data = subset(fort, Score == 'sites'),
             mapping = aes(x = NMDS1, y = NMDS2, colour = var_data$temporary),
             alpha = 0.5)+
  geom_segment(data = subset(fort, Score == 'species'),
               mapping = aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.015, "npc"),
                             type = "closed"),
               colour = "darkgray",
               size = 0.8)+
  geom_text(data = subset(fort,Score == 'species'),
            mapping = aes(label = Label, x = NMDS1 * 1.1, y = NMDS2 * 1.1)) +
  geom_abline(intercept = 0,slope = 0,linetype = "dashed", size=0.8, colour="gray") +
  geom_vline(aes(xintercept=0), linetype ="dashed", size=0, colour="gray") +
  theme(legend.position = "none")

Fig2c

Fig2d <- ggplot() +
  stat_ellipse(data = subset(fort, Score == 'sites'), 
               aes(x = NMDS2, y = NMDS3, 
                   fill=var_data$temporary,
                   colour = var_data$temporary), 
               geom="polygon", level = 0.5, alpha=0.2) +
  geom_point(data = subset(fort, Score == 'sites'),
             mapping = aes(x = NMDS2, y = NMDS3, colour = var_data$temporary),
             alpha = 0.5)+
  geom_segment(data = subset(fort, Score == 'species'),
               mapping = aes(x = 0, y = 0, xend = NMDS2, yend = NMDS3),
               arrow = arrow(length = unit(0.015, "npc"),
                             type = "closed"),
               colour = "darkgray",
               size = 0.8)+
  geom_text(data = subset(fort,Score == 'species'),
            mapping = aes(label = Label, x = NMDS2 * 1.1, y = NMDS3 * 1.1)) +
  geom_abline(intercept = 0,slope = 0,linetype = "dashed", size=0.8, colour="gray") +
  geom_vline(aes(xintercept=0), linetype ="dashed", size=0, colour="gray") +
  theme(legend.position = "none")

Fig2d


ggsave("Fig2.pdf", width = 150, height = 150, units = "mm")
ggarrange(Fig2a,Fig2b,Fig2c,Fig2d, ncol = 2,nrow = 2)
dev.off()

############################ Analyses on reduced dataset of permanent water bodies ###############################
# Because invasive fish can only occur in permanent water bodies, we carried out an adonis analysis on fish within permanent water only.

data_nf <- subset(data, var_data$temporary=="perm")
frog_data_nf <- data_nf[,19:29] 
va_data_nf <- data_nf[,4:17]

summary(frog_data_nf)
summary(va_data_nf)

var_data_nf <- va_data_nf[, -c(6, 9, 10, 11)]
frog_pa_nf <- decostand(frog_data_nf, "pa")


# We used Jaccard dissimilarity because of the presence absence nature of data. We tried k2, 3 and 4
pa.nf.nmds.k2 <- metaMDS(frog_pa_nf, trymax = 100, k = 2, distance = "jaccard", autotransform = F)
pa.nf.nmds.k3 <- metaMDS(frog_pa_nf, trymax = 100, k = 3, distance = "jaccard", autotransform = F)
pa.nf.nmds.k4 <- metaMDS(frog_pa_nf, trymax = 100, k = 4, distance = "jaccard", autotransform = F)

#Test to see how many dimensions the nmds should have
pa.nf.nmds.k2$stress
pa.nf.nmds.k3$stress
pa.nf.nmds.k4$stress

nfismorethan0.05 <- (pa.nf.nmds.k4$stress - pa.nf.nmds.k3$stress)
nfismorethan0.05
#  2D is better than 3D as stress drops by 0.043. Critical change acceptable is 0.05


rm(pa.nf.nmds.k3)
rm(pa.nf.nmds.k4)


fish.perm.nf <- adonis(frog_pa_nf ~ var_data_nf$Fish, data = var_data_nf)  
fish.perm.nf
View(fish.perm.nf)
coef(fish.perm.nf)
summary(fish.perm.nf)
print(fish.perm.nf)



fort.nf <- fortify(pa.nf.nmds.k2)


NMDS_nf <- ggplot(pa.nf.nmds.k2, aes(x = NMDS1, y = NMDS2)) + geom_point(aes(NMDS1, NMDS2, colour = var_data_nf$Fish), size = 4) + labs(colour = "Presence or absence of invasive fish")
NMDS_nf

###################### Figure 3 #############################
# plot NMDS of sites with permanent water with or withouth invasive fish

Fig3 <- ggplot() +
  stat_ellipse(data = subset(fort.nf, Score == 'sites'), 
               aes(x = NMDS1, y = NMDS2, 
                   fill=var_data_nf$Fish,
                   colour = var_data_nf$Fish), 
               geom="polygon", level = 0.5, alpha=0.2) +
  geom_point(data = subset(fort.nf, Score == 'sites'),
             mapping = aes(x = NMDS1, y = NMDS2, colour = var_data_nf$Fish),
             alpha = 0.5)+
  geom_segment(data = subset(fort.nf, Score == 'species'),
               mapping = aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.015, "npc"),
                             type = "closed"),
               colour = "darkgray",
               size = 0.4)+
  geom_text(data = subset(fort.nf,Score == 'species'),
            mapping = aes(label = Label, x = NMDS1 * 1.1, y = NMDS2 * 1.1)) +
  geom_abline(intercept = 0,slope = 0,linetype = "dashed", size=0.8, colour="gray") +
  geom_vline(aes(xintercept=0), linetype ="dashed", size=0, colour="gray") +
  theme(legend.position = "none")+
  coord_fixed(xlim = c(-1,1), ylim = c(-1,1))

Fig3

ggsave("Fig3.pdf", width = 75, height = 75, units = "mm")
ggarrange(Fig3, ncol = 1,nrow = 1)
dev.off()





############################# 3D plots #################################3
# 3D plots of pa.nmds.k3 are available in the Suppl information 
# code was taken with thanks from https://github.com/natearoe/myrepo/blob/master/Jessicas_class_presentation.Rmd

my.colors <- brewer.pal(4, "Accent")
par3d(windowRect = c(100, 100, 912, 912))
ordirgl(pa.nmds.k3, size = 4)
orgltext(pa.nmds.k3, display = "species")
legend3d("topright", legend = sort(unique(var_data$Wetland.type)), pch = 16, col = my.colors[1:4], inset = c(0.02))
rglwidget()


# 3D plot for wetland type
my.colors <- brewer.pal(4, "Accent")
par3d(windowRect = c(100, 100, 912, 912))
ordirgl(pa.nmds.k3, size = 4, col = my.colors[as.numeric(var_data$Wetland.type)])
orgltext(pa.nmds.k3, display = "species")
orglspider(pa.nmds.k3, var_data$Wetland.type, col = my.colors[1:4])
orglellipse(pa.nmds.k3, var_data$Wetland.type, col = my.colors[1:4], kind = "se", conf = .95, scaling = "sites")
legend3d("topright", legend = sort(unique(var_data$Wetland.type)), pch = 16, col = my.colors[1:4], inset = c(0.02))
rglwidget()
# ellipses are 95% confidence intervals

# 3D plot for temporary or permanent
my.colors <- brewer.pal(4, "Accent")
par3d(windowRect = c(100, 100, 912, 912))
ordirgl(pa.nmds.k3, size = 4, col = my.colors[as.numeric(var_data$temporary)])
orgltext(pa.nmds.k3, display = "species")
orglspider(pa.nmds.k3, var_data$temporary, col = my.colors[1:4])
orglellipse(pa.nmds.k3, var_data$temporary, col = my.colors[1:4], kind = "se", conf = .95, scaling = "sites")
legend3d("topright", legend = sort(unique(var_data$temporary)), pch = 16, col = my.colors[1:4], inset = c(0.02))
rglwidget()
# ellipses are 95% confidence intervals

# 3D plot for fish vs no fish in water
my.colors <- brewer.pal(4, "Accent")
par3d(windowRect = c(100, 100, 912, 912))
ordirgl(pa.nf.nmds.k2, size = 4, col = my.colors[as.numeric(var_data_nf$Fish)])
orgltext(pa.nf.nmds.k3, display = "species")
orglspider(pa.nf.nmds.k3, var_data_nf$Fish, col = my.colors[1:4])
orglellipse(pa.nmds.k3, var_data_nf$Fish, col = my.colors[1:4], kind = "se", conf = .95, scaling = "sites")
legend3d("topright", legend = sort(unique(var_data_nf$Fish)), pch = 16, col = my.colors[1:4], inset = c(0.02))
rglwidget()
# ellipses are 95% confidence intervals  
#with(var_data, ordirgl(pa.nmds.k3, col = as.numeric(var_data$Fish), scaling = "sites"))

############### Shannin diversity for each site + species accumulation curves ##############################

#gives Shannon diversity index for each site.
diversity(frog_data, index = "shannon")

#produces a species accumulation curve for the frog data at 50 sites. This suggests that there are unlikely to be more in the area than the 11 species that were found
spa <- specaccum(frog_data)
plot(spa)

