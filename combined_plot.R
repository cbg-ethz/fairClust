library(ggplot2)
library(ggpubr)
library(extrafont)

# load data
data_census <- readRDS("results/census.rds")
data_compas <- readRDS("results/compas.rds")

levels(data_census$measure) <- c("TV", "NDE", "NIE", "Exp-SE")
levels(data_compas$measure) <- c("TV", "NDE", "NIE", "Exp-SE")

my_colors <- c("#D73027", "#117777","#708090","#ABD9E9", "#4575B4")

# make subplots and combine them

# Create the plot
p_census <- data_census %>%
  ggplot( aes(x=measure, y=value, fill=Method, colour=Method)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  geom_hline(aes(yintercept = 0), color = "black") +
  scale_colour_manual(values=my_colors) +
  scale_fill_manual(values=my_colors) +
  geom_point(pch = 21, position = position_jitterdodge(0.15),cex=0.4, alpha = 0.3)+ 
  theme_minimal() +
  xlab("Fairness Measure")+
  ylab("Value")+ 
  theme(legend.position = "none");p_census

# Create the plot
p_compas <- data_compas %>%
  ggplot( aes(x=measure, y=value, fill=Method, colour=Method)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  geom_hline(aes(yintercept = 0), color = "black") +
  scale_colour_manual(values=my_colors) +
  scale_fill_manual(values=my_colors) +
  geom_point(pch = 21, position = position_jitterdodge(0.15),cex=0.4, alpha = 0.3)+ 
  theme_minimal() +
  xlab("Fairness Measure")+
  ylab("Value")+ 
  theme(legend.position = "none");p_compas

ggarrange(p_census,p_compas, common.legend = TRUE, legend="bottom", labels = letters[1:2])
# ggarrange(p_census,p_compas, common.legend = TRUE, legend="bottom", labels = c("A: Census", "B: Compas"))




loadfonts()
pdf("~/Desktop/census_compas_plot.pdf", width = 8, height = 5,
    family = "Arial", paper = "special", onefile = FALSE)
# family = "Times New Roman", paper = "special", onefile = FALSE)
op <- par(mar = c(5, 4, 0.05, 0.05) + 0.1)
ggarrange(p_census,p_compas, common.legend = TRUE, legend="bottom", labels = letters[1:2])
par(op)
dev.off()

# without second version

my_colors <- c("#D73027","#708090","#ABD9E9", "#4575B4")

# remove second test version
data_census <- data_census[-which(data_census$Method=="Causally Fair (SE)"),]
data_compas <- data_compas[-which(data_compas$Method=="Causally Fair (SE)"),]

# make subplots and combine them

# Create the plot
p_census <- data_census %>%
  ggplot( aes(x=measure, y=value, fill=Method, colour=Method)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  geom_hline(aes(yintercept = 0), color = "black") +
  scale_colour_manual(values=my_colors) +
  scale_fill_manual(values=my_colors) +
  geom_point(pch = 21, position = position_jitterdodge(0.15),cex=0.4, alpha = 0.3)+ 
  theme_minimal() +
  xlab("Fairness Measure")+
  ylab("Value")+ 
  theme(legend.position = "none");p_census

# Create the plot
p_compas <- data_compas %>%
  ggplot( aes(x=measure, y=value, fill=Method, colour=Method)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  geom_hline(aes(yintercept = 0), color = "black") +
  scale_colour_manual(values=my_colors) +
  scale_fill_manual(values=my_colors) +
  geom_point(pch = 21, position = position_jitterdodge(0.15),cex=0.4, alpha = 0.3)+ 
  theme_minimal() +
  xlab("Fairness Measure")+
  ylab("Value")+ 
  theme(legend.position = "none");p_compas

ggarrange(p_census,p_compas, common.legend = TRUE, legend="bottom", labels = letters[1:2])
# ggarrange(p_census,p_compas, common.legend = TRUE, legend="bottom", labels = c("A: Census", "B: Compas"))




loadfonts()
pdf("~/Desktop/census_compas_plot2.pdf", width = 8, height = 5,
    family = "Arial", paper = "special", onefile = FALSE)
# family = "Times New Roman", paper = "special", onefile = FALSE)
op <- par(mar = c(5, 4, 0.05, 0.05) + 0.1)
ggarrange(p_census,p_compas, common.legend = TRUE, legend="bottom", labels = letters[1:2])
par(op)
dev.off()