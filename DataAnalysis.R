library(vegan)
library(phyloseq)
library(ggplot2)
library(ggforce)
library(zCompositions)
library(mixOmics)
library(ggpubr)  
library(corrplot) 
library(psych)
library(ggtext)

theme_set(theme_bw())

### Change this to your working drive
setwd("C:/Pearl_Code")
merged_metagenomes <- import_biom("pearl.biom")

### Sample names
samnames <- c("Initial_0_0", "In situ_S1_24", "In situ_S2_24", "In situ_S3_24", "In vitro_M1_24", "In vitro_M2_24", "In situ_S1_48", "In situ_S2_48", "In situ_S3_48", "In vitro_M1_48", "In vitro_M2_48")

### Cleaning up phyloseq object
merged_metagenomes@tax_table@.Data <- substring(merged_metagenomes@tax_table@.Data, 7)
colnames(merged_metagenomes@tax_table@.Data)<- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
colnames(merged_metagenomes@otu_table@.Data)<- samnames

### Making sample data frame and adding to new phyloseq object
site <- sapply(strsplit(samnames, "_"), `[`, 1)
samp <- sapply(strsplit(samnames, "_"), `[`, 2)
day <- sapply(strsplit(samnames, "_"), `[`, 3)
sampdf <- data.frame(Site=site, Hour=day, Sample=samp)
rownames(sampdf) <- samnames
ps =  phyloseq(otu_table = merged_metagenomes@otu_table, 
               tax_table = merged_metagenomes@tax_table,
               sample_data(sampdf))

#Aggregate biom table to phylum and genus levels
glom_phy <- tax_glom(ps, taxrank="Phylum")
glom_gen <- tax_glom(ps, taxrank="Genus")


###                         Centered log ratio transformation
#Replace zeros 
otu_z_p <- cmultRepl(as.matrix(glom_phy@otu_table), output = 'p-counts')
otu_z_g <- cmultRepl(as.matrix(glom_gen@otu_table), output = 'p-counts')
otu_z <- cmultRepl(as.matrix(ps@otu_table), output = 'p-counts')

#create clr function
clr <- function(x) sweep(log(x), 1, rowMeans(log(x)), "-")

#Transpose OTU tables
otu_tx <- data.frame(t(clr(t(otu_z))))
otu_tx_p <- data.frame(t(clr(t(otu_z_p))))
otu_tx_g <- data.frame(t(clr(t(otu_z_g))))

###                Beta Diversity

#To matrix
otu_m <- as.matrix(t(otu_tx))
otu_m_p <- as.matrix(t(otu_tx_p))
otu_m_g <- as.matrix(t(otu_tx_g))

#Make and extract NMDS scores
nmds = metaMDS(otu_m, distance = "euclidean")
nmds_p = metaMDS(otu_m_p, distance = "euclidean")
nmds_g = metaMDS(otu_m_g, distance = "euclidean")

nmds_scores = as.data.frame(nmds$points)
nmds_scores_p = as.data.frame(nmds_p$points)
nmds_scores_g = as.data.frame(nmds_g$points)

nmds_scores$Site <- sampdf$Site
nmds_scores_p$Site <- sampdf$Site
nmds_scores_g$Site <- sampdf$Site

nmds_scores$Hour <- sampdf$Hour
nmds_scores_p$Hour <- sampdf$Hour
nmds_scores_g$Hour <- sampdf$Hour

nmds_scores$Samples <- c("Initial", "In situ 24h", "In situ 24h", "In situ 24h", "In vitro 24h", "In vitro 24h", "In situ 48h", "In situ 48h", "In situ 48h", "In vitro 48h", "In vitro 48h")
nmds_scores_p$Samples <- c("Initial", "In situ 24h", "In situ 24h", "In situ 24h", "In vitro 24h", "In vitro 24h", "In situ 48h", "In situ 48h", "In situ 48h", "In vitro 48h", "In vitro 48h")
nmds_scores_g$Samples <- c("Initial", "In situ 24h", "In situ 24h", "In situ 24h", "In vitro 24h", "In vitro 24h", "In situ 48h", "In situ 48h", "In situ 48h", "In vitro 48h", "In vitro 48h")

colnames(nmds_scores) <- c("NMDS1", "NMDS2", "Site", "Hour", "Samples")

#Make plots
plotted_t <- ggplot(data = nmds_scores) +
  ggtitle(" ")  +
  geom_mark_ellipse(aes(x=NMDS1,y=NMDS2,fill=Samples), expand = unit(0.5,"mm")) +
  geom_point(aes(x=NMDS1, y=NMDS2, shape = Site, color = Hour)) +
  scale_fill_discrete(labels = c(expression(italic("In situ")~"24h"), expression(italic("In situ")~"48h"), 
                                expression(italic("In vitro")~"24h"), expression(italic("In vitro")~"48h"), "Initial")) +
  scale_shape_discrete(labels = c(expression(italic("In situ")), expression(italic("In vitro")), "Initial")) +
  theme(legend.text.align = 0)

plotted_t_p <- ggplot(data = nmds_scores_p) +
  ggtitle("Phylum") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_mark_ellipse(aes(x=MDS1,y=MDS2,fill=Hour,color=Hour), expand = unit(0.5,"mm")) +
  geom_point(aes(x=MDS1, y=MDS2, shape = Site, color = Hour))

plotted_t_g <- ggplot(data = nmds_scores_g) +
  ggtitle("Genus") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_mark_ellipse(aes(x=MDS1,y=MDS2,fill=Hour,color=Hour), expand = unit(0.5,"mm")) +
  geom_point(aes(x=MDS1, y=MDS2, shape = Site, color = Hour))

#Display plots
plotted_t
plotted_t_p
plotted_t_g

###                             Stats: ANOSIM
stat_all <- otu_m
stat_d1 <- otu_m[2:6, ]
sampdf_d1 <- sampdf[2:6, ]
stat_d2 <- otu_m[7:11, ]
sampdf_d2 <- sampdf[7:11, ]
stat_ds <- otu_m[2:11, ]
sampdf_ds <- sampdf[2:11, ]

#ANOSIM
ano = anosim(otu_m, sampdf$Hour, distance = "euclidean", permutations = 99)
ano_d1 = anosim(stat_d1, sampdf_d1$Site, distance = "euclidean", permutations = 99)
ano_d2 = anosim(stat_d2, sampdf_d2$Site, distance = "euclidean", permutations = 99)
ano_ds = anosim(stat_ds, sampdf_ds$Hour, distance = "euclidean", permutations = 99)

ano
ano_d1
ano_d2
ano_ds

###                             PERMANOVA

perma = adonis2(formula = stat_all ~ sampdf$Hour, 
               permutations = 99,  method = "euclidean")
perma_d1 = adonis2(formula = stat_d1 ~ sampdf_d1$Site,
               permutations = 99,  method = "euclidean")
perma_d2 = adonis2(formula = stat_d2 ~ sampdf_d2$Site,
               permutations = 99,  method = "euclidean")
perma_ds = adonis2(formula = stat_ds ~ sampdf_ds$Hour,
               permutations = 99,  method = "euclidean")
  
perma
perma_d1
perma_d2
perma_ds

###                     Taxa Bar graphs
#Rename unclassified genus as "unknown"
merged_metagenomes <- subset_taxa(merged_metagenomes, Genus != "")
plotnames <- c("I", "S1.1", "S1.2", "S1.3", "M1.1", "M1.2", "S2.1", "S2.2", "S2.3", "M2.1", "M2.2")
colnames(merged_metagenomes@otu_table@.Data)<- plotnames
#change abundance to percentages

percentages  = transform_sample_counts(merged_metagenomes, function(x) x*100 / sum(x) )

glom_p <- tax_glom(percentages, taxrank = 'Phylum')
glom_g <- tax_glom(percentages, taxrank = 'Genus')

phyla <- psmelt(glom_p)
genus <- psmelt(glom_g)

phyla$Phylum[phyla$Abundance < 0.5] <- "Phyla < 0.5% abund."
genus$Genus[genus$Abundance < 2] <- "Genus < 2% abund."

phyla$Sample <- factor(phyla$Sample, levels=plotnames)
genus$Sample <- factor(genus$Sample, levels=plotnames)

phy.plot <- ggplot(data=phyla, aes(x=Sample, y=Abundance, fill=Phylum)) +
  geom_bar(aes(), stat="identity", position="stack")
gen.plot <- ggplot(data=genus, aes(x=Sample, y=Abundance, fill=Genus)) +
  geom_bar(aes(), stat="identity", position="stack")

phy.plot
gen.plot
