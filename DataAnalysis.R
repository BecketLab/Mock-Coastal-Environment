library(vegan)
library(phyloseq)
library(ggplot2)
library(ggforce)
library(zCompositions)
library(mixOmics)
library(ggpubr)  
library(corrplot) 
library(psych)    

theme_set(theme_bw())

### Change this to your working drive
setwd("C:/Pearl_Code")
merged_metagenomes <- import_biom("pearl.biom")

### Sample names
samnames <- c("Initial_0_0", "Scripps_S1_1", "Scripps_S2_1", "Scripps_S3_1", "Mock_M1_1", "Mock_M2_1", "Scripps_S1_2", "Scripps_S2_2", "Scripps_S3_2", "Mock_M1_2", "Mock_M2_2")

### Cleaning up phyloseq object
merged_metagenomes@tax_table@.Data <- substring(merged_metagenomes@tax_table@.Data, 7)
colnames(merged_metagenomes@tax_table@.Data)<- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
colnames(merged_metagenomes@otu_table@.Data)<- samnames

### Making sample data frame and adding to new phyloseq object
site <- sapply(strsplit(samnames, "_"), `[`, 1)
samp <- sapply(strsplit(samnames, "_"), `[`, 2)
day <- sapply(strsplit(samnames, "_"), `[`, 3)
sampdf <- data.frame(Site=site, Day=day, Sample=samp)
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

nmds_scores$Day <- sampdf$Day
nmds_scores_p$Day <- sampdf$Day
nmds_scores_g$Day <- sampdf$Day

nmds_scores$Sample <- sampdf$Sample
nmds_scores_p$Sample <- sampdf$Sample
nmds_scores_g$Sample <- sampdf$Sample


#Make plots
plotted_t <- ggplot(data = nmds_scores) +
  ggtitle("Species") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_mark_ellipse(aes(x=MDS1,y=MDS2,fill=Day,color=Day), expand = unit(0.5,"mm")) +
  geom_point(aes(x=MDS1, y=MDS2, shape = Site, color = Day))
plotted_t_p <- ggplot(data = nmds_scores_p) +
  ggtitle("Phylum") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_mark_ellipse(aes(x=MDS1,y=MDS2,fill=Day,color=Day), expand = unit(0.5,"mm")) +
  geom_point(aes(x=MDS1, y=MDS2, shape = Site, color = Day))
plotted_t_g <- ggplot(data = nmds_scores_g) +
  ggtitle("Genus") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_mark_ellipse(aes(x=MDS1,y=MDS2,fill=Day,color=Day), expand = unit(0.5,"mm")) +
  geom_point(aes(x=MDS1, y=MDS2, shape = Site, color = Day))

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

#ANOSIM
ano = anosim(otu_m, sampdf$Day, distance = "euclidean", permutations = 99)
ano_d1 = anosim(stat_d1, sampdf_d1$Site, distance = "euclidean", permutations = 99)
ano_d2 = anosim(stat_d2, sampdf_d2$Site, distance = "euclidean", permutations = 99)

ano
ano_d1
ano_d2

###                             PERMANOVA
stat_ds <- otu_m[2:11, ]
sampdf_ds <- sampdf[2:11, ]

perma = adonis2(formula = stat_all ~ sampdf$Day, 
               permutations = 99,  method = "euclidean")
perma_d1 = adonis2(formula = stat_d1 ~ sampdf_d1$Site,
               permutations = 99,  method = "euclidean")
perma_d2 = adonis2(formula = stat_d2 ~ sampdf_d2$Site,
               permutations = 99,  method = "euclidean")
perma_ds = adonis2(formula = stat_ds ~ sampdf_ds$Day,
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
