#load ggplot2 data vis package
library(ggplot2)

#load the caTools package containing the trapz trapezoidal integral function
library(caTools)

#load fast CSV file reading package data.table containing fread function
library(data.table)

#load the rawPPFD dataframe with the CSV data, change path to point to your file
rawPPFD <- fread("path/to/data/File")

#plot the data to make sure all is well, using a classic line graph aesthetic
ggplot(rawPPFD, aes(x=seconds,y=ppfd)) + geom_line() + theme_classic()

#compute the integral of the plotted curve. Seconds on X-axis, PPFD on Y-axis
DLI_micromoles <- trapz(rawPPFD$seconds,rawPPFD$ppfd)

#convert the micromoles to moles for standardized measurement by diviging by one million
DLI = DLI_micromoles / 1000000
