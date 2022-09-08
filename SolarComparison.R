#load ggplot2 data vis package
library(ggplot2)

#load the caTools package containing the trapz trapezoidal integral function
library(caTools)

#load fast CSV file reading package data.table containing fread function
library(data.table)

#load the rawPPFD dataframe with the CSV data, change path to point to your file
rawSpectrum <- fread("path/to/CSVfile")

#plot the data to make sure all is well, using a classic line graph aesthetic
ggplot(rawSpectrum, aes(x=Wavelength,y=Sunlight)) + geom_line() + theme_classic()

#plot the data to make sure all is well, using a classic line graph aesthetic
ggplot(rawSpectrum, aes(x=Wavelength,y=LEDNoon)) + geom_line() + theme_classic()

#compute the integral of the plotted curve. Wavelength on X-axis, Sunlight normalized intensity on Y-axis
sunlightIntegral <- trapz(rawSpectrum$Wavelength,rawSpectrum$Sunlight)

#compute the integral of the plotted curve. Wavelength on X-axis, LED Noon normalized intensity on Y-axis
LEDNoonIntegral <- trapz(rawSpectrum$Wavelength,rawSpectrum$LEDNoon)

#compute percent our LED lights are with respect to integral of sunlight from 400nm to 700nm
percentOfSunlight = (LEDNoonIntegral/sunlightIntegral)*100
