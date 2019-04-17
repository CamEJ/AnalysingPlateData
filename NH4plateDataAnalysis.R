
# Script for analysing NH4 data from spectrophotometer

library(plater)

# set working directory which contains 2 files
# file 1: your ODs as they came from the plate reader - just save as a csv file
# to see how this file should look, file the DemoPlate_NH4_ODs.csv file in this repository
# file 2: well IDs. This is a csv file where you put in what is in each well
# to see how this file should look, file the DemoPlate_NH4_key.csv file in this repository
# file 3: treatments file.  This is a text file (or csv) where you put in what treatments
# our sample ID correspond to.
# to see how this file should look, file the Demo_NH4_treatments.txt file in this repository

setwd("~/Google Drive/R/") # set your wkdir.

DataPlate = read_plate("DemoPlate_NH4_ODs.csv", well_ids_column = "Wells") # ie file 1 above
TmtPlate = read_plate("DemoPlate_NH4_key.csv", well_ids_column = "Wells") # ie file 2 above

data = merge(DataPlate, TmtPlate, by = "Wells")
colnames(data)[2] <- "OD" # rename column 2 to OD
colnames(data)[3] <- "ID" # rename column 3 to ID

# identify which data correspond to blanks
blank = data[grep("blank", data$ID), ]

# remove blanks from the dataset and put in new dataset called 'keeptied' 
keepties = data[-grep("blank", data$ID), ]

# subract blank from all and put data in a new column called corrOD
keepties$corrOD = with(keepties, OD - mean(blank$OD) )

# read in treatment details. 
Tmts = read.table('Demo_NH4_treatments.txt', header=T, sep='\t') # read in data. 
# this is a text document detailing what each of your sample treatmetns are
# eg: it should lok something like this:
# head(Tmts)
# ID         Treatment
# 1 T0          TimeZero
# 2 A1   NegativeControl
# 3 A2   NegativeControl
# 4 A3   NegativeControl
# 5 B1 Untreated_24hours
# 6 B2 Untreated_24hours


# add treatments to plate data via merge()
fulldata = merge(keepties, Tmts, by = "ID")
# set factor order for plotting later
fulldata$Treatment <- factor(fulldata$Treatment, levels = unique(fulldata$Treatment)) # set order for plotting later

data = fulldata # rename for ease

# subset out standards 
stan = data[grep("Standard", data$Treatment),]

# remove these from dataset now
data = data[-grep("Standard", data$Treatment),]

#check cols are numeric
sapply(stan, class)

# convert column with um conc of NH4 into numeric. 
stan$ID = as.numeric(as.character(stan$ID))

## getting standard curve for OD to NH4

# comput simple linear models from standards
# "mg_nitrogen as modeled by area under curve"
lm.NH4 <- lm(ID ~ corrOD, data=stan)

# check std curve stats: (ie R2 val)
summary(lm.NH4)

# now use the predict function to apply this lm to the df which now
# has standards removed. Will make a new column in 'data' called um_Nh4 in which predicted
# nh4 will be put
data$uM_NH4 <- predict(lm.NH4, data)

# now correct for any dilution factor used (400 here), and will also
# convert from um to mM by / 1000

data$mM_NH4_corr = with (data, (uM_NH4 * 400)/1000)

# write this data out. 
write.csv(data, "myNH4data.csv")

# ========= preparing for plotting ================


# run setFactorOrder.R
# you can find this script in R-basics repository

levels(data$Treatment)

# put in order that you want your factors.
data[["Treatment"]] <- setFactorOrder(data[["Treatment"]], c("timeZero","untreated", "Dose1"))


## =============== Plotting as box plots instead ===========================
library(ggplot2)

PrettyLittleBox = ggplot(data, aes(Treatment, mM_NH4_corr, fill = factor(Treatment))) + # define variables
  geom_boxplot(size=1) + # change line width, 
  scale_fill_manual(name = "", values = c('peru','grey52', 'olivedrab1', 'coral', 'olivedrab2', 'coral3', 
                                          'springgreen3',  'orangered', 'forestgreen', 'darkorange2', 'olivedrab')) +
  theme_bw() +  # theme_bw() to make background white
  labs(x="", y="mM NH4 per vial") +
  theme(axis.title.x = element_blank(),
        text = element_text(size=19, color = "black"),
        axis.text = element_text(color="black"),
        legend.key.size=unit(1.5, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  ) 

PrettyLittleBox

myLabs = c("Time Zero", "Untreated", "Dose 1") # define nicer labels


PrettyLittleBox + scale_x_discrete(labels= myLabs) + guides(fill=FALSE) #+ # turn off legend and change labels on x axis

PrettyLittleBox  + guides(fill=FALSE) #+ # turn off legend and change labels on x axis


## =============== stats ===========================
# will now do an ANOVA to determine if difference is significant. 

res.aov <- aov(mM_NH4_corr ~ Treatment, data = data)
# Summary of the analysis
summary(res.aov)
TukeyHSD(res.aov)


## resources used:
# for preducting um NH4 based on OD:
# https://casoilresource.lawr.ucdavis.edu/software/r-advanced-statistical-package/using-lm-and-predict-apply-standard-curve-analytical-data/


