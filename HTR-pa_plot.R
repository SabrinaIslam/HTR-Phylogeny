# setting dir
setwd("C:/Users/sabrinai/OneDrive - The University of Melbourne/PHD/Chapter1/Paper/") 

getwd()

##############################################################################

# used packages

library(ggplot2)
library(ggthemes)
library(dplyr)
library(report)


##############################################################################
# complicated data frame with read.csv, where I have to yell at R not to coerce strings to factors. I also make sure there is not junk text with file encoding, add a sep, add headers to be true.

# HTR_Data <- read.csv("data\\HTR_data.csv", header=TRUE, sep=",", stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")

# Or, I can use the read_csv from tidyverse, which already knows csv means command seperated, add header and does not corece factors into strings.  

HTR_Data <- read.csv("input/HTR_data.csv")

str(HTR_Data)

#############################################################################
# Programmatically adding columns with a formula 
## DRY
# HTR_Data <- mutate(HTR_Data, R = D+1)
# HTR_Data <- mutate(HTR_Data, Y = D+2)
# HTR_Data <- HTR_Data[,c(1,2,3,4,5,6,7,8,9,12,13,10,11)]
# ## NPxxY
# HTR_Data <- mutate(HTR_Data, P = N+1)
# HTR_Data <- mutate(HTR_Data, xxY = N+4)
# HTR_Data <- HTR_Data[,c(1,2,3,4,5,6,7,8,9,10,11,12,13, 14,16,17,15)]
# 
# ## rewriting the csv
# write.csv(HTR_Data, file = "HTR_data.csv")

# re-input data frame
# HTR_Data <- read.csv(file="C:\\Users\\sabrinai\\OneDrive - The University of Melbourne\\PHD\\Phyl4\\4.Data\\HTR_data.csv", header=TRUE, sep=",", stringsAsFactors = FALSE, fileEncoding="UTF-8-BOM")

##############################################################################  # Making the Name column a character vector, then turning it back into a factor with the levels in the correct order so ggplot does not order them alphabatically 

HTR_Data$Name <- factor(HTR_Data$Name, levels = unique (HTR_Data$Name))

HTR_Data$Species <- factor(HTR_Data$Species, levels = unique (HTR_Data$Species))

class(HTR_Data$Name)

class(HTR_Data$Species)

##############################################################################  

# remvove Dro seq

HTR_vert_data <- HTR_Data %>% filter(Species != "Drosophila_melanogaster_(drosophila)")  

HTR_vert_data

class(HTR_vert_data$Species)

write.csv(HTR_vert_data, "input/HTR_vert_data.csv")

# SPECIES SHORTHAND 
species_shrt <- c(
  "Homo_sapiens_(human)" = "Hsap",
  "Pan_troglodytes_(chimpanzee)" = "Ptro",
  "Gorilla_gorilla_(western_gorilla)" = "Ggor",
  "Macaca_mulatta_(Rhesus_monkey)" = "Mmul",
  "Macaca_fascicularis_(crab_eating_macaque)" = "Mfas",
  "Callithrix_jacchus_(white_tufted_marmoset)" = "Cjac",
  "Microcebus_murinus_(gray_mouse_lemur)" = "Mmur",
  "Mus_musculus_(house_mouse)" = "Mmus",
  "Rattus_norvegicus_(rat)" = "Rnor", 
  "Physeter_catodon_(sperm_whale)" = "Pcat",
  "Bos_taurus_(cow)" = "Btau",
  "Capra_hircus_(goat)" = "Chir",
  "Ovis_aries_(sheep)" = "Oari",
  "Sus_scrofa_(pig)" = "Sscr", 
  "Oryctolagus_cuniculus_(rabbit)" = "Ocun",
  "Canis_lupus_(dog)" = "Cfam",
  "Felis_catus_(cat)" = "Fcat",
  "Numida_meleagris_(helmeted_guineafowl)" = "Nmel",
  "Gallus_gallus_(chicken)" = "Ggal",
  "Xenopus_tropicalis_(tropical_clawed_frog)" = "Xtro",
  "Xiphophorus_maculatus_(southern_platyfish)" = "Xmac",
  "Danio_reriro_(zebrafish)" = "Drer"
)


HTR_vert_data$Species <- as.character(species_shrt[ HTR_vert_data$Species])

class(HTR_vert_data$Species)

HTR_vert_data$Species <- factor(HTR_vert_data$Species, levels = unique (HTR_vert_data$Species))

class(HTR_vert_data$Species)

#############################################################################

# values

distance <- c(0.2,
              6.5, 
              8.6, 
              28.8, 
              28.8,
              42.9, 
              74.1, 
              88.6, 
              88.6,
              94.3,
              94.3,
              94.3,
              94.3,
              94.3,
              88.6,
              94.3,
              94.3,
              318.4,
              318.4,
              351.7, 
              432.5,
              432.5)


HTR_vert_data$col22 <- distance[HTR_vert_data$Species]

levels(HTR_vert_data$Species)

############################################################
## subsetting each sequences for motif plotting and PA

# subsetting 5-HTR 1A
HTR1A_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR1A")
str(HTR1A_vert_data)

class(HTR1A_vert_data$Species)

# subsetting 5-HTR 1B
HTR1B_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR1B")
str(HTR1B_vert_data)


# subsetting 5-HTR 1D
HTR1D_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR1D")
str(HTR1D_vert_data)

# subsetting 5-HTR 1E
HTR1E_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR1E")
str(HTR1E_vert_data)

# subsetting 5-HTR 1F
HTR1F_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR1F")
str(HTR1F_vert_data)

# subsetting 5-HTR 2A
HTR2A_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR2A")
str(HTR2A_vert_data)

# new 2A without wird points
HTR2A_new_data <- HTR2A_vert_data[-c(2, 4, 7, 9, 12),]
str(HTR2A_new_data)

# subsetting 5-HTR 2B
HTR2B_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR2B")
str(HTR2B_vert_data)

# new 2B without wird points
HTR2B_new_data <- HTR2B_vert_data[-c(2, 4, 6, 8, 11, 13, 20, 22, 24),]
str(HTR2B_new_data)

# subsetting 5-HTR 2C
HTR2C_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR2C")
str(HTR2C_vert_data)

# subsetting 5-HTR 4
HTR4_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR4")
str(HTR4_vert_data)


# subsetting 5-HTR 5A
HTR5A_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR5A")
str(HTR5A_vert_data)

# subsetting 5-HTR 5B
HTR5B_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR5B")
str(HTR5B_vert_data)

# subsetting 5-HTR 6
HTR6_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR6")
# remove the rebbit seq which hurts data 
HTR6_vert_data <- HTR6_vert_data[-c(15),]
str(HTR6_vert_data)

# subsetting 5-HTR 7
HTR7_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR7")
str(HTR7_vert_data)

#########################################################################
class(HTR_Data$Species)

class(HTR1A_vert_data$Species)

class(HTR1B_vert_data$Species)

# plotting
# aes x and y are different for each line, so ggplot() is kep empty.
# coord cartesian to set the limit for Y axis
# labs need to be just here, after it doesn't output. 
# X lab is taxa, Y lab is Identity %, legend lab is Rcepetor, not color. 
# geom_point for each data points of similairty. color is set to the data.
# geom_line for joining the points. Group set to 1, as each group consists just 1 observation.
# geom_hline(yintercept=80, linetype="dashed", color = "red") creates a red dashed line across 80% Identity
# scale_color_manual() to map color of each line plot
# theme_tufte for a clean looking theme
#   theme(axis.line = element_line(),  axis.text.x = element_text(angle = 90, vjust=0.6, hjust=1), panel.background = element_blank()): remove axis line, remove background, angle taxa name 90 degree for a better fit, hjust set to 0 to align bottom, vjust set to 0.6 to place under the ticks.

##############################################################################

con_1 <-
ggplot() +
  coord_cartesian (ylim = c(0,100), xlim = c(0, 500)) +
  scale_x_continuous(breaks = seq(0, 500, 50)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  labs(x = "Evolutionary distance (Mya)", y ="Identity %", color= "Receptor")+
  # HTR1A
  geom_point(data = HTR1A_vert_data, aes(x = col22, y = Identity, color = "HTR1A") ) +
  geom_smooth(data = HTR1A_vert_data, aes(x = col22, y = Identity, color = "HTR1A"),
              method = "lm", alpha = 0.1) +
  #HTR1B
  geom_point(data = HTR1B_vert_data, aes(x = col22, y = Identity, color = "HTR1B") ) +
  geom_smooth(data = HTR1B_vert_data, aes(x = col22, y = Identity, color = "HTR1B"),
              method = "lm", alpha = 0.1) +
  #HTR1D
  geom_point(data = HTR1D_vert_data, aes(x = col22, y = Identity, color = "HTR1D") ) +
  geom_smooth(data = HTR1D_vert_data, aes(x = col22, y = Identity, color = "HTR1D"),
              method = "lm", alpha = 0.1) +
  #HTR1E
  geom_point(data = HTR1E_vert_data, aes(x = col22, y = Identity, color = "HTR1E") ) +
  geom_smooth(data = HTR1E_vert_data, aes(x = col22, y = Identity, color = "HTR1E"),
              method = "lm", alpha = 0.1) +
  #HTR1E
  geom_point(data = HTR1E_vert_data, aes(x = col22, y = Identity, color = "HTR1F") ) +
  geom_smooth(data = HTR1E_vert_data, aes(x = col22, y = Identity, color = "HTR1F"),
              method = "lm", alpha = 0.1) +
  scale_color_manual(values = c("HTR1A" = "#034e7b", 
                                "HTR1B" = "#0570b0", 
                                "HTR1D" = "#3690c0", 
                                "HTR1E" = "#74a9cf",
                                "HTR1F" = "#a6bddb")) + 
  theme_tufte() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1) ,
        text = element_text(family = "Arial", size = 14),
        axis.line = element_line(),  
        panel.background = element_blank(),
        legend.position = "right") 

con_1

con_2 <- 
ggplot() +
  coord_cartesian (ylim = c(0,100), xlim = c(0, 500)) +
  scale_x_continuous(breaks = seq(0, 500, 50)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  labs(x = "Evolutionary distance (Mya)", y ="Identity %", color= "Receptor")+
  #HTR2A
  geom_point(data = HTR2A_new_data, aes(x = col22, y = Identity, color = "HTR2A") ) +
  geom_smooth(data = HTR2A_new_data, aes(x = col22, y = Identity, color = "HTR2A"),
              method = "lm", alpha = 0.1) +
  #HTR2B
  geom_point(data = HTR2B_new_data, aes(x = col22, y = Identity, color = "HTR2B") ) +
  geom_smooth(data = HTR2B_new_data, aes(x = col22, y = Identity, color = "HTR2B"),
              method = "lm", alpha = 0.1) +
  #HTR2C
  geom_point(data = HTR2C_vert_data, aes(x = col22, y = Identity, color = "HTR2C") ) +
  geom_smooth(data = HTR2C_vert_data, aes(x = col22, y = Identity, color = "HTR2C"),
              method = "lm", alpha = 0.1) +
  scale_color_manual(values = c("HTR2A" = "#41ab5d", 
                                "HTR2B" = "#005a32",
                                "HTR2C" = "#a1d99b")) + 
  theme_tufte() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1) ,
        text = element_text(family = "Arial", size = 14),
        axis.line = element_line(),  
        panel.background = element_blank(),
        legend.position = "right") 
con_2  

con_3 <- 
  ggplot() +
    coord_cartesian (ylim = c(0,100), xlim = c(0, 500)) +
    scale_x_continuous(breaks = seq(0, 500, 50)) +
    scale_y_continuous(breaks = seq(0, 100, 10)) +
    labs(x = "Evolutionary distance (Mya)", y ="Identity %", color= "Receptor")+
  #HTR4
  geom_point(data = HTR4_vert_data, aes(x = col22, y = Identity, color = "HTR4") ) +
  geom_smooth(data = HTR4_vert_data, aes(x = col22, y = Identity, color = "HTR4"),
              method = "lm", alpha = 0.1) +
  #HTR6
  geom_point(data = HTR6_vert_data, aes(x = col22, y = Identity, color = "HTR6") ) +
  geom_smooth(data = HTR6_vert_data, aes(x = col22, y = Identity, color = "HTR6"),
              method = "lm", alpha = 0.1) +
  #HTR7
  geom_point(data = HTR7_vert_data, aes(x = col22, y = Identity, color = "HTR7") ) +
  geom_smooth(data = HTR7_vert_data, aes(x = col22, y = Identity, color = "HTR7"),
              method = "lm", alpha = 0.1) +
  scale_color_manual(values = c("HTR4" = "#E69F00", 
                                "HTR6" = "#F0E442", 
                                "HTR7" = "#CC79A7")) + 
  theme_tufte() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1) ,
        text = element_text(family = "Arial", size = 14),
        axis.line = element_line(),  
        panel.background = element_blank(),
        legend.position = "right") 

con_3


#############################################################################

motifs <- as.vector(colnames(HTR_vert_data)[9:18])

class(motifs)

for (i in 1:10) {
  motif_i <- as.data.frame(HTR_vert_data[1:352, colnames(HTR_vert_data) %in% motifs[i]])
  
  distance_i <- as.data.frame(HTR_vert_data[1:352, "Distance"])
  
  data_i <- cbind(distance_i, motif_i)
  
  res_aov_i <- oneway.test(data_i[, 2]
                           ~ data_i[, 1],
                         data = data_i,
                         var.equal = FALSE)

  res_p_i <- res_aov_i$p.value

  print(res_p_i)

}

