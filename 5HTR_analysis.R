# setting dir

setwd("C:/Users/sabri/OneDrive - The University of Melbourne/PHD/Chapter1/Paper")

getwd()

#################################################################################

# # tidyverse and ggplot2 libraries not loading 
# 
#   ## checking library locations 
#     
#     .libPaths( "C:/Users/sabri/OneDrive/Documents/R/win-library/3.5")
# 
#     print(.libPaths())
# 
#   ## checking dependencies 
#       
#     loadedNamespaces()
# 
# 
#   ## remove tidyverse 
#     
#     remove.packages ("tidyverse", lib = "C:/Users/sabri/OneDrive/Documents/R/win-library/3.5")
#     
#   ## install broom
#     
#     install.packages("broom") # all new packages as such as  dplyr got reinstalled but broom did not 
#     
#     install.packages("tidyverse::broom") #package 'tidyverse::broom' is not available (for R version 3.5.2
#     
#     install.packages("broom", dependencies=TRUE) #installed a bunch of stuff except "broom 
#     
#     library(broom) # yep, no broom 
# 
#     install.packages("tidyverse", dependencies=TRUE) # still not working 
# 
#     library(tidyverse) # i hate my life 
#     
#     install.packages("zeallot") # zeallot for broom... broom appreard and disppared 
#     
#     library(zeallot) 
#     
#     install.packages("broom")
#     
#     install.packages("tidyverse", dependencies=TRUE) 
#     
#     library(tidyverse) # still not working aaahhhhh
#     
#     install.packages("backports") 
#     
#     remove.packages ("tidyverse", lib = "C:/Users/sabri/OneDrive/Documents/R/win-library/3.5")
#     
#     install.packages("broom") # again broom appeared and disappeared 
#     
#     install.packages("tidyverse", dependencies=TRUE)
#     
#     library(tidyverse) # f*ck 
#     
#     install.packages("generics")
#     
#     install.packages("methods") # base package already 
#     
#     install.packages("nlme")
#     
#     # "purrr", "reshape2", "stringr", "tibble" already installed 
#     
#     remove.packages ("tidyverse", lib = "C:/Users/sabri/OneDrive/Documents/R/win-library/3.5")
#     
#     install.packages("broom") # again broom appeared and disappeared 
#     
#     install.packages("tidyverse", dependencies=TRUE)
#     
#     library(broom)    
#     
#     library(tidyverse) # f*ck 
#     
#     install.packages("devtools")
#     
#     library(devtools) # no devtools 
#     
#     devtools::install_github("bbolker/broom") # still no devtools 
#     
#     # updated RStudio which installed some packages but still tidyverse not working 
#     
#     # namespace 'dplyr' 0.8.5 is being loaded, but >= 1.0.0 is required
#    
#      packageVersion("dplyr")
#      
#     update.packages("dplyr") # dplyr not updating?
#     
#     #updating R tools
#     
#     update.packages("Rtools")
#     
#     # restarted PC and R
#     
#     # checked R version 
#     
#     # so far the problem seems to be dplyr wrong version -> no broom -> no tidyverse -> no ggplot2
#     
#     install.packages("broom", dependencies=TRUE) # again broom appeared and disappeared 
#     
#     install.packages("tidyverse", dependencies=TRUE)
#     
#     install.packages("dbplyr") # not installed I am desperate now 
#     
#     library(dbplyr)
#     
#     # in an act of sheer desperation I re-installed R new version. Now everything is reloading.
#     
#     # that solved the problem 
#     
#     install.packages("BiocManager")
#     
# BiocManager::install("ggtree")

# phytools not installed 

# install.packages("igraph")
# install.packages("quadprog")
# install.packages("Rtools")
# install.packages("phytools", repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
# BiocManager::install("treeio")
# install.packages("rlang")
# install.packages("dplyr", dependencies=TRUE)
# install.packages("Rcpp", dependencies=TRUE)
#

#################################################################

    # library(data.table)
    # library(htmltools)
    # library(formattable)
    # library(webshot) 
    # library(kableExtra) 
    
    
#################################################################

######## PACKAGES ###############################################

# tidy data 

  library(Rcpp) # for dplyr 
  
  library(dplyr) # for tidyverse

  library(tidyr) #  for organizing data

  library(tidyverse) # for organizing data and ggplot2

# plotting 

  library(ggplot2) # for plotting 

  library(ggthemes) # for applying themes to the plots by gg

  library(cowplot) # for combining figures 

# phylogeny 

  library(ctv) # with ape
  
  library(ape) # for phylo object 

  library(phangorn) # for phylo object

  library(phytools) # for phylo object 

  library(tm) # unsure why

  library(ggtree) # for tree element 

  library(treeio) # for phylo

#################################################################

######## DATA FORMAT & INPUT ####################################

#################################################################

# Reading data in the tidyverse format 

  HTR_Data <- read_csv("input/HTR_data.csv")

  str(HTR_Data) # All columns in the correct format 
  
  class(HTR_Data) # Data frame 


################################################################

# Formatting the data frame
  
# Making the Name column a character vector, then turning it back into a factor with the levels in the correct order so ggplot does not order them alphabetically

  HTR_Data$Name <- factor(HTR_Data$Name, levels = unique (HTR_Data$Name))

  HTR_Data$Species <- factor(HTR_Data$Species, levels = unique (HTR_Data$Species))

  class(HTR_Data$Name) # Name is a factor 

  class(HTR_Data$Species) # Species is a factor 

  str(HTR_Data) # Characters and integers 


  # Removing the Drosophila  seq to build a dataset consisting of just vertebrate sequences 
  
  HTR_vert_data <- HTR_Data %>% filter(Species != "Drosophila_melanogaster_(drosophila)") %>% droplevels()

  str(HTR_vert_data) # Characters and integers

  levels(HTR_vert_data$Species) # All species names are correct and not alphabetically ordered 

  class(HTR_vert_data$Species) # Factor 

  write.csv(HTR_vert_data, "input/HTR_vert_data.csv") # saved as a .csv 


# shorthanding specie for plot labels 

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

  class(species_shrt) # Character

  # Converting species to a factor variable 
  
    HTR_vert_data$Species <- as_factor(species_shrt[ HTR_vert_data$Species])

  class(HTR_vert_data$Species) # Factor 

  HTR_vert_data$Species <- factor(HTR_vert_data$Species, levels = unique (HTR_vert_data$Species)) # Applied "unique" to show the unique values  

  class(HTR_vert_data$Species)  # Factor
  
  levels(HTR_vert_data$Species) # All Species shorthanded 
  

# A vector for the evolutionary distance of each species from human 

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


  # Adding the distances as a new column
  
    HTR_vert_data$col22 <- distance[HTR_vert_data$Species]

    HTR_vert_data$Distance

#################################################################

# subsetting each sequences for plotting motif and PA

# subsetting 5-HTR 1A
  
  HTR1A_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR1A")
  
  str(HTR1A_vert_data)

  class(HTR1A_vert_data$Species) # Factor 

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

# new 5-HTR 2A without the data points from the transcript variant 2
  
  HTR2A_new_data <- HTR2A_vert_data[-c(2, 4, 7, 9, 12),]

  str(HTR2A_new_data)

# subsetting 5-HTR 2B

  HTR2B_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR2B")

  str(HTR2B_vert_data)

# new 2B without the data points from the transcript variant 2
  
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

  # remove the rabbit seq which hurts the data and is PREDICTED: LOW QUALITY PROTEIN

  HTR6_vert_data <- HTR6_vert_data[-c(15),]

  str(HTR6_vert_data)

# subsetting 5-HTR 7

  HTR7_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR7")

  str(HTR7_vert_data)

#################################################################
  
######### SIMILARITY % ##########################################
  
#################################################################
  
# plotting 
  
  # aes x and y are different for each line, so ggplot() is kept empty.
  
  # coord_cartesian to set the limit for Y axis
  
  # X and Y axis are scaled according to the maximum and mimimum of each receptor data 
  
  # labs need to be just here, after it doesn't output. X lab is taxa, Y lab is similarity %, legend lab is Receptor, not colour. 
  
  # geom_point for each data points of similarity. colour is set to the data.
  
  # geom_line for joining the points. Group set to 1, as each group consists of just 1 observation.
  
  # geom_hline(yintercept=80, linetype="dashed", colour = "red") can create a red dashed line across 80% similarity. But we don't want to assign meanings yet
  
  # scale_colour_manual() to map colour of each line plot
  
  # theme_tufte for a clean looking theme
  
  # theme(axis.line = element_line(),  axis.text.x = element_text(angle = 90, vjust=0.6, hjust=1), panel.background = element_blank()): remove axis line, remove background, angle taxa name 90 degree for a better fit, hjust set to 0 to align bottom, vjust set to 0.6 to place under the ticks.

  
# 5-HTR 1s and 5s     

  con_1 <- 
    ggplot() +
    coord_cartesian (ylim = c(65,100), xlim = c(0, 500)) +
    scale_x_continuous(breaks = seq(0, 550, 50)) +
    scale_y_continuous(breaks = seq(65, 100, 10)) +
    labs(x = "Evolutionary distance (Mya)", y ="Similarity %", color= "Receptor")+
    geom_point(data = HTR1A_vert_data, aes(x = col22, y = Similarity, color = "5-HTR1A") ) +
    geom_line(data = HTR1A_vert_data, aes(x = col22, y = Similarity, color = "5-HTR1A"), group = 1)+
    geom_point(data = HTR1B_vert_data, aes(x = col22, y = Similarity, color = "5-HTR1B")) +
    geom_line(data = HTR1B_vert_data, aes(x = col22, y = Similarity, color ="5-HTR1B"), group = 1)+
    geom_point(data = HTR1D_vert_data, aes(x = col22, y = Similarity, color = "5-HTR1D")) +
    geom_line(data = HTR1D_vert_data, aes(x = col22, y = Similarity, color ="5-HTR1D"), group = 1)+
    geom_point(data = HTR1E_vert_data, aes(x = col22, y = Similarity, color = "5-HTR1E")) +
    geom_line(data = HTR1E_vert_data, aes(x = col22, y = Similarity, color ="5-HTR1E"), group = 1)+
    geom_point(data = HTR1F_vert_data, aes(x = col22, y = Similarity, color = "5-HTR1F")) +
    geom_line(data = HTR1F_vert_data, aes(x = col22, y = Similarity, color ="5-HTR1F"), group = 1)+
    scale_color_manual(values = c("5-HTR1A" = "#000000", "5-HTR1B" = "#56B4E9", "5-HTR1D" = "#F0E442", "5-HTR1E" = "#D55E00", "5-HTR1F" = "#009E73")) +
    theme_tufte() +
    theme(axis.line = element_line(),  axis.text.x = element_text(angle = 90, vjust=0.6, hjust=0), panel.background = element_blank()) 
  
  con_1
  
  ggsave("output/con_1.pdf", width = 20, height = 13, units = "cm")
  

# 5-HTR 2s with the clean "new" data sets 

  con_2_new <- 
    ggplot() +
    coord_cartesian (ylim = c(60,100))+
    scale_x_continuous(breaks = seq(0, 550, 50)) +
    scale_y_continuous(breaks = seq(60, 100, 10)) +
    labs(x = "Evolutionary distance (Mya)", y ="Similarity %", color= "Receptor")+
    geom_point(data = HTR2A_new_data, aes(x = col22, y = Similarity, color = "5-HTR2A")) +
    geom_line(data = HTR2A_new_data, aes(x = col22, y = Similarity, color = "5-HTR2A"), group = 1)+
    geom_point(data = HTR2B_new_data, aes(x = col22, y = Similarity, color = "5-HTR2B")) +
    geom_line(data = HTR2B_new_data, aes(x = col22, y = Similarity, color ="5-HTR2B"), group = 1)+
    geom_point(data = HTR2C_vert_data, aes(x = col22, y = Similarity, color = "5-HTR2C")) +
    geom_line(data = HTR2C_vert_data, aes(x = col22, y = Similarity, color ="5-HTR2C"), group = 1)+
    scale_color_manual(values = c("5-HTR2A" = "#000000", "5-HTR2B" = "#56B4E9", "5-HTR2C" = "orange")) +
    theme_tufte() +
    theme(axis.line = element_line(),  axis.text.x = element_text(angle = 90, vjust=0.6, hjust=0), panel.background = element_blank()) 
  
  con_2_new
  
  ggsave("output/con_new_2.pdf", width = 20, height = 13, units = "cm")

# 5-HTR4, 6 and 7 

  con_3 <- 
    ggplot() +
    coord_cartesian (ylim = c(50,100))+
    scale_y_continuous(breaks = seq(50, 100, 10))+
    scale_x_continuous(breaks = seq(0, 550, 50)) +
    labs(x = "Evolutionary distance (Mya)", y ="Similarity %", color= "Receptor")+
    geom_point(data = HTR4_vert_data, aes(x = col22, y = Similarity, color = "5-HTR4")) +
    geom_line(data = HTR4_vert_data, aes(x = col22, y = Similarity, color = "5-HTR4"), group = 1)+
    geom_point(data = HTR6_vert_data, aes(x = col22, y = Similarity, color = "5-HTR6")) +
    geom_line(data = HTR6_vert_data, aes(x = col22, y = Similarity, color ="5-HTR6"), group = 1) +
    geom_point(data = HTR7_vert_data, aes(x = col22, y = Similarity, color = "5-HTR7")) +
    geom_line(data = HTR7_vert_data, aes(x = col22, y = Similarity, color ="5-HTR7"), group = 1)+
    scale_color_manual(values = c("5-HTR4" = "#000000", "5-HTR6" = "#56B4E9", "5-HTR7" = "orange")) +
    theme_tufte() +
    theme(axis.line = element_line(),  axis.text.x = element_text(angle = 90, vjust=0.6, hjust=0), panel.background = element_blank())
  
  con_3

  ggsave("output/con_3.pdf", width = 20, height = 13, units = "cm")
  
#################################################################
  
######### MOTIFS AND MICRO SWITCHES  ############################
  
#################################################################
  
  # ggplot() kept empty
  
  # labs for label 
  
  # scale_y_continuous sets the y value gaps and limits according to the data
  
  # geom line does not give the desired plot, maneuvered geom_bar to be thinner

  # length of receptor shown with one geom_point + geom text 

  # each motif shown with one geom_point + geom text. 
  
  # Each point colour is mapped to the motif name using scale_colour_manual to colour code each motif

  # theme_tufte 

  # text size is 8, Y axis title aligned with vjust and hjust set to 0

  # coord flip to rotate the map 

  # scale_x_discrete(limits = rev(levels())) to order X axis based on evolutionary dist.  

# 1A length and domains

  # finding the top limit for the plot
  
  max(HTR1A_vert_data$Length)
  
  # finding the bottom limit 
  
  min(HTR1A_vert_data$D2.50)
  
  # levels
  
  levels(HTR1A_vert_data$Species)
  
  levels(HTR1A_vert_data$Species) <- c(levels(HTR1A_vert_data$Species), "Drer a", "Drer b")
  
  # renaming double entries 
  
  HTR1A_vert_data[22,"Species"] <- as.character("Drer a")
  
  HTR1A_vert_data[23,"Species"] <- as.character("Drer b")
  
  # returning data frame to factor 
  
  HTR1A_vert_data$Species <- factor(HTR1A_vert_data$Species, levels = unique (HTR1A_vert_data$Species))
  
  class(HTR1A_vert_data$Species)

  # plot 

  HTR1A_motif_plot <- 
    ggplot() +
    labs(title = "5-HTR1A") +
    scale_y_continuous(breaks = seq(74, 430, 50)) +
    geom_bar(data = HTR1A_vert_data, aes(x = Species, y = Length), stat = "identity", width = 0.2, col = "black", fill = "white") +
    geom_text(data = HTR1A_vert_data, aes(x = Species, y = Length, label = Length), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1A_vert_data, aes(x = Species, y = D2.50, colour = "D2.50"), show.legend = FALSE) +
    geom_text(data = HTR1A_vert_data, aes(x =  Species, y = D2.50, label = D2.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1A_vert_data, aes(x =  Species, y = D3.49, colour = "D3.49"), show.legend = FALSE) +
    geom_text(data = HTR1A_vert_data, aes(x =  Species, y = D3.49, label = D3.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1A_vert_data, aes(x =  Species, y = N7.49, colour = "N7.49"), show.legend = FALSE) +
    geom_text(data = HTR1A_vert_data, aes(x =  Species, y = N7.49, label = N7.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1A_vert_data, aes(x =  Species, y = I3.40, colour = "I3.40"), show.legend = FALSE) +
    geom_text(data = HTR1A_vert_data, aes(x =  Species, y = I3.40, label = I3.40), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1A_vert_data, aes(x =  Species, y = P5.50, colour = "P5.50"), show.legend = FALSE) +
    geom_text(data = HTR1A_vert_data, aes(x =  Species, y = P5.50, label = P5.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1A_vert_data, aes(x =  Species, y = F6.44, colour = "F6.44"), show.legend = FALSE) +
    geom_text(data = HTR1A_vert_data, aes(x =  Species, y = F6.44, label = F6.44), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    scale_color_manual(values = c("D3.49" = "#00B81F", "N7.49" = "#00A5FF", "D2.50" = "#FFC425", "I3.40" = "#FC61D5", "P5.50" = "#FC61D5", "F6.44" = "#FC61D5"))+
    theme_tufte() +
    theme(axis.line = element_line(), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 8), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8, hjust = 0, vjust = 0), 
          plot.title = element_text(size = 8), 
          panel.background = element_blank(), 
          legend.text = element_blank(), 
          legend.title = element_blank()) +
    coord_flip(ylim = c(74, 430)) +
    scale_x_discrete(limits = rev(levels(HTR1A_vert_data$Species)))

  HTR1A_motif_plot


# 1B length and domains

  # finding the top limit for the plot
  
  max(HTR1B_vert_data$Length)
  
  # finding the bottom limit 
  
  min(HTR1B_vert_data$D2.50)
  
  # levels
  
  levels(HTR1B_vert_data$Species)
  
  class(HTR1B_vert_data$Species)
  
  HTR1B_motif_plot <- 
    ggplot() +
    labs(title = "5-HTR1B") +
    scale_y_continuous(breaks = seq(74, 410, 50)) +
    geom_bar(data = HTR1B_vert_data, aes(x = Species, y = Length), stat = "identity", width = 0.2, col = "black", fill = "white") +
    geom_text(data = HTR1B_vert_data, aes(x = Species, y = Length, label = Length), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1B_vert_data, aes(x = Species, y = D2.50, colour = "D2.50"), show.legend = FALSE) +
    geom_text(data = HTR1B_vert_data, aes(x =  Species, y = D2.50, label = D2.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1B_vert_data, aes(x =  Species, y = D3.49, colour = "D3.49"), show.legend = FALSE) +
    geom_text(data = HTR1B_vert_data, aes(x =  Species, y = D3.49, label = D3.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1B_vert_data, aes(x =  Species, y = N7.49, colour = "N7.49"), show.legend = FALSE) +
    geom_text(data = HTR1B_vert_data, aes(x =  Species, y = N7.49, label = N7.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1B_vert_data, aes(x =  Species, y = I3.40, colour = "I3.40"), show.legend = FALSE) +
    geom_text(data = HTR1B_vert_data, aes(x =  Species, y = I3.40, label = I3.40), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1B_vert_data, aes(x =  Species, y = P5.50, colour = "P5.50"), show.legend = FALSE) +
    geom_text(data = HTR1B_vert_data, aes(x =  Species, y = P5.50, label = P5.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1B_vert_data, aes(x =  Species, y = F6.44, colour = "F6.44"), show.legend = FALSE) +
    geom_text(data = HTR1B_vert_data, aes(x =  Species, y = F6.44, label = F6.44), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    scale_color_manual(values = c("D3.49" = "#00B81F", "N7.49" = "#00A5FF", "D2.50" = "#FFC425", "I3.40" = "#FC61D5", "P5.50" = "#FC61D5", "F6.44" = "#FC61D5"))+
    theme_tufte() +
    theme(axis.line = element_line(), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 8), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8, hjust = 0, vjust = 0), 
          plot.title = element_text(size = 8), 
          panel.background = element_blank(), 
          legend.text = element_blank(), 
          legend.title = element_blank()) +
    coord_flip(ylim = c(74, 410)) +
    scale_x_discrete(limits = rev(levels(HTR1B_vert_data$Species)))
  
  HTR1B_motif_plot

# 1D motifs plots 
  
  # subsetting 5-HTR 1D
  
  HTR1D_vert_data <- HTR1D_vert_data %>% droplevels()
  
  # 1D length and domains
  
  # finding the top limit for the plot
  
  max(HTR1D_vert_data$Length)
  
  # finding the bottom limit 
  
  min(HTR1D_vert_data$D2.50)
  
  # levels
  
  levels(HTR1D_vert_data$Species)
  
  class(HTR1D_vert_data$Species)
  
  # plot 
  
  HTR1D_motif_plot <- 
    ggplot() +
    labs(title = "5-HTR1D") +
    scale_y_continuous(breaks = seq(60, 420, 50)) +
    geom_bar(data = HTR1D_vert_data, aes(x = Species, y = Length), stat = "identity", width = 0.2, col = "black", fill = "white") +
    geom_text(data = HTR1D_vert_data, aes(x = Species, y = Length, label = Length), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1D_vert_data, aes(x = Species, y = D2.50, colour = "D2.50"), show.legend = FALSE) +
    geom_text(data = HTR1D_vert_data, aes(x =  Species, y = D2.50, label = D2.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1D_vert_data, aes(x =  Species, y = D3.49, colour = "D3.49"), show.legend = FALSE) +
    geom_text(data = HTR1D_vert_data, aes(x =  Species, y = D3.49, label = D3.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1D_vert_data, aes(x =  Species, y = N7.49, colour = "N7.49"), show.legend = FALSE) +
    geom_text(data = HTR1D_vert_data, aes(x =  Species, y = N7.49, label = N7.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1D_vert_data, aes(x =  Species, y = I3.40, colour = "I3.40"), show.legend = FALSE) +
    geom_text(data = HTR1D_vert_data, aes(x =  Species, y = I3.40, label = I3.40), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1D_vert_data, aes(x =  Species, y = P5.50, colour = "P5.50"), show.legend = FALSE) +
    geom_text(data = HTR1D_vert_data, aes(x =  Species, y = P5.50, label = P5.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1D_vert_data, aes(x =  Species, y = F6.44, colour = "F6.44"), show.legend = FALSE) +
    geom_text(data = HTR1D_vert_data, aes(x =  Species, y = F6.44, label = F6.44), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    scale_color_manual(values = c("D3.49" = "#00B81F", "N7.49" = "#00A5FF", "D2.50" = "#FFC425", "I3.40" = "#FC61D5", "P5.50" = "#FC61D5", "F6.44" = "#FC61D5"))+
    theme_tufte() +
    theme(axis.line = element_line(), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 8), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8, hjust = 0, vjust = 0), 
          plot.title = element_text(size = 8), 
          panel.background = element_blank(), 
          legend.text = element_blank(), 
          legend.title = element_blank()) +
    coord_flip(ylim = c(60, 420)) +
    scale_x_discrete(limits = rev(levels(HTR1D_vert_data$Species))) 
  
  HTR1D_motif_plot

# 1E motif plotting 
  
  # subsetting 5-HTR 1E
  
  HTR1E_vert_data <- HTR1E_vert_data %>% droplevels()
  
  # 1E length and domains
  
  # finding the top limit for the plot
  
  max(HTR1E_vert_data$Length)
  
  # finding the bottom limit 
  
  min(HTR1E_vert_data$D2.50)
  
  # levels
  
  levels(HTR1E_vert_data$Species)
  
  class(HTR1E_vert_data$Species)
  
  # plot 
  
  HTR1E_motif_plot <- 
    ggplot() +
    labs(title = "5-HTR1E") +
    scale_y_continuous(breaks = seq(65, 380, 50)) +
    geom_bar(data = HTR1E_vert_data, aes(x = Species, y = Length), stat = "identity", width = 0.2, col = "black", fill = "white") +
    geom_text(data = HTR1E_vert_data, aes(x = Species, y = Length, label = Length), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1E_vert_data, aes(x = Species, y = D2.50, colour = "D2.50"), show.legend = FALSE) +
    geom_text(data = HTR1E_vert_data, aes(x =  Species, y = D2.50, label = D2.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1E_vert_data, aes(x =  Species, y = D3.49, colour = "D3.49"), show.legend = FALSE) +
    geom_text(data = HTR1E_vert_data, aes(x =  Species, y = D3.49, label = D3.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1E_vert_data, aes(x =  Species, y = N7.49, colour = "N7.49"), show.legend = FALSE) +
    geom_text(data = HTR1E_vert_data, aes(x =  Species, y = N7.49, label = N7.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1E_vert_data, aes(x =  Species, y = I3.40, colour = "I3.40"), show.legend = FALSE) +
    geom_text(data = HTR1E_vert_data, aes(x =  Species, y = I3.40, label = I3.40), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1E_vert_data, aes(x =  Species, y = P5.50, colour = "P5.50"), show.legend = FALSE) +
    geom_text(data = HTR1E_vert_data, aes(x =  Species, y = P5.50, label = P5.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1E_vert_data, aes(x =  Species, y = F6.44, colour = "F6.44"), show.legend = FALSE) +
    geom_text(data = HTR1E_vert_data, aes(x =  Species, y = F6.44, label = F6.44), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    scale_color_manual(values = c("D3.49" = "#00B81F", "N7.49" = "#00A5FF", "D2.50" = "#FFC425", "I3.40" = "#FC61D5", "P5.50" = "#FC61D5", "F6.44" = "#FC61D5"))+
    theme_tufte() +
    theme(axis.line = element_line(), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 8), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8, hjust = 0, vjust = 0), 
          plot.title = element_text(size = 8), 
          panel.background = element_blank(), 
          legend.text = element_blank(), 
          legend.title = element_blank()) +
    coord_flip(ylim = c(65, 380)) +
    scale_x_discrete(limits = rev(levels(HTR1E_vert_data$Species))) 
  
  HTR1E_motif_plot

# 1F motif plot 

  # finding the top limit for the plot
  
  max(HTR1F_vert_data$Length)
  
  # finding the bottom limit 
  
  min(HTR1F_vert_data$D2.50)
  
  # levels
  
  levels(HTR1F_vert_data$Species)
  
  class(HTR1F_vert_data$Species)
  
  levels(HTR1F_vert_data$Species) <- c(levels(HTR1F_vert_data$Species), "Drer a", "Drer b")
  
  # renaming double entries 
  
  HTR1F_vert_data[22,"Species"] <- as.character("Drer a")
  
  HTR1F_vert_data[23,"Species"] <- as.character("Drer b")
  
  # returning data frame to factor 
  
  HTR1F_vert_data$Species <- factor(HTR1F_vert_data$Species, levels = unique (HTR1F_vert_data$Species))
  
  class(HTR1F_vert_data$Species)
  
  # plot 
  
  HTR1F_motif_plot <- 
    ggplot() +
    labs(title = "5-HTR1F") +
    scale_y_continuous(breaks = seq(70, 420, 50)) +
    geom_bar(data = HTR1F_vert_data, aes(x = Species, y = Length), stat = "identity", width = 0.2, col = "black", fill = "white") +
    geom_text(data = HTR1F_vert_data, aes(x = Species, y = Length, label = Length), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1F_vert_data, aes(x = Species, y = D2.50, colour = "D2.50"), show.legend = FALSE) +
    geom_text(data = HTR1F_vert_data, aes(x =  Species, y = D2.50, label = D2.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1F_vert_data, aes(x =  Species, y = D3.49, colour = "D3.49"), show.legend = FALSE) +
    geom_text(data = HTR1F_vert_data, aes(x =  Species, y = D3.49, label = D3.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1F_vert_data, aes(x =  Species, y = N7.49, colour = "N7.49"), show.legend = FALSE) +
    geom_text(data = HTR1F_vert_data, aes(x =  Species, y = N7.49, label = N7.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1F_vert_data, aes(x =  Species, y = I3.40, colour = "I3.40"), show.legend = FALSE) +
    geom_text(data = HTR1F_vert_data, aes(x =  Species, y = I3.40, label = I3.40), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1F_vert_data, aes(x =  Species, y = P5.50, colour = "P5.50"), show.legend = FALSE) +
    geom_text(data = HTR1F_vert_data, aes(x =  Species, y = P5.50, label = P5.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR1F_vert_data, aes(x =  Species, y = F6.44, colour = "F6.44"), show.legend = FALSE) +
    geom_text(data = HTR1F_vert_data, aes(x =  Species, y = F6.44, label = F6.44), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    scale_color_manual(values = c("D3.49" = "#00B81F", "N7.49" = "#00A5FF", "D2.50" = "#FFC425", "I3.40" = "#FC61D5", "P5.50" = "#FC61D5", "F6.44" = "#FC61D5"))+
    theme_tufte() +
    theme(axis.line = element_line(), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 8), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8, hjust = 0, vjust = 0), 
          plot.title = element_text(size = 8), 
          panel.background = element_blank(), 
          legend.text = element_blank(), 
          legend.title = element_blank()) +
    coord_flip(ylim = c(70, 420)) +
    scale_x_discrete(limits = rev(levels(HTR1F_vert_data$Species))) 
  
  HTR1F_motif_plot

# 5A motif plotting 

  # subsetting 5-HTR 5A
  
  HTR5A_vert_data <-HTR5A_vert_data %>% droplevels()
  
  # 5A length and domains
  
  # finding the top limit for the plot
  
  max(HTR5A_vert_data$Length)
  
  # finding the bottom limit 
  
  min(HTR5A_vert_data$D2.50)
  
  # levels
  
  levels(HTR5A_vert_data$Species)
  
  class(HTR5A_vert_data$Species)
  
  # plot
  
  HTR5A_motif_plot <- 
    ggplot() +
    labs(title = "5-HTR5A") +
    scale_y_continuous(breaks = seq(76, 370, 50)) +
    geom_bar(data = HTR5A_vert_data, aes(x = Species, y = Length), stat = "identity", width = 0.2, col = "black", fill = "white") +
    geom_text(data = HTR5A_vert_data, aes(x = Species, y = Length, label = Length), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR5A_vert_data, aes(x = Species, y = D2.50, colour = "D2.50"), show.legend = FALSE) +
    geom_text(data = HTR5A_vert_data, aes(x =  Species, y = D2.50, label = D2.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR5A_vert_data, aes(x =  Species, y = D3.49, colour = "D3.49"), show.legend = FALSE) +
    geom_text(data = HTR5A_vert_data, aes(x =  Species, y = D3.49, label = D3.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR5A_vert_data, aes(x =  Species, y = N7.49, colour = "N7.49"), show.legend = FALSE) +
    geom_text(data = HTR5A_vert_data, aes(x =  Species, y = N7.49, label = N7.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR5A_vert_data, aes(x =  Species, y = I3.40, colour = "I3.40"), show.legend = FALSE) +
    geom_text(data = HTR5A_vert_data, aes(x =  Species, y = I3.40, label = I3.40), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR5A_vert_data, aes(x =  Species, y = P5.50, colour = "P5.50"), show.legend = FALSE) +
    geom_text(data = HTR5A_vert_data, aes(x =  Species, y = P5.50, label = P5.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR5A_vert_data, aes(x =  Species, y = F6.44, colour = "F6.44"), show.legend = FALSE) +
    geom_text(data = HTR5A_vert_data, aes(x =  Species, y = F6.44, label = F6.44), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    scale_color_manual(values = c("D3.49" = "#00B81F", "N7.49" = "#00A5FF", "D2.50" = "#FFC425", "I3.40" = "#FC61D5", "P5.50" = "#FC61D5", "F6.44" = "#FC61D5"))+
    theme_tufte() +
    theme(axis.line = element_line(), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 8), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8, hjust = 0, vjust = 0), 
          plot.title = element_text(size = 8), 
          panel.background = element_blank(), 
          legend.text = element_blank(), 
          legend.title = element_blank()) +
    coord_flip(ylim = c(76, 370)) +
    scale_x_discrete(limits = rev(levels(HTR5A_vert_data$Species))) 
  
  HTR5A_motif_plot

# 5B motif plotting 

  # subsetting 5-HTR 5B
  
  HTR5B_vert_data <- HTR5B_vert_data %>% droplevels()
  
  # 5B length and domains
  
  # finding the top limit for the plot
  
  max(HTR5B_vert_data$Length)
  
  # finding the bottom limit 
  
  min(HTR5B_vert_data$D2.50)
  
  # levels
  
  levels(HTR5B_vert_data$Species)
  
  class(HTR5B_vert_data$Species)
  
  # plot
  
  HTR5B_motif_plot <- 
    ggplot() +
    labs(title = "5-HTR5B") +
    scale_y_continuous(breaks = seq(98, 380, 50)) +
    geom_bar(data = HTR5B_vert_data, aes(x = Species, y = Length), stat = "identity", width = 0.2, col = "black", fill = "white") +
    geom_text(data = HTR5B_vert_data, aes(x = Species, y = Length, label = Length), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR5B_vert_data, aes(x = Species, y = D2.50, colour = "D2.50"), show.legend = FALSE) +
    geom_text(data = HTR5B_vert_data, aes(x =  Species, y = D2.50, label = D2.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR5B_vert_data, aes(x =  Species, y = D3.49, colour = "D3.49"), show.legend = FALSE) +
    geom_text(data = HTR5B_vert_data, aes(x =  Species, y = D3.49, label = D3.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR5B_vert_data, aes(x =  Species, y = N7.49, colour = "N7.49"), show.legend = FALSE) +
    geom_text(data = HTR5B_vert_data, aes(x =  Species, y = N7.49, label = N7.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR5B_vert_data, aes(x =  Species, y = I3.40, colour = "I3.40"), show.legend = FALSE) +
    geom_text(data = HTR5B_vert_data, aes(x =  Species, y = I3.40, label = I3.40), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR5B_vert_data, aes(x =  Species, y = P5.50, colour = "P5.50"), show.legend = FALSE) +
    geom_text(data = HTR5B_vert_data, aes(x =  Species, y = P5.50, label = P5.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR5B_vert_data, aes(x =  Species, y = F6.44, colour = "F6.44"), show.legend = FALSE) +
    geom_text(data = HTR5B_vert_data, aes(x =  Species, y = F6.44, label = F6.44), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    scale_color_manual(values = c("D3.49" = "#00B81F", "N7.49" = "#00A5FF", "D2.50" = "#FFC425", "I3.40" = "#FC61D5", "P5.50" = "#FC61D5", "F6.44" = "#FC61D5"))+
    theme_tufte() +
    theme(axis.line = element_line(), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 8), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8, hjust = 0, vjust = 0), 
          plot.title = element_text(size = 8), 
          panel.background = element_blank(), 
          legend.text = element_blank(), 
          legend.title = element_blank()) +
    coord_flip(ylim = c(98, 380)) +
    scale_x_discrete(limits = rev(levels(HTR5B_vert_data$Species))) 
  
  HTR5B_motif_plot

# 6 motif plotting 

  # subsetting 5-HTR 6

  HTR6_vert_data <- HTR6_vert_data  %>% droplevels()

  # finding the top limit for the plot

  max(HTR6_vert_data$Length)

  # finding the bottom limit 

  min(HTR6_vert_data$D2.50, na.rm=T)

  # levels

  levels(HTR6_vert_data$Species)

  class(HTR6_vert_data$Species)

  levels(HTR6_vert_data$Species) <- c(levels(HTR6_vert_data$Species), "Xmac X1", "Xmac X2")

  # renaming double entries 

  HTR6_vert_data[19,"Species"] <- as.character("Xmac X1")

  HTR6_vert_data[20,"Species"] <- as.character("Xmac X2")

  # returning data frame to factor 

  HTR6_vert_data$Species <- factor(HTR6_vert_data$Species, levels = unique (HTR6_vert_data$Species))

  class(HTR6_vert_data$Species)

  # plot

    HTR6_motif_plot <- 
    ggplot() +
    labs(title = "5-HTR6") +
    scale_y_continuous(breaks = seq(42, 490, 50)) +
    geom_bar(data = HTR6_vert_data, aes(x = Species, y = Length), stat = "identity", width = 0.2, col = "black", fill = "white") +
    geom_text(data = HTR6_vert_data, aes(x = Species, y = Length, label = Length), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR6_vert_data, aes(x = Species, y = D2.50, colour = "D2.50"), show.legend = FALSE) +
    geom_text(data = HTR6_vert_data, aes(x =  Species, y = D2.50, label = D2.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR6_vert_data, aes(x =  Species, y = D3.49, colour = "D3.49"), show.legend = FALSE) +
    geom_text(data = HTR6_vert_data, aes(x =  Species, y = D3.49, label = D3.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR6_vert_data, aes(x =  Species, y = N7.49, colour = "N7.49"), show.legend = FALSE) +
    geom_text(data = HTR6_vert_data, aes(x =  Species, y = N7.49, label = N7.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR6_vert_data, aes(x =  Species, y = I3.40, colour = "I3.40"), show.legend = FALSE) +
    geom_text(data = HTR6_vert_data, aes(x =  Species, y = I3.40, label = I3.40), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR6_vert_data, aes(x =  Species, y = P5.50, colour = "P5.50"), show.legend = FALSE) +
    geom_text(data = HTR6_vert_data, aes(x =  Species, y = P5.50, label = P5.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR6_vert_data, aes(x =  Species, y = F6.44, colour = "F6.44"), show.legend = FALSE) +
    geom_text(data = HTR6_vert_data, aes(x =  Species, y = F6.44, label = F6.44), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    scale_color_manual(values = c("D3.49" = "#00B81F", "N7.49" = "#00A5FF", "D2.50" = "#FFC425", "I3.40" = "#FC61D5", "P5.50" = "#FC61D5", "F6.44" = "#FC61D5"))+
    theme_tufte() +
    theme(axis.line = element_line(), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 8), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8, hjust = 0, vjust = 0), 
          plot.title = element_text(size = 8), 
          panel.background = element_blank(), 
          legend.text = element_blank(), 
          legend.title = element_blank()) +
    coord_flip(ylim = c(42, 490)) +
    scale_x_discrete(limits = rev(levels(HTR6_vert_data$Species))) 
  
  HTR6_motif_plot

  # 2C motif plotting 
  
  # 2C length and domains
  
  # finding the top limit for the plot
  
    max(HTR2C_vert_data$Length)
  
  # finding the bottom limit 
  
    min(HTR2C_vert_data$D2.50, na.rm=T)
  
  # levels
  
    levels(HTR2C_vert_data$Species)
  
    class(HTR2C_vert_data$Species)
  
    levels(HTR2C_vert_data$Species) <- c(levels(HTR2C_vert_data$Species), "Nmel X1", "Nmel X2", "Ggal X1", "Ggal X2")
  
  # renaming double entries 
  
    HTR2C_vert_data[17,"Species"] <- as.character("Nmel X1")
  
    HTR2C_vert_data[18,"Species"] <- as.character("Nmel X2")
  
    HTR2C_vert_data[19,"Species"] <- as.character("Ggal X1")
  
    HTR2C_vert_data[19,"Species"] <- as.character("Ggal X2")
  
  # returning data frame to factor 
  
    HTR2C_vert_data$Species <- factor(HTR2C_vert_data$Species, levels = unique (HTR2C_vert_data$Species))
  
  
    class(HTR2C_vert_data$Species)
  
  # new 2C without the X2 data points 
  
    HTR2C_new_data <- HTR2C_vert_data[-c(18,20),] %>% droplevels()
  str(HTR2C_new_data)
  
  # plot
  
  HTR2C_motif_plot <- 
    ggplot() +
    labs(title = "5-HTR2C") +
    scale_y_continuous(breaks = seq(99, 520, 50)) +
    geom_bar(data = HTR2C_new_data, aes(x = Species, y = Length), stat = "identity", width = 0.2, col = "black", fill = "white") +
    geom_text(data = HTR2C_new_data, aes(x = Species, y = Length, label = Length), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2C_new_data, aes(x = Species, y = D2.50, colour = "D2.50"), show.legend = FALSE) +
    geom_text(data = HTR2C_new_data, aes(x =  Species, y = D2.50, label = D2.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2C_new_data, aes(x =  Species, y = D3.49, colour = "D3.49"), show.legend = FALSE) +
    geom_text(data = HTR2C_new_data, aes(x =  Species, y = D3.49, label = D3.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2C_new_data, aes(x =  Species, y = N7.49, colour = "N7.49"), show.legend = FALSE) +
    geom_text(data = HTR2C_new_data, aes(x =  Species, y = N7.49, label = N7.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2C_new_data, aes(x =  Species, y = I3.40, colour = "I3.40"), show.legend = FALSE) +
    geom_text(data = HTR2C_new_data, aes(x =  Species, y = I3.40, label = I3.40), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2C_new_data, aes(x =  Species, y = P5.50, colour = "P5.50"), show.legend = FALSE) +
    geom_text(data = HTR2C_new_data, aes(x =  Species, y = P5.50, label = P5.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2C_new_data, aes(x =  Species, y = F6.44, colour = "F6.44"), show.legend = FALSE) +
    geom_text(data = HTR2C_new_data, aes(x =  Species, y = F6.44, label = F6.44), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    scale_color_manual(values = c("D3.49" = "#00B81F", "N7.49" = "#00A5FF", "D2.50" = "#FFC425", "I3.40" = "#FC61D5", "P5.50" = "#FC61D5", "F6.44" = "#FC61D5"))+
    theme_tufte() +
    theme(axis.line = element_line(), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 8), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8, hjust = 0, vjust = 0), 
          plot.title = element_text(size = 8), 
          panel.background = element_blank(), 
          legend.text = element_blank(), 
          legend.title = element_blank()) +
    coord_flip(ylim = c(99, 520)) +
    scale_x_discrete(limits = rev(levels(HTR2C_new_data$Species))) 
  
  HTR2C_motif_plot

# plotting 2b motifs 

  # finding the top limit for the plot
  
    max(HTR2B_vert_data$Length)
  
  # finding the bottom limit 
  
    min(HTR2B_vert_data$D2.50, na.rm=T)
  
  # levels
  
    levels(HTR2B_vert_data$Species)
  
  
    levels(HTR2B_vert_data$Name)
  
    class(HTR2B_vert_data$Species)
  
  
    levels(HTR2B_vert_data$Species) <- c(levels(HTR2B_vert_data$Species), 
                                       "Hsap 1",
                                       "Hsap 2",
                                       "Ptro X1",
                                       "Ptro X3",
                                       "Ggor X1",
                                       "Ggor X2",
                                       "Mmul X1",
                                       "Mmul X2",
                                       "Mfas X1",
                                       "Mfas X2",
                                       "Cjac X1",
                                       "Cjac X2",
                                       "Mmur X1",
                                       "Mmur X2",
                                       "Sscr X1",
                                       "Sscr X2",
                                       "Oari X1",
                                       "Oari X2",
                                       "Ocun X1",
                                       "Ocun X2")
  
  
    # renaming double entries 
      HTR2B_vert_data[1,"Species"] <- as.character("Hsap 1")
      HTR2B_vert_data[2,"Species"] <- as.character("Hsap 2")
      HTR2B_vert_data[3,"Species"] <- as.character("Ptro X1")
      HTR2B_vert_data[4,"Species"] <- as.character("Ptro X3")
      HTR2B_vert_data[5,"Species"] <- as.character("Ggor X1")
      HTR2B_vert_data[6,"Species"] <- as.character("Ggor X2")
      HTR2B_vert_data[7,"Species"] <- as.character("Mmul X1")
      HTR2B_vert_data[8,"Species"] <- as.character("Mmul X2")
      HTR2B_vert_data[9,"Species"] <- as.character("Mfas X1")
      HTR2B_vert_data[10,"Species"] <- as.character("Cjac X1")
      HTR2B_vert_data[11,"Species"] <- as.character("Cjac X2")
      HTR2B_vert_data[12,"Species"] <- as.character("Mmur X1")
      HTR2B_vert_data[13,"Species"] <- as.character("Mmur X2")
      HTR2B_vert_data[19,"Species"] <- as.character("Oari X1")
      HTR2B_vert_data[20,"Species"] <- as.character("Oari X2")
      HTR2B_vert_data[21,"Species"] <- as.character("Sscr X1")
      HTR2B_vert_data[22,"Species"] <- as.character("Sscr X2")
      HTR2B_vert_data[23,"Species"] <- as.character("Ocun X1")
      HTR2B_vert_data[24,"Species"] <- as.character("Ocun X2")
  
  
      # returning data frame to factor 
  
      HTR2B_vert_data$Species <- factor(HTR2B_vert_data$Species, levels = unique (HTR2B_vert_data$Species))
  
      class(HTR2B_vert_data$Species)
  
  
      # plot
  
      HTR2B_motif_plot <- 
      ggplot() +
      labs(title = "5-HTR2B") +
      scale_y_continuous(breaks = seq(48, 540, 50)) +
      geom_bar(data = HTR2B_vert_data, aes(x = Species, y = Length), stat = "identity", width = 0.2, col = "black", fill = "white") +
      geom_text(data = HTR2B_vert_data, aes(x = Species, y = Length, label = Length), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
      geom_point(data = HTR2B_vert_data, aes(x = Species, y = D2.50, colour = "D2.50"), show.legend = FALSE) +
      geom_text(data = HTR2B_vert_data, aes(x =  Species, y = D2.50, label = D2.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
      geom_point(data = HTR2B_vert_data, aes(x =  Species, y = D3.49, colour = "D3.49"), show.legend = FALSE) +
      geom_text(data = HTR2B_vert_data, aes(x =  Species, y = D3.49, label = D3.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
      geom_point(data = HTR2B_vert_data, aes(x =  Species, y = N7.49, colour = "N7.49"), show.legend = FALSE) +
      geom_text(data = HTR2B_vert_data, aes(x =  Species, y = N7.49, label = N7.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
      geom_point(data = HTR2B_vert_data, aes(x =  Species, y = I3.40, colour = "I3.40"), show.legend = FALSE) +
      geom_text(data = HTR2B_vert_data, aes(x =  Species, y = I3.40, label = I3.40), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
      geom_point(data = HTR2B_vert_data, aes(x =  Species, y = P5.50, colour = "P5.50"), show.legend = FALSE) +
      geom_text(data = HTR2B_vert_data, aes(x =  Species, y = P5.50, label = P5.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
      geom_point(data = HTR2B_vert_data, aes(x =  Species, y = F6.44, colour = "F6.44"), show.legend = FALSE) +
      geom_text(data = HTR2B_vert_data, aes(x =  Species, y = F6.44, label = F6.44), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
      scale_color_manual(values = c("D3.49" = "#00B81F", "N7.49" = "#00A5FF", "D2.50" = "#FFC425", "I3.40" = "#FC61D5", "P5.50" = "#FC61D5", "F6.44" = "#FC61D5"))+
      theme_tufte() +
      theme(axis.line = element_line(), 
            axis.title.x = element_blank(), 
            axis.text.x = element_text(size = 8), 
            axis.title.y = element_blank(),
            axis.text.y = element_text(size = 8, hjust = 0, vjust = 0), 
            plot.title = element_text(size = 8), 
            panel.background = element_blank(), 
            legend.text = element_blank(), 
            legend.title = element_blank()) +
      coord_flip(ylim = c(48, 540)) +
      scale_x_discrete(limits = rev(levels(HTR2B_vert_data$Species))) 
    
    HTR2B_motif_plot
  
  # new 2B without weird points
  
    HTR2B_new_data <- HTR2B_new_data %>% droplevels()
  
    str(HTR2B_new_data)
  
    HTR2B_new_motif_plot <- 
    ggplot() +
    labs(title = "5-HTR2B") +
    scale_y_continuous(breaks = seq(48, 540, 50)) +
    geom_bar(data = HTR2B_new_data, aes(x = Species, y = Length), stat = "identity", width = 0.2, col = "black", fill = "white") +
    geom_text(data = HTR2B_new_data, aes(x = Species, y = Length, label = Length), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2B_new_data, aes(x = Species, y = D2.50, colour = "D2.50"), show.legend = FALSE) +
    geom_text(data = HTR2B_new_data, aes(x =  Species, y = D2.50, label = D2.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2B_new_data, aes(x =  Species, y = D3.49, colour = "D3.49"), show.legend = FALSE) +
    geom_text(data = HTR2B_new_data, aes(x =  Species, y = D3.49, label = D3.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2B_new_data, aes(x =  Species, y = N7.49, colour = "N7.49"), show.legend = FALSE) +
    geom_text(data = HTR2B_new_data, aes(x =  Species, y = N7.49, label = N7.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2B_new_data, aes(x =  Species, y = I3.40, colour = "I3.40"), show.legend = FALSE) +
    geom_text(data = HTR2B_new_data, aes(x =  Species, y = I3.40, label = I3.40), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2B_new_data, aes(x =  Species, y = P5.50, colour = "P5.50"), show.legend = FALSE) +
    geom_text(data = HTR2B_new_data, aes(x =  Species, y = P5.50, label = P5.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2B_new_data, aes(x =  Species, y = F6.44, colour = "F6.44"), show.legend = FALSE) +
    geom_text(data = HTR2B_new_data, aes(x =  Species, y = F6.44, label = F6.44), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    scale_color_manual(values = c("D3.49" = "#00B81F", "N7.49" = "#00A5FF", "D2.50" = "#FFC425", "I3.40" = "#FC61D5", "P5.50" = "#FC61D5", "F6.44" = "#FC61D5"))+
    theme_tufte() +
    theme(axis.line = element_line(), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 8), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8, hjust = 0, vjust = 0), 
          plot.title = element_text(size = 8), 
          panel.background = element_blank(), 
          legend.text = element_blank(), 
          legend.title = element_blank()) +
    coord_flip(ylim = c(48, 540)) +
    scale_x_discrete(limits = rev(levels(HTR2B_new_data$Species))) 
  
  HTR2B_new_motif_plot
  

# 2A motif plot 

# finding the top limit for the plot

  max(HTR2A_vert_data$Length)

# finding the bottom limit 

  min(HTR2A_vert_data$D2.50, na.rm=T)

  min(HTR2A_vert_data$D3.49, na.rm=T)

# levels

  levels(HTR2A_vert_data$Species)

  class(HTR2A_vert_data$Species)

  levels(HTR2A_vert_data$Species) <- c(levels(HTR2A_vert_data$Species), 
                                     "Hsap 1",
                                     "Hsap 2",
                                     "Ptro X1",
                                     "Ptro X2",
                                     "Ggor",
                                     "Mmul X1",
                                     "Mmul X2",
                                     "Mfas X1",
                                     "Mfas X2",
                                     "Cjac",
                                     "Mmur X1",
                                     "Mmur X2")

# renaming double entries 

  HTR2A_vert_data[1,"Species"] <- as.character("Hsap 1")

  HTR2A_vert_data[2,"Species"] <- as.character("Hsap 2")

  HTR2A_vert_data[3,"Species"] <- as.character("Ptro X1")

  HTR2A_vert_data[4,"Species"] <- as.character("Ptro X2")

  HTR2A_vert_data[6,"Species"] <- as.character("Mmul X1")

  HTR2A_vert_data[7,"Species"] <- as.character("Mmul X2")

  HTR2A_vert_data[8,"Species"] <- as.character("Mfas X1")

  HTR2A_vert_data[9,"Species"] <- as.character("Mfas X2")

  HTR2A_vert_data[11,"Species"] <- as.character("Mmur X1")

  HTR2A_vert_data[12,"Species"] <- as.character("Mmur X2")

# returning data frame to factor 

  HTR2A_vert_data$Species <- factor(HTR2A_vert_data$Species, levels = unique (HTR2A_vert_data$Species))

  class(HTR2A_vert_data$Species)

# plot 

  HTR2A_motif_plot <- 
  ggplot() +
  labs(title = "5-HTR2A") +
  scale_y_continuous(breaks = seq(66, 500, 50)) +
  geom_bar(data = HTR2A_vert_data, aes(x = Species, y = Length), stat = "identity", width = 0.2, col = "black", fill = "white") +
  geom_text(data = HTR2A_vert_data, aes(x = Species, y = Length, label = Length), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
  geom_point(data = HTR2A_vert_data, aes(x = Species, y = D2.50, colour = "D2.50"), show.legend = FALSE) +
  geom_text(data = HTR2A_vert_data, aes(x =  Species, y = D2.50, label = D2.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
  geom_point(data = HTR2A_vert_data, aes(x =  Species, y = D3.49, colour = "D3.49"), show.legend = FALSE) +
  geom_text(data = HTR2A_vert_data, aes(x =  Species, y = D3.49, label = D3.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
  geom_point(data = HTR2A_vert_data, aes(x =  Species, y = N7.49, colour = "N7.49"), show.legend = FALSE) +
  geom_text(data = HTR2A_vert_data, aes(x =  Species, y = N7.49, label = N7.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
  geom_point(data = HTR2A_vert_data, aes(x =  Species, y = I3.40, colour = "I3.40"), show.legend = FALSE) +
  geom_text(data = HTR2A_vert_data, aes(x =  Species, y = I3.40, label = I3.40), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
  geom_point(data = HTR2A_vert_data, aes(x =  Species, y = P5.50, colour = "P5.50"), show.legend = FALSE) +
  geom_text(data = HTR2A_vert_data, aes(x =  Species, y = P5.50, label = P5.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
  geom_point(data = HTR2A_vert_data, aes(x =  Species, y = F6.44, colour = "F6.44"), show.legend = FALSE) +
  geom_text(data = HTR2A_vert_data, aes(x =  Species, y = F6.44, label = F6.44), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
  scale_color_manual(values = c("D3.49" = "#00B81F", "N7.49" = "#00A5FF", "D2.50" = "#FFC425", "I3.40" = "#FC61D5", "P5.50" = "#FC61D5", "F6.44" = "#FC61D5"))+
  theme_tufte() +
  theme(axis.line = element_line(), 
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 8), 
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8, hjust = 0, vjust = 0), 
        plot.title = element_text(size = 8), 
        panel.background = element_blank(), 
        legend.text = element_blank(), 
        legend.title = element_blank()) +
  coord_flip(ylim = c(66, 500)) +
  scale_x_discrete(limits = rev(levels(HTR2A_vert_data$Species))) 

HTR2A_motif_plot

# new 2A without the 2 isoform 

  HTR2A_new_data <- HTR2A_new_data %>% droplevels()
  
  str(HTR2A_new_data)
  
  HTR2A_new_motif_plot <-
    ggplot() +
    labs(title = "5-HTR2A") +
    scale_y_continuous(breaks = seq(48, 540, 50)) +
    geom_bar(data = HTR2A_new_data, aes(x = Species, y = Length), stat = "identity", width = 0.2, col = "black", fill = "white") +
    geom_text(data = HTR2A_new_data, aes(x = Species, y = Length, label = Length), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2A_new_data, aes(x = Species, y = D2.50, colour = "D2.50"), show.legend = FALSE) +
    geom_text(data = HTR2A_new_data, aes(x =  Species, y = D2.50, label = D2.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2A_new_data, aes(x =  Species, y = D3.49, colour = "D3.49"), show.legend = FALSE) +
    geom_text(data = HTR2A_new_data, aes(x =  Species, y = D3.49, label = D3.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2A_new_data, aes(x =  Species, y = N7.49, colour = "N7.49"), show.legend = FALSE) +
    geom_text(data = HTR2A_new_data, aes(x =  Species, y = N7.49, label = N7.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2A_new_data, aes(x =  Species, y = I3.40, colour = "I3.40"), show.legend = FALSE) +
    geom_text(data = HTR2A_new_data, aes(x =  Species, y = I3.40, label = I3.40), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2A_new_data, aes(x =  Species, y = P5.50, colour = "P5.50"), show.legend = FALSE) +
    geom_text(data = HTR2A_new_data, aes(x =  Species, y = P5.50, label = P5.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR2A_new_data, aes(x =  Species, y = F6.44, colour = "F6.44"), show.legend = FALSE) +
    geom_text(data = HTR2A_new_data, aes(x =  Species, y = F6.44, label = F6.44), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    scale_color_manual(values = c("D3.49" = "#00B81F", "N7.49" = "#00A5FF", "D2.50" = "#FFC425", "I3.40" = "#FC61D5", "P5.50" = "#FC61D5", "F6.44" = "#FC61D5"))+
    theme_tufte() +
    theme(axis.line = element_line(), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 8), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8, hjust = 0, vjust = 0), 
          plot.title = element_text(size = 8), 
          panel.background = element_blank(), 
          legend.text = element_blank(), 
          legend.title = element_blank()) +
    coord_flip(ylim = c(48, 540)) +
    scale_x_discrete(limits = rev(levels(HTR2A_new_data$Species))) 
  
  HTR2A_new_motif_plot

# 7 motif plot 
  
  # finding the top limit for the plot
  
    max(HTR7_vert_data$Length)
  
  # finding the bottom limit 
  
    min(HTR7_vert_data$D2.50, na.rm=T)
  
    min(HTR7_vert_data$D3.49, na.rm=T)
  
  # levels
  
    levels(HTR7_vert_data$Species)
  
    class(HTR7_vert_data$Species)
  
    levels(HTR7_vert_data$Name)
  
    levels(HTR7_vert_data$Species) <- c(levels(HTR7_vert_data$Species), 
                                      "Hsap a", 
                                      "Hsap b", 
                                      "Hsap d",   
                                      "Ptro X1",
                                      "Ptro X2",  
                                      "Ggor X1",
                                      "Ggor X2",
                                      "Mmul X1",
                                      "Mmul X2", 
                                      "Mmul X3", 
                                      "Mfas X1",      
                                      "Mfas X2", 
                                      "Mfas X3", 
                                      "Cjac X1",      
                                      "Cjac X2",      
                                      "Cjac X3",      
                                      "Cjac X4",     
                                      "Mmur X1",
                                      "Mmur X2",
                                      "Mmur X3",  
                                      "Mmus 1", 
                                      "Mmus 2", 
                                      "Mmus 3",   
                                      "Rnor X1",  
                                      "Rnor X2", 
                                      "Btau X1",  
                                      "Btau X2", 
                                      "Sscr X1",
                                      "Sscr X2",
                                      "Ocun X1",  
                                      "Ocun X2",
                                      "Cfam X1",
                                      "Cfam X2", 
                                      "Fcat X1",
                                      "Fcat X2",
                                      "Fcat X3", 
                                      "Ggal X1",
                                      "Ggal X2",     
                                      "Ggal X3" )
  
  # renaming double entries 
  
    HTR7_vert_data[1,"Species"] <- as.character("Hsap a")
  
    HTR7_vert_data[2,"Species"] <- as.character("Hsap b")
  
    HTR7_vert_data[3,"Species"] <- as.character("Hsap d")
  
    HTR7_vert_data[5,"Species"] <- as.character("Ptro X1")
  
    HTR7_vert_data[6,"Species"] <- as.character("Ptro X2")  
  
    HTR7_vert_data[7,"Species"] <- as.character("Ggor X1")
  
    HTR7_vert_data[8,"Species"] <- as.character("Ggor X2")
  
    HTR7_vert_data[9,"Species"] <- as.character("Mmul X1")
  
    HTR7_vert_data[10,"Species"] <- as.character("Mmul X2") 
  
    HTR7_vert_data[11,"Species"] <- as.character("Mmul X3") 
  
    HTR7_vert_data[12,"Species"] <- as.character("Mfas X1")      
  
    HTR7_vert_data[13,"Species"] <- as.character("Mfas X2") 
  
    HTR7_vert_data[14,"Species"] <- as.character("Mfas X3") 
  
    HTR7_vert_data[15,"Species"] <- as.character("Cjac X1")      
  
    HTR7_vert_data[16,"Species"] <- as.character("Cjac X2")      
  
    HTR7_vert_data[17,"Species"] <- as.character("Cjac X3")      
  
    HTR7_vert_data[18,"Species"] <- as.character("Cjac X4")     
  
    HTR7_vert_data[19,"Species"] <- as.character("Mmur X1")
  
    HTR7_vert_data[20,"Species"] <- as.character("Mmur X2")
  
    HTR7_vert_data[21,"Species"] <- as.character("Mmur X3")  
  
    HTR7_vert_data[22,"Species"] <- as.character("Mmus 1") 
  
    HTR7_vert_data[23,"Species"] <- as.character("Mmus 2") 
  
    HTR7_vert_data[24,"Species"] <- as.character("Mmus 3")      
  
    HTR7_vert_data[26,"Species"] <- as.character("Rnor X1")  
  
    HTR7_vert_data[27,"Species"] <- as.character("Rnor X2") 
  
    HTR7_vert_data[29,"Species"] <- as.character("Btau X1")  
  
    HTR7_vert_data[30,"Species"] <- as.character("Btau X2") 
  
    HTR7_vert_data[34,"Species"] <- as.character("Sscr X1")
  
    HTR7_vert_data[35,"Species"] <- as.character("Sscr X2")
  
    HTR7_vert_data[36,"Species"] <- as.character("Ocun X1")  
  
    HTR7_vert_data[37,"Species"] <- as.character("Ocun X2")
  
    HTR7_vert_data[38,"Species"] <- as.character("Cfam X1")
  
    HTR7_vert_data[39,"Species"] <- as.character("Cfam X2") 
  
    HTR7_vert_data[40,"Species"] <- as.character("Fcat X1")
  
    HTR7_vert_data[41,"Species"] <- as.character("Fcat X2")
  
    HTR7_vert_data[42,"Species"] <- as.character("Fcat X3") 
  
    HTR7_vert_data[44,"Species"] <- as.character("Ggal X1")
  
    HTR7_vert_data[45,"Species"] <- as.character("Ggal X2")     
    
    HTR7_vert_data[46,"Species"] <- as.character("Ggal X3")
  
  # returning data frame to factor 
  
   HTR7_vert_data$Species <- factor(HTR7_vert_data$Species, levels = unique (HTR7_vert_data$Species))
  
  
   class(HTR7_vert_data$Species)
  
  
  # plot 
  
   HTR7_motif_plot <- 
    ggplot() +
    labs(title = "5-HTR7") +
    scale_y_continuous(breaks = seq(107, 510, 50)) +
    geom_bar(data = HTR7_vert_data, aes(x = Species, y = Length), stat = "identity", width = 0.2, col = "black", fill = "white") +
    geom_text(data = HTR7_vert_data, aes(x = Species, y = Length, label = Length), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR7_vert_data, aes(x = Species, y = D2.50, colour = "D2.50"), show.legend = FALSE) +
    geom_text(data = HTR7_vert_data, aes(x =  Species, y = D2.50, label = D2.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR7_vert_data, aes(x =  Species, y = D3.49, colour = "D3.49"), show.legend = FALSE) +
    geom_text(data = HTR7_vert_data, aes(x =  Species, y = D3.49, label = D3.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR7_vert_data, aes(x =  Species, y = N7.49, colour = "N7.49"), show.legend = FALSE) +
    geom_text(data = HTR7_vert_data, aes(x =  Species, y = N7.49, label = N7.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR7_vert_data, aes(x =  Species, y = I3.40, colour = "I3.40"), show.legend = FALSE) +
    geom_text(data = HTR7_vert_data, aes(x =  Species, y = I3.40, label = I3.40), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR7_vert_data, aes(x =  Species, y = P5.50, colour = "P5.50"), show.legend = FALSE) +
    geom_text(data = HTR7_vert_data, aes(x =  Species, y = P5.50, label = P5.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR7_vert_data, aes(x =  Species, y = F6.44, colour = "F6.44"), show.legend = FALSE) +
    geom_text(data = HTR7_vert_data, aes(x =  Species, y = F6.44, label = F6.44), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    scale_color_manual(values = c("D3.49" = "#00B81F", "N7.49" = "#00A5FF", "D2.50" = "#FFC425", "I3.40" = "#FC61D5", "P5.50" = "#FC61D5", "F6.44" = "#FC61D5"))+
    theme_tufte() +
    theme(axis.line = element_line(), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 8), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8, hjust = 0, vjust = 0), 
          plot.title = element_text(size = 8), 
          panel.background = element_blank(), 
          legend.text = element_blank(), 
          legend.title = element_blank()) +
    coord_flip(ylim = c(107, 510)) +
    scale_x_discrete(limits = rev(levels(HTR7_vert_data$Species))) 
  
  HTR7_motif_plot

# 4 motif plot 

  # finding the top limit for the plot
  
   max(HTR4_vert_data$Length)
  
  # finding the bottom limit 
  
   min(HTR4_vert_data$D2.50, na.rm=T)
  
  # levels
  
   levels(HTR4_vert_data$Species)
  
   class(HTR4_vert_data$Species)
  
   levels(HTR4_vert_data$Species) <- c(levels(HTR4_vert_data$Species), 
                                      "Hsap a",
                                      "Hsap b",
                                      "Hsap c",
                                      "Hsap d",
                                      "Hsap g",
                                      "Hsap i",
                                      "Ptro X1",
                                      "Ptro X2",
                                      "Ptro X3",
                                      "Ptro X4",
                                      "Ggor X1",
                                      "Ggor X2",
                                      "Mmul X1",
                                      "Mmul X2",
                                      "Mmul X3",
                                      "Mfas X1",
                                      "Mfas X2",
                                      "Mfas X3",
                                      "Mfas X4",
                                      "Cjac X1",
                                      "Cjac X2",
                                      "Mmur X1",
                                      "Mmur X2",
                                      "Mmur X3",
                                      "Mmur X4",
                                      "Mmus 1",
                                      "Mmus 2",
                                      "Mmus 3",
                                      "Mmus 4",
                                      "Mmus 5",
                                      "Rnor X1",
                                      "Rnor X2",
                                      "Pcat X1",
                                      "Pcat X2",
                                      "Pcat X3",
                                      "Btau X1",
                                      "Btau X2",
                                      "Btau X3",
                                      "Btau X4",
                                      "Chir X1",
                                      "Chir X2",
                                      "Ocun X1",
                                      "Ocun X2",
                                      "Cfam X1",
                                      "Cfam X2",
                                      "Cfam X3",
                                      "Fcat X1",
                                      "Fcat X2",
                                      "Fcat X3",
                                      "Nmel X1",
                                      "Nmel X2",
                                      "Nmel X3",
                                      "Ggal X1",
                                      "Ggal X2",
                                      "Xmac X1",
                                      "Xmac X2",
                                      "Xmac X3",
                                      "Xmac X4",
                                      "Drer X1",
                                      "Drer X2")
  
  # renaming double entries 
  
   HTR4_vert_data[1,"Species"] <- as.character("Hsap a")
  
   HTR4_vert_data[2,"Species"] <- as.character("Hsap b")
  
   HTR4_vert_data[3,"Species"] <- as.character("Hsap c")
  
   HTR4_vert_data[4,"Species"] <- as.character("Hsap d")
  
   HTR4_vert_data[5,"Species"] <- as.character("Hsap g")
  
   HTR4_vert_data[6,"Species"] <- as.character("Hsap i")
  
   HTR4_vert_data[7,"Species"] <- as.character("Ptro X1")
  
   HTR4_vert_data[8,"Species"] <- as.character("Ptro X2")
  
   HTR4_vert_data[9,"Species"] <- as.character("Ptro X3")
  
   HTR4_vert_data[10,"Species"] <- as.character("Ptro X4")
  
   HTR4_vert_data[11,"Species"] <- as.character("Ggor X1")
  
   HTR4_vert_data[12,"Species"] <- as.character("Ggor X2")
  
   HTR4_vert_data[13,"Species"] <- as.character("Mmul X1")
  
   HTR4_vert_data[14,"Species"] <- as.character("Mmul X2")
  
   HTR4_vert_data[15,"Species"] <- as.character("Mmul X3")
  
   HTR4_vert_data[16,"Species"] <- as.character("Mfas X1")
  
   HTR4_vert_data[17,"Species"] <- as.character("Mfas X2")
  
   HTR4_vert_data[18,"Species"] <- as.character("Mfas X3")
  
   HTR4_vert_data[19,"Species"] <- as.character("Mfas X4")
  
   HTR4_vert_data[20,"Species"] <- as.character("Cjac X1")
  
   HTR4_vert_data[21,"Species"] <- as.character("Cjac X2")
   
   HTR4_vert_data[22,"Species"] <- as.character("Mmur X1")
  
   HTR4_vert_data[23,"Species"] <- as.character("Mmur X2")
  
   HTR4_vert_data[24,"Species"] <- as.character("Mmur X3")
  
   HTR4_vert_data[25,"Species"] <- as.character("Mmur X4")
  
   HTR4_vert_data[26,"Species"] <- as.character("Mmus 1")
  
   HTR4_vert_data[27,"Species"] <- as.character("Mmus 2")
  
   HTR4_vert_data[28,"Species"] <- as.character("Mmus 3")
  
   HTR4_vert_data[29,"Species"] <- as.character("Mmus 4")
  
   HTR4_vert_data[30,"Species"] <- as.character("Mmus 5")
  
   HTR4_vert_data[31,"Species"] <- as.character("Rnor X1")
  
   HTR4_vert_data[32,"Species"] <- as.character("Rnor X2")
  
   HTR4_vert_data[33,"Species"] <- as.character("Pcat X1")
  
   HTR4_vert_data[34,"Species"] <- as.character("Pcat X2")
  
   HTR4_vert_data[35,"Species"] <- as.character("Pcat X3")
  
   HTR4_vert_data[36,"Species"] <- as.character("Btau X1")
  
   HTR4_vert_data[37,"Species"] <- as.character("Btau X2")
  
   HTR4_vert_data[38,"Species"] <- as.character("Btau X3")
  
   HTR4_vert_data[39,"Species"] <- as.character("Btau X4")
  
   HTR4_vert_data[40,"Species"] <- as.character("Chir X1")
  
   HTR4_vert_data[41,"Species"] <- as.character("Chir X2")
  
   HTR4_vert_data[44,"Species"] <- as.character("Ocun X1")
  
   HTR4_vert_data[45,"Species"] <- as.character("Ocun X2")
  
   HTR4_vert_data[46,"Species"] <- as.character("Cfam X1")
  
   HTR4_vert_data[47,"Species"] <- as.character("Cfam X2")
  
   HTR4_vert_data[48,"Species"] <- as.character("Cfam X3")
  
   HTR4_vert_data[49,"Species"] <- as.character("Fcat X1")
  
   HTR4_vert_data[50,"Species"] <- as.character("Fcat X2")
  
   HTR4_vert_data[51,"Species"] <- as.character("Fcat X3")
  
   HTR4_vert_data[52,"Species"] <- as.character("Nmel X1")
  
   HTR4_vert_data[53,"Species"] <- as.character("Nmel X2")
  
   HTR4_vert_data[54,"Species"] <- as.character("Nmel X3")
  
   HTR4_vert_data[55,"Species"] <- as.character("Ggal X1")
  
   HTR4_vert_data[56,"Species"] <- as.character("Ggal X2")
  
   HTR4_vert_data[58,"Species"] <- as.character("Xmac X1")
  
   HTR4_vert_data[59,"Species"] <- as.character("Xmac X2")
  
   HTR4_vert_data[60,"Species"] <- as.character("Xmac X3")
  
   HTR4_vert_data[61,"Species"] <- as.character("Xmac X4")
  
   HTR4_vert_data[62,"Species"] <- as.character("Drer X1")
  
   HTR4_vert_data[63,"Species"] <- as.character("Drer X2")
  
  # returning data frame to factor 
  
   HTR4_vert_data$Species <- factor(HTR4_vert_data$Species, levels = unique (HTR4_vert_data$Species))
  
   class(HTR4_vert_data$Species)
  
  # remove the dog, sheep and chimp X3 seq which hurts data 
  
   HTR4_vert_data <- HTR4_vert_data[-c(9,42,46,47,48),]  %>% droplevels()
  
  
  # plot 
  
   HTR4_motif_plot <- 
    ggplot() +
    labs(title = "5-HTR4") +
    scale_y_continuous(breaks = seq(35, 560, 50)) +
    geom_bar(data = HTR4_vert_data, aes(x = Species, y = Length), stat = "identity", width = 0.2, col = "black", fill = "white") +
    geom_text(data = HTR4_vert_data, aes(x = Species, y = Length, label = Length), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR4_vert_data, aes(x = Species, y = D2.50, colour = "D2.50"), show.legend = FALSE) +
    geom_text(data = HTR4_vert_data, aes(x =  Species, y = D2.50, label = D2.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR4_vert_data, aes(x =  Species, y = D3.49, colour = "D3.49"), show.legend = FALSE) +
    geom_text(data = HTR4_vert_data, aes(x =  Species, y = D3.49, label = D3.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR4_vert_data, aes(x =  Species, y = N7.49, colour = "N7.49"), show.legend = FALSE) +
    geom_text(data = HTR4_vert_data, aes(x =  Species, y = N7.49, label = N7.49), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR4_vert_data, aes(x =  Species, y = I3.40, colour = "I3.40"), show.legend = FALSE) +
    geom_text(data = HTR4_vert_data, aes(x =  Species, y = I3.40, label = I3.40), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR4_vert_data, aes(x =  Species, y = P5.50, colour = "P5.50"), show.legend = FALSE) +
    geom_text(data = HTR4_vert_data, aes(x =  Species, y = P5.50, label = P5.50), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    geom_point(data = HTR4_vert_data, aes(x =  Species, y = F6.44, colour = "F6.44"), show.legend = FALSE) +
    geom_text(data = HTR4_vert_data, aes(x =  Species, y = F6.44, label = F6.44), hjust = 0, vjust = 0, size = 1.8, nudge_x = 0.2) +
    scale_color_manual(values = c("D3.49" = "#00B81F", "N7.49" = "#00A5FF", "D2.50" = "#FFC425", "I3.40" = "#FC61D5", "P5.50" = "#FC61D5", "F6.44" = "#FC61D5"))+
    theme_tufte() +
    theme(axis.line = element_line(), 
          axis.title.x = element_blank(), 
          axis.text.x = element_text(size = 8), 
          axis.title.y = element_blank(),
          axis.text.y = element_text(size = 8, hjust = 0, vjust = 0), 
          plot.title = element_text(size = 8), 
          panel.background = element_blank(), 
          legend.text = element_blank(), 
          legend.title = element_blank()) +
    coord_flip(ylim = c(35, 560)) +
    scale_x_discrete(limits = rev(levels(HTR4_vert_data$Species))) 
  
  
   HTR4_motif_plot

#################################################################

######### CHRONOGRAMS ##########################################

#################################################################

# install.views('Phylogenetics')
  
   update.views('Phylogenetics')

# plotting
   
   ## mrds for the time scale
   
   ## geom_tippoint to colour tips 
   
   ## scale colour for a nice paletter
   
   ## geom_label2 to label heights of every node
   
   ## geom_nodelab to label posterior along the branch
   
   ## geom_hilight to highlight clades 
   
   ## theme_tree2() to get the time tree
   
   ## theme() to place the legend of the tip points and to font size
   
   ## labs to caption
   
   ## REVTS() is not working after updating so I will draw the timeline manually
   
# Strict clock 
   
  # pre- step 1: prep the beast file. 
  
  ## once you make duplicates of tip labels there is no more opening of it in figtree.
  
  ## make sure both taxa name and the phylogenetic data are changed
  
  ## load at every stage to make sure it's alright.
  
  # pre- step 2: reading the tree file
  
   strict_tree <- read.beast(file="input/1994_str_30M_4_MRCA_thin2.tre")
  
  # step 1: plotting just the branches
  
    plot1 <- ggtree(strict_tree, layout = "rectangular")
  
    plot(plot1)
  
  
  # step 2: finding the key nodes
  
    ## check with the PDF from the figtree output 
  
    plot2 <- ggtree(strict_tree, aes( color = branch.length)) +
    geom_tiplab(size =3, color="black") +
    geom_label2(aes(subset=!isTip, label=node), size =3, color="black", alpha=0.5) +
    scale_color_continuous(low='white', high='black', name="Branch length (my)") +
    theme(legend.position="bottom")
  
    plot(plot2)
  
   # step 3: plotting the tree, 
  
    plot3 <- ggtree(strict_tree, right = FALSE, mrsd="2000-01-01") +
      geom_tippoint(aes(color= label), shape = 16, size=.9, alpha = 0.75,  position = "identity") +
      scale_color_manual(values=c("black", "yellow", "purple2", "darkred", "blue", "orange", "green", "pink3")) +
      geom_label2(aes(subset= node == 133, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 132, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 139, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 182, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 200, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 221, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 227, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 148, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 156, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 157, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 170, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 134, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 228, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 229, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 230, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 199, label= round(height, 2)), size =3, color="black") +
      geom_nodelab(aes(x=branch, subset= node == 133, label= round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
      geom_nodelab(aes(x=branch, subset= node == 132, label= round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
      geom_nodelab(aes(x=branch, subset= node == 139, label=round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
      geom_nodelab(aes(x=branch, subset= node == 182, label=round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
      geom_nodelab(aes(x=branch, subset= node == 200, label=round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
      geom_nodelab(aes(x=branch, subset= node == 221, label=round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
      geom_nodelab(aes(x=branch, subset= node == 227, label=round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
      geom_nodelab(aes(x=branch, subset= node == 199, label=round(posterior, 3)), vjust= 2, hjust =1, size =3, color = "red", fontface = "bold") +
      geom_hilight(node=133, fill="slateblue", alpha=0.3) +
      geom_hilight(node=139, fill="skyblue", alpha=0.4) +
      geom_hilight(node=182, fill="orchid", alpha=0.4) +
      geom_hilight(node=200, fill="lightsalmon", alpha=0.4) +
      geom_hilight(node=221, fill="khaki", alpha=0.4) +
      geom_hilight(node=227, fill="palegreen", alpha=0.4) +
      theme_tree() +
      theme(legend.position="right", legend.text = element_text(size = 10)) +
      labs(caption="Time (mya)")
    
    plot(plot3)
    
    # step 4: flipping the time axis

    # plot4 <- revts(plot3)
    #
    # plot(plot4)
    #
    # # step 5: scaling it nicely
    #
    # plot5 <- plot3 +
    #   scale_x_continuous(breaks = c(-1200, -1100, -1000, -900, -800, -700, -600, -500, -400, -300, -200, -100, 0)) +
    #   theme(axis.text.x = element_text(size = 10), plot.caption = element_text(size =11))
    #
    # plot(plot5)
    #

  # post step 1:
  ## label the clades
  ## label = "Taxa" 


# exponential clock 
    
  # step 1
  
    expo_tree <- read.beast(file="input/1994_rlxE_50M_4_MRCA_thin2.tre")
  
  # step 1
  
    plot6 <- ggtree(expo_tree, layout = "rectangular")
  
    plot(plot6)
  
  # step 2
  
    plot7 <- ggtree(expo_tree, aes( color = branch.length)) +
    geom_tiplab(size =3, color="black") +
    geom_label2(aes(subset=!isTip, label=node), size =3, color="black", alpha=0.5) +
    scale_color_continuous(low='white', high='black', name="Branch length (my)") +
    theme(legend.position="bottom")
  
    plot(plot7)
  
  # step 3
  
    plot8 <- ggtree(expo_tree, right = FALSE, mrsd="2000-01-01") +
    geom_tippoint(aes(color= label), shape = 16, size=.9, alpha = 0.75,  position = "identity") +
    scale_color_manual(values=c("black", "yellow", "purple2", "darkred", "blue", "orange", "green", "pink3", "pink2")) +
    geom_label2(aes(subset= node == 132, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 131, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 138, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 181, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 200, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 231, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 252, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 199, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 130, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 200, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 201, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 202, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 147, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 155, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 148, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 198, label= round(height, 2)), size =3, color="black") +
    geom_nodelab(aes(x=branch, subset= node == 132, label= round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 131, label= round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 138, label=round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 181, label=round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 199, label=round(posterior, 3)), vjust= -1, hjust = 1.5, size =3, color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 200, label=round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 231, label=round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 252, label=round(posterior, 3)), vjust= 2, hjust =1, size =3, color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 198, label=round(posterior, 3)), vjust= -1.5, hjust =.5, size =3, color = "red", fontface = "bold") +
    geom_hilight(node=132, fill="slateblue", alpha=0.3) +
    geom_hilight(node=138, fill="skyblue", alpha=0.4) +
    geom_hilight(node=181, fill="orchid", alpha=0.4) +
    geom_hilight(node=200, fill="palegreen", alpha=0.4) +
    geom_hilight(node=231, fill="lightsalmon", alpha=0.4) +
    geom_hilight(node=252, fill="khaki", alpha=0.4) +
    theme_tree2() +
    theme(legend.position="right", legend.text = element_text(size = 10)) +
    labs(caption="Time (mya)")
  
  plot(plot8)
  
  # # step 4: flipping the time axis
  # 
  # plot9 <- revts(plot8)
  # 
  # plot(plot9)
  # 
  # # step 5: scaling it nicely
  # plot10 <- plot9 + 
  #   scale_x_continuous(breaks = c(-1000, -900, -800, -700, -600, -500, -400, -300, -200, -100, 0)) + 
  #   theme(axis.text.x = element_text(size = 10), plot.caption = element_text(size =11))
  # 
  # plot(plot10)

# lognormal tree

  log_tree <- read.beast(file="input/1994_rlxL_55M_5_MRCA_thin2.tre")

  # step 1

    plot11 <- ggtree(log_tree, layout = "rectangular")

    plot(plot11)

  # step 2
  
    plot12 <- ggtree(log_tree, aes( color = branch.length)) +
    geom_tiplab(size =3, color="black") +
    geom_label2(aes(subset=!isTip, label=node), size =3, color="black", alpha=0.5) +
    scale_color_continuous(low='white', high='black', name="Branch length (my)") +
    theme(legend.position="bottom")
  
  plot(plot12)
  
  # step 3
  
  plot13 <- ggtree(log_tree, right = FALSE, mrsd="2000-01-01") +
    geom_tippoint(aes(color= label), shape = 16, size=.9, alpha = 0.75,  position = "identity") +
    scale_color_manual(values=c("black", "yellow", "purple2", "darkred", "blue", "orange", "green", "pink3", "pink2")) +
    geom_label2(aes(subset= node == 132, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 131, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 138, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 181, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 200, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 231, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 252, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 199, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 130, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 200, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 201, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 202, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 147, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 155, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 148, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 198, label= round(height, 2)), size =3, color="black") +
    geom_nodelab(aes(x=branch, subset= node == 132, label= round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 131, label= round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 138, label=round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 181, label=round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 199, label=round(posterior, 3)), vjust= -1, hjust = 1.5, size =3, color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 200, label=round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 231, label=round(posterior, 3)), vjust=-.5, hjust = 1, size =3, color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 252, label=round(posterior, 3)), vjust= 2, hjust =1, size =3, color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 198, label=round(posterior, 3)), vjust= -1.5, hjust =.5, size =3, color = "red", fontface = "bold") +
    geom_hilight(node=132, fill="slateblue", alpha=0.3) +
    geom_hilight(node=138, fill="skyblue", alpha=0.4) +
    geom_hilight(node=181, fill="orchid", alpha=0.4) +
    geom_hilight(node=200, fill="palegreen", alpha=0.4) +
    geom_hilight(node=231, fill="lightsalmon", alpha=0.4) +
    geom_hilight(node=252, fill="khaki", alpha=0.4) +
    theme_tree2() +
    theme(legend.position="right", legend.text = element_text(size = 10)) +
    labs(caption="Time (mya)")
  
  plot(plot13)

  # step 4: flipping the time axis
  
  # plot14 <- revts(plot13)
  # 
  # plot(plot14)
  # 
  # # step 5: scaling it nicely
  # 
  # plot15 <- plot14 + 
  #   scale_x_continuous(breaks = c(-1000, -900, -800, -700, -600, -500, -400, -300, -200, -100, 0)) + 
  #   theme(axis.text.x = element_text(size = 10), plot.caption = element_text(size =11))
  # 
  # plot(plot15)
  

#######################################################################

### combined images #########################################
  
######################################################################  
  

# motif plots 
  
  HTR_p1 <- plot_grid(HTR1A_motif_plot, HTR2A_new_motif_plot,
                    labels = c("A", "B"),
                    ncol = 1)

  ggsave("output/HTR_p1.pdf", width = 20, height = 20, units = "cm")


  HTR4_motif_plot

  ggsave("output/HTR4_motif_plot.pdf", width = 20, height = 20, units = "cm")


  HTR_p2 <- plot_grid(HTR1B_motif_plot, HTR1D_motif_plot, HTR1E_motif_plot, HTR1F_motif_plot,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2,
                    heights = c(2, 2)
)

  ggsave("output/HTR_p2.pdf", width = 20, height = 20, units = "cm")


  HTR_p3 <- plot_grid(HTR2B_new_motif_plot, HTR2C_motif_plot, HTR5A_motif_plot, HTR6_motif_plot,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2,
                    heights = c(2, 2)
)

  ggsave("output/HTR_p3.pdf", width = 20, height = 20, units = "cm")

  HTR7_motif_plot

  ggsave("output/HTR_7_motif_plot.pdf", width = 20, height = 20, units = "cm")

  