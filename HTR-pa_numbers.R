# setting dir

setwd("C:/Users/sabrinai/OneDrive - The University of Melbourne/PHD/Chapter1/Paper")


#################################################################

######## PACKAGES ###############################################

#################################################################

# tidy data 

  library(Rcpp) # for dplyr 
  
  library(dplyr) # for tidyverse

  library(tidyr) #  for organizing data

#################################################################

######## DATA FORMAT & INPUT ####################################

#################################################################

# Reading data in the tidyverse format 

  HTR_Data <- read.csv("input/HTR_data.csv")

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
  
  HTR_vert_data <- HTR_Data %>% filter(Species != "Drosophila_melanogester_(Drosophila)") %>% droplevels()

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
  
  HTR_vert_data$Species <- as.factor(species_shrt[ HTR_vert_data$Species])

  class(HTR_vert_data$Species) # Factor 

  HTR_vert_data$Species <- factor(HTR_vert_data$Species, levels = unique (HTR_vert_data$Species)) # Applied "unique" to show the unique values  

  class(HTR_vert_data$Species)  # Factor
  
  levels(HTR_vert_data$Species) # All Species shorthanded 
  
  
#################################################################
  
  # subsetting each sequences for plotting motif and PA
  
  # filtering by receptors
  
  # droplevels to remove unused levels 
  
  # subsetting 5-HTR 1A
  
  HTR1A_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR1A") %>% droplevels()
  
  str(HTR1A_vert_data)
  
  class(HTR1A_vert_data$Species) # Factor 
  
  # subsetting 5-HTR 1B
  
  HTR1B_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR1B")  %>% droplevels()

  str(HTR1B_vert_data)
  
  # subsetting 5-HTR 1D
  
  HTR1D_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR1D")  %>% droplevels()
  
  str(HTR1D_vert_data)
  
  # subsetting 5-HTR 1E
  
  HTR1E_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR1E")  %>% droplevels()
  
  str(HTR1E_vert_data)
  
  # subsetting 5-HTR 1F
  
  HTR1F_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR1F")  %>% droplevels()
  
  str(HTR1F_vert_data)
  
  # subsetting 5-HTR 2A
  
  HTR2A_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR2A")  %>% droplevels()
  
  str(HTR2A_vert_data)
  
  # subsetting 5-HTR 2B
  
  HTR2B_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR2B")  %>% droplevels()
  
  str(HTR2B_vert_data)
  
  # new 2B without the data points from the transcript variant 2
  
  HTR2B_new_data <- HTR2B_vert_data[-c(2, 4, 6, 8, 11, 13, 20, 22, 24),]
  
  str(HTR2B_new_data)
  
  # subsetting 5-HTR 2C
  
  HTR2C_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR2C")  %>% droplevels()
  
  str(HTR2C_vert_data)
  
  # removing the chicken and guinea fowl sequences 
  
  HTR2C_new_data <- HTR2C_vert_data[-c(18,20),]
  
  str(HTR2C_new_data)
  
  # subsetting 5-HTR 4
  
  HTR4_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR4")  %>% droplevels()
  
  str(HTR4_vert_data)
  
  # subsetting 5-HTR 5A
  
  HTR5A_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR5A")  %>% droplevels()
  
  str(HTR5A_vert_data)
  
  # subsetting 5-HTR 5B
  
  HTR5B_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR5B")  %>% droplevels()
  
  str(HTR5B_vert_data)
  
  # subsetting 5-HTR 6
  
  HTR6_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR6")  %>% droplevels()
  
  # subsetting 5-HTR 7
  
  HTR7_vert_data <- filter (HTR_vert_data, Receptor == "5-HTR7")  %>% droplevels()
  
  str(HTR7_vert_data)

  
##############################################################################

vert_PA_data <- HTR_vert_data %>% 
    select (Receptor, Species, Identity, Similarity)
  
# filtering out human values with !=
  
  vert_PA_data <- vert_PA_data %>% filter(Species != c( "Hsap" )) 
  
# list
  
  vert_pa <- list()
  
# index for loop
  
  htrlist <- levels(as.factor(vert_PA_data$Receptor))
  
# for loop
  
  for (i in 1:length(htrlist)) {
      pa_i <- cbind( 
      vert_PA_data %>% filter (Receptor %in% htrlist[i]) %>% 
        filter (Identity %in% min(Identity)),
      
      vert_PA_data %>% filter (Receptor %in% htrlist[i]) %>% 
        filter (Identity %in% max(Identity)),
      
      vert_PA_data %>% filter (Receptor %in% htrlist[i]) %>%
        summarise (median = median(Identity)),  
      
      vert_PA_data %>% filter (Receptor %in% htrlist[i]) %>%
        summarise (IQR = IQR(Identity))
      )
      
      vert_pa[[i]] <- pa_i
  }
  
  vert_pa
  
# summary data 
  
  vert_summ <- do.call(rbind, vert_pa) # binding all mean values 
  
# removing the middle colummn 
  
  vert_PA_summary_rd <- vert_summ [,-c(5)]

  vert_PA_summary_rd
  
  
# removing the duplicate 
  
  vert_PA_summary_rd <- vert_PA_summary_rd [-c(5, 10, 14),]
  
# round IQR
  
  vert_PA_summary_rd$IQR <- round(vert_PA_summary_rd$IQR, 2)
  
# renaming header rows with names ()
  
  names(vert_PA_summary_rd) <- c("Receptor", "Species with the minimum identity%", "Min ident%", "Min sim%", 
                                 "Species with the maximum identity%", "Max ident%", "Max sim%", "Median iden%", "IQR iden%")
  
  
# latex
  
  vert_PA_summary_rd
  
  write.csv(vert_PA_summary_rd, "pa.csv")
  
# from csv removed first empty column, replaced 5-HTR with HTR, and species acronymn with species name
  
# in notepad replaced "," with "&" for latex and added \\ end of each row   