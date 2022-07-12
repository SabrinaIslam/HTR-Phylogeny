# setting dir

setwd("C:/Users/sabrinai/OneDrive - The University of Melbourne/PHD/Chapter1/Paper")


#################################################################

######## PACKAGES ###############################################

#################################################################

# tidy data 

  library(Rcpp) # for dplyr 
  
  library(dplyr) # for tidyverse

  library(tidyr) #  for organizing data

  library(tidyverse) # for organizing data and ggplot2

# plotting 

  library(ggplot2) # for plotting 

  library(ggthemes) # for applying themes to the plots by gg

  library(cowplot) # for combining figures 

  library(magick) # for figures 

# phylogeny 

  library(ctv) # with ape
  
  library(ape) # for phylo object 

  library(phangorn) # for phylo object

  library(phytools) # for phylo object 

  library(tm) # unsure why

  library(treeio) # for phylo

  library(ggtree) # for tree element 


#################################################################

######### CHRONOGRAMS ##########################################

#################################################################

# install.views('Phylogenetics')
#   
#    update.views('Phylogenetics')

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
  
    plot3 <- ggtree(strict_tree, size = 0.5,  mrsd="2000-01-01") + # aes(color=label),
      # geom_tippoint(aes(color= label), shape = 16, size= 0.8 , position = "identity") +
      # geom_tiplab(aes(color= label), size = 1.5) +
      scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#f781bf", "#a65628", "#8dd3c7")) +
      labs(colour = "Species") +
      geom_label2(aes(subset= node == 131, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 133, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 139, label= round(height, 2)), size =3, color="black") +
      geom_label2(aes(subset= node == 158, label= round(height, 2)), size =3, color="black") +
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
      geom_nodelab(aes(x=branch, subset= node == 131, label= round(posterior, 3)), vjust=-.5, hjust = 1, size = 4, color = "red", fontface = "bold") +
      geom_nodelab(aes(x=branch, subset= node == 139, label= round(posterior, 3)), vjust=-.5, hjust = 1, size = 4, color = "red", fontface = "bold") +
      geom_nodelab(aes(x=branch, subset= node == 132, label= round(posterior, 3)), vjust=-.5, hjust = 1, size = 4, color = "red", fontface = "bold") +
      geom_nodelab(aes(x=branch, subset= node == 158, label=round(posterior, 3)), vjust=-.5, hjust = 1, size = 4, color = "red", fontface = "bold") +
      geom_nodelab(aes(x=branch, subset= node == 182, label=round(posterior, 3)), vjust=-.5, hjust = 1, size = 4, color = "red", fontface = "bold") +
      geom_nodelab(aes(x=branch, subset= node == 200, label=round(posterior, 3)), vjust=-.5, hjust = 1, size = 4, color = "red", fontface = "bold") +
      geom_nodelab(aes(x=branch, subset= node == 221, label=round(posterior, 3)), vjust=-.5, hjust = 1, size = 4, color = "red", fontface = "bold") +
      geom_nodelab(aes(x=branch, subset= node == 227, label=round(posterior, 3)), vjust=-.5, hjust = 1, size = 4, color = "red", fontface = "bold") +
      geom_nodelab(aes(x=branch, subset= node == 199, label=round(posterior, 3)), vjust= 2, hjust =1, size = 4, color = "red", fontface = "bold") +
      theme_tree2() +
      theme(legend.position="none", 
        legend.text = element_text(size = 10)) +
      labs(caption="Time (mya)")
    
    plot(plot3)
    
    # step 4: flipping the time axis

    plot4 <- revts(plot3)
    
    plot(plot4)
    
    # step 5: scaling it nicely + highlighting clades
    
    plot5 <- plot4 +
      geom_hilight(node=133, fill="#999999", alpha=0.3) +
      geom_hilight(node=139, fill="#999999", alpha=0.4) +
      geom_hilight(node=182, fill="#666666", alpha=0.4) +
      geom_hilight(node=200, fill="#666666", alpha=0.4) +
      geom_hilight(node=221, fill="#666666", alpha=0.4) +
      geom_hilight(node=227, fill="#EEEEEE", alpha=0.4) +
      scale_x_continuous(breaks = c(-1300, -1200, -1100, -1000, -900, -800, -700, -600, -500, -400, -300, -200, -100, 0)) +
      theme(axis.text.x = element_text(size = 10), plot.caption = element_text(size =11))
    
    plot(plot5)

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
  
  plot8 <- ggtree(expo_tree, size = 0.5,  mrsd="2000-01-01") + # aes(color=label),
    # geom_tippoint(aes(color= label), shape = 16, size= 0.8, position = "identity") +
    # geom_tiplab(aes(color= label), size = 1.5) +
    scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#f781bf", "#a65628", "#8dd3c7")) +
    labs(colour = "Species") +
    geom_label2(aes(subset= node == 130, label= round(height, 2)), size =3, color="black") +
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
    geom_nodelab(aes(x=branch, subset= node == 130, label= round(posterior, 3)), vjust=-.5, hjust = 1, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 132, label= round(posterior, 3)), vjust=-.5, hjust = 1, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 131, label= round(posterior, 3)), vjust=-.5, hjust = 1, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 138, label=round(posterior, 3)), vjust=-.5, hjust = 1, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 181, label=round(posterior, 3)), vjust=-.5, hjust = 1, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 199, label=round(posterior, 3)), vjust= -1, hjust = 1.5, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 200, label=round(posterior, 3)), vjust=-.5, hjust = 1, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 231, label=round(posterior, 3)), vjust=-.5, hjust = 1, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 252, label=round(posterior, 3)), vjust= 2, hjust =1, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 198, label=round(posterior, 3)), vjust= -1.5, hjust =.5, size = 4 , color = "red", fontface = "bold") +
    theme_tree2() +
    theme(legend.position="right", legend.text = element_text(size = 10)) +
    labs(caption="Time (mya)")
  
  plot(plot8)
  
  # step 4: flipping the time axis
  
  plot9 <- revts(plot8)
   
  plot(plot9)

  # step 5: scaling it nicely + highlighting clades
  plot10 <- plot9 + 
    geom_hilight(node=132, fill="#999999", alpha=0.3) +
    geom_hilight(node=138, fill="#999999", alpha=0.4) +
    geom_hilight(node=181, fill="#666666", alpha=0.4) +
    geom_hilight(node=200, fill="#EEEEEE", alpha=0.4) +
    geom_hilight(node=231, fill="#666666", alpha=0.4) +
    geom_hilight(node=252, fill="#666666", alpha=0.4) +
    scale_x_continuous(breaks = c(-1000, -900, -800, -700, -600, -500, -400, -300, -200, -100, 0)) + 
    theme(axis.text.x = element_text(size = 10), plot.caption = element_text(size =11))
  
  plot(plot10)
  
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
  
  plot13 <- ggtree(log_tree,size = 0.5, mrsd="2000-01-01") + #  aes(color=label),
    # geom_tippoint(aes(color= label), shape = 16, size = 0.8, position = "identity") +
    # geom_tiplab(aes(color= label), size = 1.5) +
    scale_color_manual(values=c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#f781bf", "#a65628", "#8dd3c7")) +
    labs(colour = "Species") +
    geom_label2(aes(subset= node == 130, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 132, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 131, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 138, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 181, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 200, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 231, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 237, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 199, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 130, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 200, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 201, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 202, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 147, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 155, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 148, label= round(height, 2)), size =3, color="black") +
    geom_label2(aes(subset= node == 198, label= round(height, 2)), size =3, color="black") +
    geom_nodelab(aes(x=branch, subset= node == 130, label= round(posterior, 3)), vjust=-.5, hjust = 1, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 131, label= round(posterior, 3)), vjust=-.5, hjust = 1, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 132, label= round(posterior, 3)), vjust=-.5, hjust = 1, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 138, label=round(posterior, 3)), vjust=-.5, hjust = 1, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 181, label=round(posterior, 3)), vjust=-.5, hjust = 1, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 199, label=round(posterior, 3)), vjust= -1, hjust = 1.5, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 200, label=round(posterior, 3)), vjust=-.5, hjust = 1, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 231, label=round(posterior, 3)), vjust=-.5, hjust = 1, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 237, label=round(posterior, 3)), vjust= -1, hjust =1, size = 4 , color = "red", fontface = "bold") +
    geom_nodelab(aes(x=branch, subset= node == 198, label=round(posterior, 3)), vjust= -1.5, hjust =.5, size = 4 , color = "red", fontface = "bold") +
    theme_tree2() +
    theme(legend.position="right", legend.text = element_text(size = 10)) +
    labs(caption="Time (mya)")
  
  plot(plot13)

  # step 4: flipping the time axis
  
  plot14 <- revts(plot13)
   
  plot(plot14)
   
  # step 5: scaling it nicely + highlighting clades
   
  plot15 <- plot14 + 
    geom_hilight(node=132, fill="#999999", alpha=0.3) +
    geom_hilight(node=138, fill="#999999", alpha=0.4) +
    geom_hilight(node=181, fill="#666666", alpha=0.4) +
    geom_hilight(node=200, fill="#EEEEEE", alpha=0.4) +
    geom_hilight(node=231, fill="#666666", alpha=0.4) +
    geom_hilight(node=237, fill="#666666", alpha=0.4) +
    scale_x_continuous(breaks = c(-1000, -900, -800, -700, -600, -500, -400, -300, -200, -100, 0)) + 
    theme(axis.text.x = element_text(size = 10), plot.caption = element_text(size =11))
  
  plot(plot15)

  
# saving images 
  
  ggsave(file ="output/stricttree.pdf", plot = plot5, width = 297, height = 210, units = "mm")
  
  ggsave(file ="output/expotree.pdf", plot = plot10, width = 297, height = 210, units = "mm")
  
  ggsave(file ="output/logtree.pdf", plot = plot15, width = 297, height = 210, units = "mm")
  