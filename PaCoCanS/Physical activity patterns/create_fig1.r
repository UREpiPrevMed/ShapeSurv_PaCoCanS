# Figure 1 ------------------------------------------------------------------------------------
# This code reproduces Figure 1 from Stein et al. 2023 (not fully styled)

## Figure 1A
# Text for fraction variance explained
fve <- c(D_pca[["cumFVE"]][1],
         D_pca[["cumFVE"]][2]-D_pca[["cumFVE"]][1],
         D_pca[["cumFVE"]][3]-D_pca[["cumFVE"]][2],
         D_pca[["cumFVE"]][4]-D_pca[["cumFVE"]][3] ) %>%
    round(.,2)*100 #round and get percentage

fve <- c("fPC1"=paste0("fPC1: ",fve[1],"%"),
         "fPC2"=paste0("fPC2: ",fve[2],"%"),
         "fPC3"=paste0("fPC3: ",fve[3],"%"),
         "fPC4"=paste0("fPC4: ",fve[4],"%") )

pca_data <- reshape::melt(pca_data,c("Time")) #melt for plotting

Gpca1 <- ggplot() + geom_hline(yintercept=0, color="grey92", size=1) +
    geom_line(data=subset(pca_data, variable=="fPC1"),aes(x=Time, y=value), color="darkred", linewidth=1, show.legend = F) +
    facet_wrap(~variable, nrow=1, labeller = as_labeller(fve)) +
    scale_x_continuous(breaks=seq(0,24,2)) +
    scale_y_continuous(limits=c(-0.55,0.55), expand = c(0, 0)) +
    labs(title="A", y="value of fPC curve") + theme_minimal() +
    theme(panel.grid.minor = element_blank()) 
Gpca2 <- ggplot() + geom_hline(yintercept=0, color="grey92", size=1) +
    geom_line(data=subset(pca_data, variable=="fPC2"),aes(x=Time, y=value),color="darkblue", linewidth=1, show.legend = F) +
    facet_wrap(~variable, nrow=1, labeller = as_labeller(fve)) +
    scale_x_continuous(breaks=seq(0,24,2)) +
    scale_y_continuous(limits=c(-0.55,0.55), expand = c(0, 0)) +
    labs(title="", y="") + theme_minimal() +
    theme(panel.grid.minor = element_blank()) 
Gpca3 <- ggplot() + geom_hline(yintercept=0, color="grey92", size=1) +
    geom_line(data=subset(pca_data, variable=="fPC3"),aes(x=Time, y=value),color="orange", linewidth=1, show.legend = F) +
    facet_wrap(~variable, nrow=1, labeller = as_labeller(fve)) +
    scale_x_continuous(breaks=seq(0,24,2)) +
    scale_y_continuous(limits=c(-0.55,0.55), expand = c(0, 0)) +
    labs(title="", y="") + theme_minimal() +
    theme(panel.grid.minor = element_blank()) 
Gpca4 <- ggplot() + geom_hline(yintercept=0, color="grey92", size=1) +
    geom_line(data=subset(pca_data, variable=="fPC4"),aes(x=Time, y=value), color="darkgreen", linewidth=1, show.legend = F) +
    facet_wrap(~variable, nrow=1, labeller = as_labeller(fve)) +
    scale_x_continuous(breaks=seq(0,24,2)) +
    scale_y_continuous(limits=c(-0.55,0.55), expand = c(0, 0)) +
    labs(title="", y="") + theme_minimal() +
    theme(panel.grid.minor = element_blank()) 

Gpca <- gridExtra::grid.arrange(Gpca1,Gpca2,Gpca3,Gpca4, nrow=1)

## Figure 1B
# Plot average activity of postives and negatives on PCs
pos <- list(); neg <- list() # create lists of positive scorers and negative scorers
for (fpc in c("fPC1","fPC2","fPC3","fPC4")){
    pos <- append(pos, list(fpc = data.frame("ID" = pca_scores$ID[which(pca_scores[[fpc]] > mean(pca_scores[[fpc]]) + sd(pca_scores[[fpc]]))] ))) # at least on SD above mean
    neg <- append(neg, list(fpc = data.frame("ID" = pca_scores$ID[which(pca_scores[[fpc]] < mean(pca_scores[[fpc]]) - sd(pca_scores[[fpc]]))] ))) # one SD below
}
names(pos) <- c("fPC1","fPC2","fPC3","fPC4")
names(neg) <- c("fPC1","fPC2","fPC3","fPC4")

# calc population mea
pop_mean <- aggregate(df$ENMO, by=list(df$time), FUN="mean")$x
# extract ENMOs for IDs of positive and negative scorers for each fPC
pos1 <- df[which(df$f.eid %in% pos$fPC1[["ID"]] ) ,]
pos2 <- df[which(df$f.eid %in% pos$fPC2[["ID"]] ) ,]
pos3 <- df[which(df$f.eid %in% pos$fPC3[["ID"]] ) ,]
pos4 <- df[which(df$f.eid %in% pos$fPC4[["ID"]] ) ,]
neg1 <- df[which(df$f.eid %in% neg$fPC1[["ID"]] ) ,]
neg2 <- df[which(df$f.eid %in% neg$fPC2[["ID"]] ) ,]
neg3 <- df[which(df$f.eid %in% neg$fPC3[["ID"]] ) ,]
neg4 <- df[which(df$f.eid %in% neg$fPC4[["ID"]] ) ,]

df_pos_neg <- data.frame("Time"=0:23,
                         "Mean"=pop_mean,
                         "pos1"=aggregate(pos1$ENMO, by=list(pos1$time), FUN="mean")$x,
                         "pos2"=aggregate(pos2$ENMO, by=list(pos2$time), FUN="mean")$x,
                         "pos3"=aggregate(pos3$ENMO, by=list(pos3$time), FUN="mean")$x,
                         "pos4"=aggregate(pos4$ENMO, by=list(pos4$time), FUN="mean")$x,
                         "neg1"=aggregate(neg1$ENMO, by=list(neg1$time), FUN="mean")$x,
                         "neg2"=aggregate(neg2$ENMO, by=list(neg2$time), FUN="mean")$x,
                         "neg3"=aggregate(neg3$ENMO, by=list(neg3$time), FUN="mean")$x,
                         "neg4"=aggregate(neg4$ENMO, by=list(neg4$time), FUN="mean")$x
)
rm(pos1,pos2,pos3,pos4,neg1,neg2,neg3,neg4)

df_pos_neg$label <- rep("lab",nrow(df_pos_neg)) # placeholder for fPCs contrast labelling later
xpos <- 18.1; ypos <- 72.5
lab <- paste0("Daily mean\n","fPC1+   ",round(mean(df_pos_neg$pos1),digits=2),"\n", "fPC1-   ",round(mean(df_pos_neg$neg1),digits=2)) # label for overall average
pc1 <- ggplot(data=df_pos_neg) +
    geom_line(aes(x=Time, y=Mean), color="grey", size=1, show.legend = F) +
    geom_line(aes(x=Time, y=pos1), linetype="dashed", color="darkred", size=1, show.legend = F) +
    geom_line(aes(x=Time, y=neg1), linetype="dotted", color="darkred", size=1, show.legend = F) +
    facet_wrap(~label, labeller=as_labeller(c("lab"="High vs. Low"))) +
    geom_label(data=data.frame(),aes(x=xpos,y=ypos,label=lab),color="darkred", size=3, fontface="bold",hjust = 0) +
    scale_x_continuous(breaks=seq(0,24,2)) + 
    scale_y_continuous(limits=c(0,80), expand = c(0, 0)) +
    labs(title="B1") + xlab("Time") + ylab(expression(bold("ENMO (m"*bolditalic(g)*")"))) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) 
lab <- paste0("Daily mean\n","fPC2+   ",round(mean(df_pos_neg$pos2),digits=2),"\n", "fPC2-   ",round(mean(df_pos_neg$neg2),digits=2)) # label for overall average
pc2 <- ggplot(data=df_pos_neg) +
    geom_line(aes(x=Time, y=Mean), color="grey", size=1, show.legend = F) +
    geom_line(aes(x=Time, y=pos2), linetype="dashed", color="darkblue", size=1, show.legend = F) +
    geom_line(aes(x=Time, y=neg2), linetype="dotted", color="darkblue", size=1, show.legend = F) +
    facet_wrap(~label, labeller=as_labeller(c("lab"="Early vs. Late"))) +
    geom_label(data=data.frame(),aes(x=xpos,y=ypos,label=lab),color="darkblue", size=3, fontface="bold",hjust = 0) +
    scale_x_continuous(breaks=seq(0,24,2)) +
    scale_y_continuous(limits=c(0,80), expand = c(0, 0)) +
    labs(title="B2") + xlab("Time") + ylab("") + 
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) 
lab <- paste0("Daily mean\n","fPC3+   ",round(mean(df_pos_neg$pos3),digits=2),"\n", "fPC3-   ",round(mean(df_pos_neg$neg3),digits=2)) # label for overall average
pc3 <- ggplot(data=df_pos_neg) +
    geom_line(aes(x=Time, y=Mean), color="grey", size=1, show.legend = F) +
    geom_line(aes(x=Time, y=pos3), linetype="dashed", color="orange", size=1, show.legend = F) +
    geom_line(aes(x=Time, y=neg3), linetype="dotted", color="orange", size=1, show.legend = F) +
    facet_wrap(~label, labeller=as_labeller(c("lab"="Midday vs. Early and Late"))) +
    geom_label(data=data.frame(),aes(x=xpos,y=ypos,label=lab),color="orange", size=3, fontface="bold",hjust = 0) +
    scale_x_continuous(breaks=seq(0,24,2)) + 
    scale_y_continuous(limits=c(0,80), expand = c(0, 0)) +
    labs(title="B3") + xlab("Time") + ylab("") + 
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) 
lab <- paste0("Daily mean\n","fPC4+   ",round(mean(df_pos_neg$pos4),digits=2),"\n", "fPC4-   ",round(mean(df_pos_neg$neg4),digits=2)) # label for overall average
pc4 <- ggplot(data=df_pos_neg) +
    geom_line(aes(x=Time, y=Mean), color="grey", size=1, show.legend = F) +
    geom_line(aes(x=Time, y=pos4), linetype="dashed", color="darkgreen", size=1, show.legend = F) +
    geom_line(aes(x=Time, y=neg4), linetype="dotted", color="darkgreen", size=1, show.legend = F) +
    facet_wrap(~label, labeller=as_labeller(c("lab"="Midday and Night vs. Early and Evening"))) +
    geom_label(data=data.frame(),aes(x=xpos,y=ypos,label=lab),color="darkgreen", size=3, fontface="bold",hjust = 0) +
    scale_x_continuous(breaks=seq(0,24,2)) +
    scale_y_continuous(limits=c(0,80), expand = c(0, 0)) +
    labs(title="B4") + xlab("Time") + ylab("") + 
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) 


Gscorer <- gridExtra::grid.arrange(pc1,pc2,pc3,pc4, nrow=1)

gridExtra::grid.arrange(Gpca,Gscorer,heights=c(1,2) )

