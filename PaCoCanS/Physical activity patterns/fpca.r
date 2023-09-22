library(tidyverse)

# Load UKB data -------------------------------------------------------------------------------
fpath <- "~/" # path to .tab file from UKB
bd <- read.table(fpath, header=TRUE, sep="\t")

lvl.0009 <- c(0,1)
lbl.0009 <- c("Female","Male")
bd$f.31.0.0 <- ordered(bd$f.31.0.0, levels=lvl.0009, labels=lbl.0009) 

names(bd)[names(bd) == "f.90027.0.0"] <- "ENMO_0" 
names(bd)[names(bd) == "f.90028.0.0"] <- "ENMO_1" 
names(bd)[names(bd) == "f.90029.0.0"] <- "ENMO_2" 
names(bd)[names(bd) == "f.90030.0.0"] <- "ENMO_3" 
names(bd)[names(bd) == "f.90031.0.0"] <- "ENMO_4" 
names(bd)[names(bd) == "f.90032.0.0"] <- "ENMO_5" 
names(bd)[names(bd) == "f.90033.0.0"] <- "ENMO_6" 
names(bd)[names(bd) == "f.90034.0.0"] <- "ENMO_7" 
names(bd)[names(bd) == "f.90035.0.0"] <- "ENMO_8" 
names(bd)[names(bd) == "f.90036.0.0"] <- "ENMO_9" 
names(bd)[names(bd) == "f.90037.0.0"] <- "ENMO_10" 
names(bd)[names(bd) == "f.90038.0.0"] <- "ENMO_11" 
names(bd)[names(bd) == "f.90039.0.0"] <- "ENMO_12" 
names(bd)[names(bd) == "f.90040.0.0"] <- "ENMO_13" 
names(bd)[names(bd) == "f.90041.0.0"] <- "ENMO_14" 
names(bd)[names(bd) == "f.90042.0.0"] <- "ENMO_15" 
names(bd)[names(bd) == "f.90043.0.0"] <- "ENMO_16" 
names(bd)[names(bd) == "f.90044.0.0"] <- "ENMO_17" 
names(bd)[names(bd) == "f.90045.0.0"] <- "ENMO_18" 
names(bd)[names(bd) == "f.90046.0.0"] <- "ENMO_19" 
names(bd)[names(bd) == "f.90047.0.0"] <- "ENMO_20" 
names(bd)[names(bd) == "f.90048.0.0"] <- "ENMO_21" 
names(bd)[names(bd) == "f.90049.0.0"] <- "ENMO_22" 
names(bd)[names(bd) == "f.90050.0.0"] <- "ENMO_23" 

lvl.0007 <- c(0,1)
lbl.0007 <- c("No","Yes")
bd$f.90015.0.0 <- ordered(bd$f.90015.0.0, levels=lvl.0007, labels=lbl.0007) #data quality
bd$f.90016.0.0 <- ordered(bd$f.90016.0.0, levels=lvl.0007, labels=lbl.0007) #calibration quality

# only take valid acceleration (according to Doherty et al)
data <- bd
data <- data[which(data$f.90015.0.0 == "Yes") , ] #take only IDs with valid acceleration
data <- data[which(data$f.90016.0.0 == "Yes") , ] #take only IDs with valid calibration

#add age at accelerometry 
wt <- data$f.90010.0.0 %>% as.Date() # weartime start date
at <- data$f.53.0.0  %>% as.Date() # baseline assessment date
time_between <- difftime(wt,at,units="days") %>% as.double() # time between wear time and baseline
data$accel_age <- data$f.21022.0.0 + time_between/365 # age at accelerometery

# save(data, file="data.Rda") 

# Adjust data ------------------------------------------------------------------------
# remove 99.9th percentile
data <- data[which(data$f.90012.0.0 < quantile(data$f.90012.0.0, probs = c(0.999), na.rm=TRUE)) ,] 
# remove missing bmi
data <- data[-which(is.na(data$f.21001.0.0)), ]

#transform to long format
df <- data %>% 
    pivot_longer(
        cols = starts_with("ENMO_"),
        names_to = "time",
        names_prefix = "ENMO_",
        values_to = "ENMO",
        values_drop_na = FALSE
    )
#remove non-necessary columns (step not needed indeed)
df <- df[, c("f.eid","accel_age","f.31.0.0","f.21001.0.0","f.54.0.0","time","ENMO")]

# format time and study center
df$time <- as.numeric(df$time)
df$f.54.0.0 <- as.character(df$f.54.0.0) %>% as.factor() 

# Obtain residuals from linear model
lm <- lm(ENMO ~ accel_age + f.31.0.0 + f.21001.0.0 + f.54.0.0, data = df)
resids <- rstandard(lm) # standardize
names(resids) <- NULL
res <- data.frame("ID"=df$f.eid,
                  "Time"=df$time,
                  "Residuals"=resids)

# FPCA --------------------------------------------------------------------
# install.packages("fdapace")
# transform to fPCA list
D_trans <- fdapace::MakeFPCAInputs(res$ID, res$Time, res$Residuals)

# Calc fPCA
D_pca <- fdapace::FPCA(D_trans$Ly, D_trans$Lt,
                       list(plot = F,
                            dataType = "Sparse", #for sparse data NAs are not supported (->bmis excluded)
                            kernel = "gauss",
                            maxK = 10, # 4-6 is usually enough; here 4 >96%
                            usergrid = TRUE, #use observation grid for fitting
                            error = FALSE, 
                            verbose = FALSE
                            )
                       )

# Obtain first four variables
pca_data = data.frame("Time"=D_pca$workGrid,
                      "fPC1"=D_pca$phi[,1],
                      "fPC2"=D_pca$phi[,2],
                      "fPC3"=D_pca$phi[,3],
                      "fPC4"=D_pca$phi[,4])

# Check directions of fPCs; some might need to be mirrored
pca_long <- reshape::melt(pca_data,c("Time")) #melt for plotting
ggplot(pca_long) + geom_line(aes(x=Time,y=value,color=variable)) + facet_wrap(~variable)
# mirror for better comparability with fPC3
pca_data$fPC4 <- pca_data$fPC4 * -1 

#Obtain loading scores per participant
pca_scores <- data$f.eid
pca_scores <- cbind(pca_scores, D_pca$xiEst)
pca_scores <- pca_scores[,-c(6,7)] # remove fifth and sixth fPC
pca_scores <- as.data.frame(pca_scores)
colnames(pca_scores) <- c("ID","fPC1","fPC2","fPC3","fPC4")
pca_scores$fPC4 <- pca_scores$fPC4 * -1 # mirror

# save(pca_scores, "~/pca_scores.RData")
