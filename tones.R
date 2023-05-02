### The script has four sections: PACKAGES, FILE DOWNLOAD, DATA PREPARATION
### and ANALYSES; they should be carried out in sequence
# the script was created under R version 4.2.1
# FILE DOWNLOAD and DATA PREPARATION can be skipped if the lines 357 and 364-365 just before
# ANALYSES loading the pho2, pho3, and wld objects are run since the output of those two
# sections are in those objects, which have been saved and supplied for convenience

### PACKAGES
# The three following packages are used: lme4 (version 1.1.30),
# rworldmap (1.3.6), colorspace (version 2.0.3), vioplot (0.4.0)
# ggplot2 (3.4.1), dplyr (1.1.1), tibble (3.2.1)
# they will get installed if they aren't already running the following
packages <- c("lme4", "rworldmap", "colorspace", "vioplot", "ggplot2", "
              dplyr", "tibble")
for (i in seq_along(packages)) {
  if ( (packages[i] %in% installed.packages())==FALSE ) {
    install.packages(packages[i], repos="http://cran.us.r-project.org",
                     verbose=FALSE, quiet=TRUE)
  }
}

# If you want to know if you have the same version as originally used you can
# check which package version you have, writing, e.g., packageVersion("lme4")
# If you want to install the same version of a package as originally used
# the install_version() function of the remotes package is helpful (see
# https://search.r-project.org/CRAN/refmans/remotes/html/install_version.html),
# but it is actually recommend to just use the most recent version of any package (and of R)
# since back compatibility is usually strongly supported.

### FILE DOWNLOAD
# At this point you should set the working directory to a folder where you
# want to keep all files relating to this study.
# You can verify what the working directory is running getwd() and
# you can change that using setwd("C:/myfolder/mysubfolder/.../..."))
# Now you are ready to download the three datasets used in the study,
# namely Phoible, ASJP word length data, and WALS.
# This will be done for you by the script.
options(timeout=180)

# Phoible data is in cldf-datasets/phoible-v2.0.1.zip downloaded from https://zenodo.org/record/2677911
# (doi: 10.5281/zenodo.2677911, version used: 2.0.1)
download.file("https://zenodo.org/record/2677911/files/cldf-datasets/phoible-v2.0.1.zip", "phoible-v2.0.1.zip")

# ASJP word length data is in the file Data-01 ASJP data raw.txt downloaded from https://zenodo.org/record/6344024
# (doi: 10.5281/zenodo.6344024, version used: v1)
download.file("https://zenodo.org/record/6344024/files/Data-01%20ASJP%20data%20raw.txt", "Data-01 ASJP data raw.txt")

# WALS data is in wals-v2020.3.zip downloaded from https://zenodo.org/record/7385533
# (doi: 10.5281/zenodo.7385533, version used: v2020.3)
download.file("https://zenodo.org/record/7385533/files/cldf-datasets/wals-v2020.3.zip", "wals-v2020.3.zip")

# Data from EURPhon have been downloaded from https://eurphon.info/downloads and are in
# languages.tsv and tones.tsv supplies here.
# The file languages.tsv has extraneous newlines that need to be removed by
# hand; they were removed in the supplied file

### DATA PREPARATION
# Prepare the Phoible data:
# the following code will unzip the relevant files for you,
# rename them to avoid same file names from different datasets,
# and will delete the zip file
unzip("phoible-v2.0.1.zip",
  files=c("cldf-datasets-phoible-f36deac/cldf/contributions.csv",
  "cldf-datasets-phoible-f36deac/cldf/languages.csv",
  "cldf-datasets-phoible-f36deac/cldf/values.csv"),
  junkpaths=TRUE)
file.rename("contributions.csv", "contributions_phoible.csv")
file.rename("languages.csv", "languages_phoible.csv")
file.rename("values.csv", "values_phoible.csv")
file.remove("phoible-v2.0.1.zip")

# the following operations unite the essential data from Phoible in a
# data frame called pho
contributions <- read.csv(file="contributions_phoible.csv")  # basic data on tones
languages <- read.csv(file="languages_phoible.csv")  # metadata
values <- read.csv(file="values_phoible.csv")  # metadata
m <- match(unique(values$Contribution_ID),values$Contribution_ID)
meta <- data.frame(values$Contribution_ID[m], values$Language_ID[m])
names(meta) <- c("ID", "Language_ID")
m <- match(meta$Language_ID, languages$ID)
iso_code <- languages$ISO639P3code[m]
meta <- cbind(meta, iso_code)
pho <- contributions[,c("ID", "Name", "Contributor_ID", "count_phonemes",
                        "count_consonants", "count_vowels", "count_tones")]
m <- match(pho$ID, meta$ID)
iso_code <- meta$iso_code[m]
pho <- cbind(pho, iso_code)
names(pho)[1] <- "inventory_id"
# remove cases where data is from UPSID or SAPHON
# since there is not information on tones
# remove cases where data from PH indicates
# lack of tone since at least some of these are unreliable
out1 <- which(pho$Contributor_ID %in% c("UPSID", "SAPHON"))
out2 <- which(pho$Contributor_ID=="PH" & pho$count_tones==0)
out <- union(out1, out2)
if (length(out) > 0) {
  pho <- pho[-out,]
}
# add data from eurphon
el <- read.table(file="eurphon/languages.tsv", header=TRUE, # eurphon.languages
                                sep="\t", quote="", na.strings="",
                                comment.char="")
et <- read.table(file="eurphon/tones.tsv", header=TRUE,  # eurphon.tones
                                sep="\t", quote="", na.strings="",
                                comment.char="")
el.data <- matrix(NA, nrow=length((el$id)), ncol=ncol(pho))
colnames(el.data) <- names(pho)
el.data <- as.data.frame(el.data)
el.data$inventory_id <- el$id
el.data$Name <- el$name
el.data$Contributor_ID[1:nrow(el.data)] <- rep("EA", nrow(el.data))
el.data$count_tones[1:nrow(el.data)] <- rep(0, nrow(el.data))
tone.table <- table(et$language_id)
w_tonal <- match(as.character(names(tone.table)),
                       as.character(el.data$inventory_id))
number_tones <- as.vector(tone.table)
el.data$count_tones[w_tonal] <- number_tones
el.data$iso_code <- el$iso_code
pho <- rbind(pho, el.data)

# Prepare the ASJP word length data:
wld <- read.table(file="Data-01 ASJP data raw.txt", header=TRUE, sep="\t", quote="")
# get the amount of languages for which there is word length data
length(wld$forty_mean) # 5289

# prune the data
# take out languages in the "other" category in the ASJP/WALS classification
w_other <- which(wld$wals_fam=="Oth")
if ( length(w_other) > 0 ) {
	wld <- wld[-which(wld$wals_fam=="Oth"),]
}
# take out those for which less than 20 of the 40 items are attested
w_few <- which(wld$perc_att_40 < 50)
if ( length(w_few) > 0 ) {
	wld <- wld[-which(wld$perc_att_40 < 50),]
}
save(wld, file="wld.RData")

# exclude cases of missing iso-codes from PHOIBLE
w_na <- which(is.na(pho$iso_code))
if ( length(w_na) > 0 ) {
	pho <- pho[-w_na,]
}

# rework the PHOIBLE data to produce averages over ISO-codes
isos_pho <- unique(pho$iso_code)
for (i in 1:length(isos_pho)) {
	w_iso <- which(pho$iso_code==isos_pho[i])
	merge_set <- pho[w_iso,]
	if ( length(w_iso) > 1 ) {
		ID <- "multiple"
		Contributor_ID <- "multiple"
	} else {
		ID <- pho$inventory_id[w_iso]
		Contributor_ID <- pho$Contributor_ID[w_iso]
	}
	add <- c(ID,
	         merge_set[1,"Name"],
	         Contributor_ID,
	         round(mean(merge_set[,"count_phonemes"], na.rm=TRUE),2),
	         round(mean(merge_set[,"count_consonants"], na.rm=TRUE),2),
	         round(mean(merge_set[,"count_vowels"], na.rm=TRUE),2),
	         round(mean(merge_set[,"count_tones"], na.rm=TRUE),2),
	         merge_set[1,"iso_code"])
	add <- t(data.frame(add))
	add <- as.data.frame(add)
	add[4:7] <- as.numeric(add[4:7])
	names(add) <- names(pho)
	pho <- pho[-w_iso,]
	pho <- rbind(pho, add)
}

# add a column to the PHOIBLE data with glot_fam, pop, forty_mean, continent, names
glot_fam <- c()
pop <- c()
forty_mean <- c()
continent <- c()
names <- c()
for (i in 1:nrow(pho)) {
	w_asjp <- which(wld$iso==pho$iso_code[i])
	if ( length(w_asjp)==0 ) {
		glot_fam[i] <- NA
		pop[i] <- NA
		forty_mean[i] <- NA
		continent[i] <- NA
		names[i] <- NA
	} else {
		glot_fam[i] <- wld$glot_fam[w_asjp]
		pop[i] <- wld$pop[w_asjp]
		forty_mean[i] <- wld$forty_mean[w_asjp]
		continent[i] <- wld$continent[w_asjp]
		names[i] <- wld$names[w_asjp]
	}
}
pho <- cbind(pho, data.frame(glot_fam, pop, forty_mean, continent, names))

# get rid of languages classified as a Glottolog "family" MixedLanguage
# in case there are any (shouldn't be the case)
w_ml <- which(pho$glot_fam=="MixedLanguage")
if ( length(w_ml) > 0 ) {
	pho <- pho[-w_ml,]
}

# for families extending over more than one continent
# assign the family to its likely continent of origin
for (i in 1:nrow(pho)) {
	if (!is.na(pho$glot_fam[i])) {
		if (pho$glot_fam[i]=="Afro-Asiatic") {pho$continent[i] <- "Africa"}
		if (pho$glot_fam[i]=="Algic") {pho$continent[i] <- "W N America"}
		if (pho$glot_fam[i]=="Arawakan") {pho$continent[i] <- "S America"}
		if (pho$glot_fam[i]=="Athabaskan-Eyak-Tlingit") {pho$continent[i] <- "W N America"}
		if (pho$glot_fam[i]=="Austronesian") {pho$continent[i] <- "S/SE Asia"}
		if (pho$glot_fam[i]=="Chibchan") {pho$continent[i] <- "C America"}
		if (pho$glot_fam[i]=="Cochimi-Yuman") {pho$continent[i] <- "W N America"}
		if (pho$glot_fam[i]=="Dravidian") {pho$continent[i] <- "S/SE Asia"}
		if (pho$glot_fam[i]=="Eskimo-Aleut") {pho$continent[i] <- "W N America"}
		if (pho$glot_fam[i]=="Indo-European") {pho$continent[i] <- "W and SW Eurasia"}
		if (pho$glot_fam[i]=="Kiowa-Tanoan") {pho$continent[i] <- "E N America"}
		if (pho$glot_fam[i]=="Mongolic") {pho$continent[i] <- "N-C Asia"}
		if (pho$glot_fam[i]=="Sino-Tibetan") {pho$continent[i] <- "S/SE Asia"}
		if (pho$glot_fam[i]=="Turkic") {pho$continent[i] <- "N-C Asia"}
		if (pho$glot_fam[i]=="Uralic") {pho$continent[i] <- "N-C Asia"}
		if (pho$glot_fam[i]=="Uto-Aztecan") {pho$continent[i] <- "W N America"}
	}
}

# get rid of languages for which data on word length is not available
w_na <- which(is.na(pho$forty_mean))
if ( length(w_na) > 0 ) {
	pho <- pho[-w_na,]
}

# make a column for presence vs. absence of tones
p_a <- pho$count_tones

# assign a 1 whenever tones are present
for (i in 1:length(pho$count_tones)) {
	if (!is.na(pho$count_tones[i]) ) {
		if ( pho$count_tones[i] > 0 ) {
			p_a[i] <- 1
		}
	}
}
pho <- cbind(pho, p_a)
source <- rep("phoible", nrow(pho))
pho <- cbind(pho, source)

# from here on data are added from WALS
# and new objects, pho2 and the more lightweight pho3, are created
# these will be used in all analyses

# Prepare the WALS data:
# the following code will unzip the relevant files for you,
# rename to avoid same file names from different datasets,
# and will delete the zip file
unzip("wals-v2020.3.zip",
  files=c("cldf-datasets-wals-878ea47/cldf/languages.csv",
  "cldf-datasets-wals-878ea47/cldf/values.csv"),
  junkpaths=TRUE)
file.rename("languages.csv", "languages_wals.csv")
file.rename("values.csv", "values_wals.csv")
file.remove("wals-v2020.3.zip")

# now read the WALS data and extract data on tones
lgs <- read.csv(file="languages_wals.csv")
vls <- read.csv(file="values_wals.csv")
tone_data <- vls[which(vls$Parameter_ID=="13A"),c("Language_ID","Value")]
iso_code <- lgs$ISO639P3code[match(tone_data$Language_ID, lgs$ID)]
tone_data <- cbind(tone_data, iso_code)
wals_p_a <- tone_data$Value
wals_p_a[wals_p_a==1] <- 0
wals_p_a[wals_p_a > 1] <- 1
tone_data <- cbind(tone_data, wals_p_a)
names(tone_data) <- c("wals_code", "wals_value", "iso_code", "wals_p_a")
source <- rep("wals", nrow(tone_data))
tone_data <- cbind(tone_data, source)
wals_fix <- c("aze", "cax", "oji", "mla", "tuk", "wah", "ktk")
iso_fix <- c("azb", "cni", "ojg", "mcu","bhq", "wgi", "aal")
for (i in 1:length(tone_data$wals_code)) {
	if ( tone_data$wals_code[i] %in% wals_fix ) {
		m <- match(tone_data$wals_code[i], wals_fix)
		tone_data$iso_code[i] <- iso_fix[m]
	}
}

pho2 <- merge(pho, tone_data, by = "iso_code", all=TRUE)

for (i in 1:nrow(pho2)) {
	if ( is.na(pho2$p_a[i]) ) {
		pho2$p_a[i] <- pho2$wals_p_a[i]
		pho2$source.x[i] <- "wals"
	}
}

for (i in 1:nrow(pho2)) {
	if ( is.na(pho2$count_phonemes[i]) ) {
		w_iso <- which(wld$iso==pho2$iso_code[i])
		if ( length(w_iso) > 0 ) {
			pho2$names[i] <- wld$names[w_iso]
			pho2$glot_fam[i] <- wld$glot_fam[w_iso]
			pho2$pop[i] <- wld$pop[w_iso]
			pho2$forty_mean[i] <- wld$forty_mean[w_iso]
			pho2$continent[i] <- wld$continent[w_iso]
		}
	}
}

delete <- match(c("wdo","mxm"), pho2$wals_code)
if ( length(delete) > 0 ) {
	pho2 <- pho2[-delete,]
}
round_count_tones <- round(pho2$count_tone)
pho2 <- cbind(pho2, round_count_tones)

# change names of columns called "Name" and "source.x"
column_Name <- which(names(pho2)=="Name")
column_source.x <- which(names(pho2)=="source.x")
names(pho2)[c(column_Name,column_source.x)] <- c("name", "source")

# delete the column called "source.y"
column_source.y <- which(names(pho2)=="source.y")
pho2 <- pho2[,-column_source.y]

# add coordinates
m <- match(pho2$iso_code, wld$iso)
lat <- wld$lat[m]
lon <- wld$lon[m]
pho2 <- cbind(pho2, lat, lon)

# change count_tone to 0 when it is NA and WALS
# supplies the information that it is absent
w_add_zero <- which(pho2$p_a==0 & is.na(pho2$count_tones))
pho2$count_tones[w_add_zero] <- 0
# in case you want to inspect the data in a file
# the following line can be run:
# write.table(pho2, file="pho.txt", sep="\t", quote=FALSE, row.names=FALSE)

# count how many languages at disposal for count of tones and word length
lgs_count <- length(which(!is.na(pho2$forty_mean) & !is.na(pho2$count_tones)))
lgs_count  # 1380
# in case you want to inspect the data in a file
# the following line can be run:
# write.table(pho2[which(!is.na(pho2$forty_mean) & !is.na(pho2$count_tones)),],
#   file="pho_tones_count_wl.txt", sep="\t", quote=FALSE, , row.names=FALSE)

# count how many languages at disposal for presence/absence and word length
lgs_p_a <- length(which(!is.na(pho2$forty_mean) & !is.na(pho2$p_a)))
lgs_p_a  # 1488
# count how many languages with presence/absence counts were contributed by WALS
length(which(pho2$source=="wals"))  # 257
# in case you want to inspect the data in a file
# the following line can be run:
# write.table(pho2[which(!is.na(pho2$forty_mean) & !is.na(pho2$p_a)),],
#   file="pho_tones_pa_wl.txt", sep="\t", quote=FALSE, , row.names=FALSE)

save(pho2, file="pho2.RData")
load("pho2.RData")
# reduce to the columns needed
pho2b <- pho2[,c("iso_code", "glot_fam", "continent", "lat", "lon", "count_tones", "forty_mean", "p_a")]
no.nas <- apply(pho2b, 1, function(x) sum(is.na(x)) == 0)
# retain only those
pho3 <- pho2b[no.nas,]
save(pho3, file="pho3.RData")
load("pho3.RData")
load("wld.RData")

### ANALYSES
# FIGURE 1
# boxplots for MWL, one per number of tones
# library(vioplot)
MWL_0 <- as.vector(na.omit(pho2$forty_mean[pho2$round_count_tones==0]))
MWL_1 <- as.vector(na.omit(pho2$forty_mean[pho2$round_count_tones==1]))
MWL_2 <- as.vector(na.omit(pho2$forty_mean[pho2$round_count_tones==2]))
MWL_3 <- as.vector(na.omit(pho2$forty_mean[pho2$round_count_tones==3]))
MWL_4 <- as.vector(na.omit(pho2$forty_mean[pho2$round_count_tones==4]))
MWL_5 <- as.vector(na.omit(pho2$forty_mean[pho2$round_count_tones==5]))
MWL_6 <- as.vector(na.omit(pho2$forty_mean[pho2$round_count_tones==6]))
MWL_7 <- as.vector(na.omit(pho2$forty_mean[pho2$round_count_tones==7]))
MWL_8 <- as.vector(na.omit(pho2$forty_mean[pho2$round_count_tones==8]))
MWL_9 <- as.vector(na.omit(pho2$forty_mean[pho2$round_count_tones==9]))
MWL_10 <- as.vector(na.omit(pho2$forty_mean[pho2$round_count_tones==10]))
N <- sapply(list(MWL_0, MWL_1, MWL_2, MWL_3, MWL_4, MWL_5, MWL_6, MWL_7, MWL_8, MWL_9, MWL_10), length)
# vioplot(MWL_0, MWL_1, MWL_2, MWL_3, MWL_4, MWL_5, MWL_6, MWL_7, MWL_8, MWL_9, MWL_10,
#         ylim = c(2,7), xlab="Number of tonal distinctions", ylab="Mean word length", xaxt="n")
boxplot(MWL_0, MWL_1, MWL_2, MWL_3, MWL_4, MWL_5, MWL_6, MWL_7, MWL_8, MWL_9, MWL_10,
        ylim = c(2,7), xlab="Number of tonal distinctions", ylab="Mean word length", xaxt="n")
xtick <- c(1:11)
text(x=xtick, par("usr")[3], labels = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), pos = 1, xpd = TRUE)
text(x=xtick, par("usr")[3], labels = paste("N=", N, sep=""), pos = 3, xpd = TRUE, cex = .8)
means <- sapply(list(MWL_0, MWL_1, MWL_2, MWL_3, MWL_4, MWL_5, MWL_6, MWL_7, MWL_8, MWL_9, MWL_10), mean)
points(1:11, means, pch=15, cex=1.5)
abline(lm(pho2$forty_mean ~  pho2$count_tones), lwd=2, lty="dashed")
summary(lm(pho2$forty_mean ~  pho2$count_tones))

# linear mixed effects model
library(lme4)
full_model <- lmer(count_tones ~ forty_mean + (1|continent) + (1|glot_fam), data=pho2, REML = FALSE)
summary(full_model)
reduced_model <- lmer(count_tones ~ 1 + (1|continent) + (1|glot_fam), data=pho2, REML = FALSE)
anova(reduced_model, full_model)

# FIGURE 2
library(ggplot2)
library(dplyr)
library(tibble)
fam_count <- pho3 %>% group_by(glot_fam) %>% summarise(Count = n()) %>% arrange(desc(Count))
n_top <- 26
top_families <- fam_count$glot_fam[1:n_top]
labels <- character(n_top)
names(labels) <- top_families
indiv_models <- data.frame(glot_fam = rep(NA, n_top), intercepts = rep(NA, n_top), slopes = rep(NA, n_top))
for (i in 1:n_top) {
  fam_name <- fam_count$glot_fam[i]
  fam_name <- gsub("_", " ", fam_name)
  if (fam_name == "Nuclear Trans New Guinea") {
    fam_name <- "Nucl Trans N Guinea"
  }
  labels[i] <- paste(fam_name, "\n n = ", fam_count$Count[i], sep = "")
  fam_data <- pho3[pho3$glot_fam==fam_count$glot_fam[i],]
  indiv_model <- lm(count_tones ~ forty_mean, data=fam_data)
  indiv_models$glot_fam[i] <- fam_count$glot_fam[i]
  indiv_models$intercepts[i] <- indiv_model$coefficients[1]
  indiv_models$slopes[i] <- indiv_model$coefficients[2]
}
coefs <- coef(full_model)$glot_fam %>%
  rename(Intercept = `(Intercept)`, Slope = forty_mean) %>%
  rownames_to_column("glot_fam") %>%
  filter(glot_fam %in% top_families)
pho3_coefs <- left_join(pho3, coefs, by = "glot_fam")
pho3_coefs <- left_join(pho3_coefs, indiv_models, by = "glot_fam")
pho3_coefs <- filter(pho3_coefs, glot_fam %in% top_families)
pho3_coefs$glot_fam <- factor(pho3_coefs$glot_fam, levels = top_families)
coef_plot <- ggplot(data = pho3_coefs, aes(forty_mean, count_tones)) +
  geom_point(na.rm = T, alpha = 0.3) +
  geom_abline(aes(intercept = Intercept, slope = Slope), linewidth = 0.7) +
  geom_abline(aes(intercept = intercepts, slope = slopes), linewidth = 0.7, color="red") +
  facet_wrap( ~ glot_fam, labeller = labeller(glot_fam = labels)) +
  xlab("Mean word length") + ylab("Number of tonal distinctions") +
  theme_bw()
coef_plot


# FIGURE 3
cont_count <- pho3 %>% group_by(continent) %>% summarise(Count = n()) %>% arrange(desc(Count))
n_top <- 10
top_continents <- cont_count$continent[1:n_top]
labels <- character(n_top)
names(labels) <- top_continents
indiv_models <- data.frame(continent = rep(NA, n_top), intercepts = rep(NA, n_top), slopes = rep(NA, n_top))
for (i in 1:n_top) {
  labels[i] <- paste(cont_count$continent[i], "\n n = ", cont_count$Count[i], sep = "")
  cont_data <- pho3[pho3$continent==cont_count$continent[i],]
  indiv_model <- lm(count_tones ~ forty_mean, data=cont_data)
  indiv_models$continent[i] <- cont_count$continent[i]
  indiv_models$intercepts[i] <- indiv_model$coefficients[1]
  indiv_models$slopes[i] <- indiv_model$coefficients[2]
}
coefs <- coef(full_model)$continent %>%
  rename(Intercept = `(Intercept)`, Slope = forty_mean) %>%
  rownames_to_column("continent")
pho3_coefs <- left_join(pho3, coefs, by = "continent")
pho3_coefs <- left_join(pho3_coefs, indiv_models, by = "continent")
pho3_coefs <- filter(pho3_coefs, continent %in% top_continents)
pho3_coefs$continent <- factor(pho3_coefs$continent, levels = top_continents)
coef_plot <- ggplot(data = pho3_coefs, aes(forty_mean, count_tones)) +
  geom_point(na.rm = T, alpha = 0.3) +
  geom_abline(aes(intercept = Intercept, slope = Slope), linewidth = 0.7) +
  geom_abline(aes(intercept = intercepts, slope = slopes), linewidth = 0.7, color="red") +
  facet_wrap( ~ continent, labeller = labeller(continent = labels)) +
  xlab("Mean word length") + ylab("Number of tonal distinctions") +
  theme_bw()
coef_plot

# FIGURE 4
# logistic regression to investigate presence of tone
# as a function of word length
library(lme4)
binary_model <- glmer(p_a ~ forty_mean + (1|continent) + (1|glot_fam), data=pho2, family = binomial)
summary(binary_model)
reduced_binary_model <- glmer(p_a ~ 1 + (1|continent) + (1|glot_fam), data=pho2, family = binomial)
anova(binary_model, reduced_binary_model)
intercept <- 3.0598642
slope <- -1.1157857
mwl_vals <- seq(2.3, 6.8, 0.1)
y_preds <- plogis(intercept + slope * mwl_vals)
df <- data.frame(mwl_vals,y_preds)
plot(df, type="n", xlab = "Mean word length", ylab = "Probability of having tone / Density mean world length", xlim=c(2,7), ylim=c(0,0.6))
lines(df, lwd=2)
lines(density(wld$forty_mean), lty="dotted", lwd=2, xlim=c(2,7), ylim=c(0,0.6))
# abline(v = mean(wld$forty_mean), lty="dotted")
abline(v = mean(wld$forty_mean), col="red", lwd=2)
grid()
# get summary data on mean word length
summary(wld$forty_mean)

# FIGURE 5
# map showing presence of tone associated with small
# vs. large values of  MWL and also absence of tone
library(rworldmap)
library(colorspace)
# gather the relevant data
mi <- data.frame(pho2$p_a, pho2$forty_mean, pho2$lat, pho2$lon)  # stands for map input
names(mi) <- c("p_a", "mwl", "lat", "lon")
# get rid of languages for which coordinates are lacking
nas <- which(is.na(mi$lat) | is.na(mi$lon))
if ( length(nas) > 0 ) {
	mi <- mi[-nas,]
}
# divide the languages up in those having short (first quartile),
# intermediate (second and third quartile) and long words (fourth quartile)
breakingpoints <- c(0,summary(mi$mwl)[2],summary(mi$mwl)[5],10)
cats <- as.numeric(cut(mi$mwl, breaks=breakingpoints))
mi <- cbind(cats, mi)
# remove languages not having tones
mi_tone <- mi[-which(mi$p_a==0),]

map <- getMap()
plot(map)
palette("default")
points(cbind(mi_tone$lon, mi_tone$lat), pch=20,
   col=(mi_tone$cats * 3 + 1), cex=2)
palette("default")
