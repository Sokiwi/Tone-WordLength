# You can skip to lines 124-125 and load the supplied RData files
# in case it is desirable to avoid downloading and 
# processing the large (773 MB) TeDDi_v0_1.RData file

# download TeDDi_v0_1.RData from 
# https://drive.switch.ch/index.php/s/MJv7xFkzqlzFn0y
# set the working directory to where you store the file
load("TeDDi_v0_1.RData")  # may take 2.5 minutes

# fix a couple of typos in TeDDi
# error in a language name:
where_error_1 <- which(clc_file$language_name_wals=="Guraní")
if ( length(where_error_1) > 0 ) {
	clc_file$language_name_wals[where_error_1] <- "Guaraní"
}
# error in a WALS code:
where_error_2 <- which(clc_language$name_wals=="Canela-Krahô")
clc_language$wals_code[where_error_2] <- "cnl"
# error in a language name:
where_error_3 <- which(clc_file$language_name_wals=="Otomi (Mezquital)")
if ( length(where_error_3) > 0 ) {
	clc_file$language_name_wals[where_error_3] <- "Otomí (Mezquital)"
}

# get metadata for Universal Declaration of Human Rights texts
w_udhr <- grep("Universal Declaration of Human Rights", clc_file$short_description)
# the following shows that for Greek (Modern), Hausa, Mandarin, and Vietnamese there are two versions
clc_file$language_name_wals[w_udhr]
# inspection shows that for Greek one text (a) is simplified w.r.t diacritics while (b) one has more, 
# for Hausa one (a) is for Niger, one for Nigeria (b), for Mandarin, one (a) is simplified and (b) one 
# in traditional characters, for Vietnamese one (a) is plain text and one (b) in Chinese characters
# I choose the ones that I labelled (a) for the purpose of this orientation
udhr_unselect1 <- match(c(9430, 9437, 15102, 23299), w_udhr)
udhr_select <- w_udhr[-udhr_unselect1]
# check which texts are in Burmese, Japanese, Thai or Chinese writing and exclude
udhr_select_writing_system <- clc_file$writing_system[udhr_select]
udhr_unselect2 <- which(udhr_select_writing_system %in% c("Jpan","Mymr", "Hans", "Thai"))
udhr_select <- udhr_select[-udhr_unselect2]
# final list of languages and writing systems in udhr selection
udhr_select_language <- clc_file$language_name_wals[udhr_select]
udhr_select_writing_system <- clc_file$writing_system[udhr_select]
# corresponding ISO 639-3 codes in udhr selection
udhr_select_iso_code <- clc_language$iso639_3[match(clc_file$language_name_wals[udhr_select], clc_language$name_wals)]

# get metadata for Bible texts
w_bible <- union(grep("Bible", clc_file$short_description), grep("New Testament", clc_file$short_description))
# the following shows that for Greek (Modern) and Hindi there are two versions
clc_file$language_name_wals[w_bible]
# inspection show that one Greek text is from Mayer/Cysouw, the other from Gutenberg. The first is chosen
# for possibly better consistency with others also from Mayer/Cysouw; one Hindi text is the New
# Testament, one is the Bible, here the Bible is chosen
bible_unselect1 <- match(c(7893, 11085), w_bible)
bible_select <- w_bible[-bible_unselect1]  # 51 languages
# check which texts are in Burmese, Japanese, Thai or Chinese writing and exclude
bible_select_writing_system <- clc_file$writing_system[bible_select]
bible_unselect2 <- which(bible_select_writing_system %in% c("Jpan","Mymr", "Hans", "Thai"))
bible_select <- bible_select[-bible_unselect2]
# final list of languages and writing systems in Bible selection
bible_select_language <- clc_file$language_name_wals[bible_select]
bible_select_writing_system <- clc_file$writing_system[bible_select]
# corresponding ISO 639-3 codes in Bible selection
bible_select_iso_code <- clc_language$iso639_3[match(clc_file$language_name_wals[bible_select], clc_language$name_wals)]

# check which languages are represented for the two types of text
intersect(udhr_select_language, bible_select_language)  # languages for which both types are available
setdiff(udhr_select_language, bible_select_language)  # languages for which only udhr is available
setdiff(bible_select_language, udhr_select_language)  # languages for which only the Bible is available

# function for cleaning a line for punctuation and extra spaces
cleanline <- function(txtline) {
	a1 <- gsub("[[:punct:]]", "", txtline)
	a2 <- gsub("\\s+", " ", a1)
	a3 <- trimws(a2)
	return(a3)
}

# function for outputting a vector of word lengths given a line of text
wl <- function(txtline_clean) {
	words <- unlist(strsplit(txtline_clean, " "))
	word_lengths <- unlist(lapply(words, nchar))
}

# this block of code can be run once only
# since the output is saved
# go through Bible texts and output word counts
wl_bibles <- c()
for (i in 1:length(bible_select)) {
	wl_count <- c()
	w_txt <- which(clc_line$file_id==bible_select[i])
	for (j in 1:length(w_txt)) {
		if ( !is.na(clc_line$text[w_txt[j]]) ) {
			wl_out <- wl(clc_line$text[w_txt[j]])
			wl_count <- c(wl_count, wl_out)
		} else {
			wl_count <- c(wl_count, NA)
		}
	}
	wl_bibles[i] <- round(mean(wl_count, na.rm=TRUE), 3)
}
save(wl_bibles, file="wl_bibles.RData")


# this block of code can be run once only
# since the output is saved
# go through udhr texts and output word counts
wl_udhrs <- c()
for (i in 1:length(udhr_select)) {
	wl_count <- c()
	w_txt <- which(clc_line$file_id==udhr_select[i])
	for (j in 1:length(w_txt)) {
		if ( !is.na(clc_line$text[w_txt[j]]) ) {
			wl_out <- wl(clc_line$text[w_txt[j]])
			wl_count <- c(wl_count, wl_out)
		} else {
			wl_count <- c(wl_count, NA)
		}
	}
	wl_udhrs[i] <- round(mean(wl_count, na.rm=TRUE), 3)
}
save(wl_udhrs, file="wl_udhrs.RData")

# in case you already ran the two previous blocks of code
# you can now just load the objects that were created
load("wl_bibles.RData")
load("wl_udhrs.RData")

# read in word length data from online SI for Wichmann and Holman (2023) at Zenodo
# download Data-01 ASJP data raw.txt from https://doi.org/10.5281/zenodo.6344023
# store it in the working directory and now read it
asjp_wl_data <- read.table(file="Data-01 ASJP data raw.txt", sep="\t", header=TRUE, na.strings="", quote="")
# via ISO-codes for the languages in the udhr selection get ASJP word length
w_udhr_select_iso_code <- match(udhr_select_iso_code, asjp_wl_data$iso)
asjp_wl_udhr_select <- asjp_wl_data$forty_mean[w_udhr_select_iso_code]
# via ISO-codes for the languages in the Bible selection get ASJP word length
w_bible_select_iso_code <- match(bible_select_iso_code, asjp_wl_data$iso)
asjp_wl_bible_select <- asjp_wl_data$forty_mean[w_bible_select_iso_code]

# make data frames for comparing the corpus word length counts with counts from ASJP
udhr_comp <- data.frame(udhr_select_iso_code, udhr_select_language, wl_udhrs, asjp_wl_udhr_select)
names(udhr_comp) <- c("iso", "lg", "wl_txt", "wl_asjp")
bible_comp <- data.frame(bible_select_iso_code, bible_select_language, wl_bibles, asjp_wl_bible_select)
names(bible_comp) <- c("iso", "lg", "wl_txt", "wl_asjp")

# inspect relationships between word length in texts and asjp visually (not used)
# plot(udhr_comp$wl_txt, udhr_comp$wl_asjp)
# abline(lm(udhr_comp$wl_asjp ~ udhr_comp$wl_txt))

plot(bible_comp$wl_txt, bible_comp$wl_asjp)
abline(lm(bible_comp$wl_asjp ~ bible_comp$wl_txt))

# do correlations between word length in texts and in ASJP
cor.test(udhr_comp$wl_txt, udhr_comp$wl_asjp) # r = 0.575, p < 0.001, n = 36
cor.test(bible_comp$wl_txt, bible_comp$wl_asjp) # r = 0.599, p < 0.001, n = 49

# do correlations between the two texts (not used)
# merger <- merge(bible_comp, udhr_comp, by.x="iso", by.y="iso")
# cor.test(merger$wl_txt.x, merger$wl_txt.y) # r = 0.951. p < 0.001, n = 28

