# Tone-WordLength
Scripts for preparing data and replicating analyses and replicating analyses for the paper "Tones and word length across languages".

The scfript tones.R contains code for main analyses and figures. Instructions for accessing data to be combined for the purposes of the analyses are in the first part of the script. It produces pho2.RData and pho3.RData, which are also supplied for convenience. To produce these, languages.tsv and tones.tsv (from EURPhon) are needed. In case someone should want to carry out their own analyses not using the script, the following two files are offered: pho_tones_count_wl.txt and pho_tones_pa_wl. The first has the essential data on word length and the number of tones and is a subset of the latter, which also includes additional data for some languages where only data on presence and absence of tone (not counts) were available. The files are generated within the script and are not used as physical files, but are also offered here for convenience.

The script analyze_TeDDi.R extracts word length data for Bible and Universal Declaration of Human Rights texts from TeDDi (Moran et al. 2022. TeDDi sample: Text Data Diversity sample for language comparison and multilingual NLP. LREC 2022, 1150–1158) and correlates it with word lenght data from ASJP. The script stores the word length data from TeDDi in two R objects, wl_bibles.RData and wl_udhrs.RData, which are supplied here for convenience in case it is desirable to avoid having to download and process the large (773 MB) TeDDi_v0_1.RData file.


