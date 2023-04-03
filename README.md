# Tone-WordLength
Scripts for preparing data and replicating analyses and replicating analyses for the paper "Tones and word length across languages".

Tones.R contains code for main analyses and figures. Instructions for accessing data to be combined for the purposes of the analyses are in the first part of the script. It produces pho2.RData and pho3.RData, which are also supplied for convenience. To produce these, languages.tsv and tones.tsv (from EURPhon) are needed. In case someone should want to carry out their own analyses not using the script, the following two files are offered: pho_tones_count_wl.txt and pho_tones_pa_wl. The first has the essential data on word length and the number of tones and is a subset of the latter, which also includes additional data for some languages where only data on presence and absence of tone (not counts) were available. The files are generated within the script and are not used as physical files, but are also offered here for convenience.

