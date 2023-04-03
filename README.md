# Tone-WordLength
Scripts for preparing data and replicating analyses for the paper "Tones and word length across languages".

tones.R 
contains code for main analyses and figures. Instructions for accessing data to be combined for the purposes of the analyses are in the first part of the script. It produces pho2.RData and pho3.RData, which are also supplied for convenience. To produce these, languages.tsv and tones.tsv (from EURPhon) are needed. In case someone should want to carry out their own analyses not using the script, the following two files are offered: pho_tones_count_wl.txt and pho_tones_pa_wl. The first has the essential data on word length and the number of tones and is a subset of the latter, which also includes additional data for some languages where only data on presence and absence of tone (not counts) were available. The files are generated within the script and are not used as physical files, but are offered here for convenience.

analyze_TeDDi.R 
extracts word length data for Bible and Universal Declaration of Human Rights texts from TeDDi (Moran et al. 2022. TeDDi sample: Text Data Diversity sample for language comparison and multilingual NLP. LREC 2022, 1150–1158) and correlates it with word length data from ASJP. The script stores the word length data from TeDDi in two R objects, wl_bibles.RData and wl_udhrs.RData, which are supplied here for convenience in case it is desirable to avoid having to download and process the large (773 MB) TeDDi_v0_1.RData file.

analyze_NorthEuraLex.R 
extracts word length data from NorthEuraLex (Dellert et al. 2020. NorthEuraLex: a wide-coverage lexical database of Northern Eurasia. Lang. Resources & Evaluation 54, 273–301). It correlates that with ASJP data, and also performs correlations of ASJP 40- and 100-item lists.

phylogenetic_correlation.R 
performs phylogenetic correlation for families with 6 or more members and at least one tonal language. It requires matrices with lexical distances, supplied here as: Afro-Asiatic_LDN.txt, Athabaskan-Eyak-Tlingit_LDN, Atlantic-Congo_LDN, Austroasiatic_LDN, Austronesian_LDN, Central_Sudanic_LDN, Indo-European_LDN, Kadugli-Krongo_LDN, Mande_LDN, Nilotic_LDN, Nuclear_Trans_New_Guinea_LDN, Otomanguean_LDN, Salishan_LDN, Sino-Tibetan_LDN, Tai-Kadai_LDN, and Ta-Ne-Omotic_LDN. The script explains how the creation of these files can be replicated. From the matrices branch lengths are supplied to Glottolog trees. Other input is data on tones and word length for each of 16 families. This is prepared by the script which then calls BayesTraitsV4.exe, which should be downloaded from http://www.evolution.reading.ac.uk/BayesTraitsV4.0.1/BayesTraitsV4.0.1.html. The direct input to and output from Bayestraits is generated by the script and BayesTraits itself, but is supplied here in two zipped files for documentation: BayesTraitsInput.zip and BayesTraitsOutput.zip.
