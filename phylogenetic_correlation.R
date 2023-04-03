## LIBRARIES
# libraries needed
library(ape)  # read.tree(), write.tree(), drop.tip(), write.nexus()
library(glottoTrees)  # keep_as_tip()
library(phangorn)  # nnls.tree
library(stringr)  # str_split, str_sub
library(ade4)  # mantel.rtest

## SOFTWARE
# Download BayesTraits 4.0.1 from 
# http://www.evolution.reading.ac.uk/BayesTraitsV4.0.1/BayesTraitsV4.0.1.html

## DATA PREPARATION PRIOR TO THIS SCRIPT
# Using the interactive ASJP software at https://github.com/Sokiwi/InteractiveASJP02
# produce LDN distance matrices for relevant families,
# i.e. any family with 6 or more members that has at least one tonal language

## DOWNLOADING FILES
# download the file tree_glottolog_newick.txt from https://glottolog.org/meta/downloads
# download asjp-v20.zip from https://zenodo.org/record/7079637 
# download.file("https://zenodo.org/record/7079637/files/lexibank/asjp-v20.zip?download=1", "asjp.zip")
# unzip("asjp-v20.zip", files="languages.csv", junkpaths=TRUE)
# download languages_and_dialects_geo.csv from https://glottolog.org/meta/downloads
# download languoid.csv from https://glottolog.org/meta/downloads
# the above download may not work, and will then have to be done manually

## READ FILES, PREPARE DATA
# Prepare Glottolog trees, reading tree_glottolog_newick.txt
trees <- suppressWarnings(readLines("tree_glottolog_newick.txt"))
# two apostrophes in names of taxa symbolize one apostrophe; these are 
# converted to "§" here
for (i in 1:length(trees)) {
  trees[i] <- gsub("\'\'\'", "'§", trees[i])
}
for (i in 1:length(trees)) {
  trees[i] <- gsub("\'\'", "§", trees[i])
}

# Prepare ASJP metadata
# revise asjp_meta such that a dialect or family glottocode match to an ASJP
# doculect is converted to a language-level languoid
# takes a little more than one minute
unify.level <- function(asjp_meta) {
  # having gotten the ASJP languages.csv file with metadata now read it
  asjp_meta <- read.csv("languages.csv")
  languoid <- read.csv("languoid.csv")
  for (i in 1:nrow(asjp_meta)) {
    gcode_old <- asjp_meta$Glottocode[i]
    if (gcode_old != "") {
      w_gcode <- grep(gcode_old, languoid$id)
      if(languoid$level[w_gcode]=="dialect") {
        parent1 <- languoid$parent_id[w_gcode]
        w_parent1 <- which(languoid$id==parent1)
        if(languoid$level[w_parent1]=="language") {
          asjp_meta$Glottocode[i] <- languoid$id[w_parent1]
        } else if(languoid$level[w_parent1]=="dialect") {
          parent2 <- languoid$parent_id[w_parent1]
          w_parent2 <- which(languoid$id==parent2)
          if(languoid$level[w_parent2]=="language") {
            asjp_meta$Glottocode[i] <- languoid$id[w_parent2]
          } else if(languoid$level[w_parent2]=="dialect") {
            parent3 <- languoid$parent_id[w_parent2]
            w_parent3 <- which(languoid$id==parent3)
            if(languoid$level[w_parent3]=="language") {
              asjp_meta$Glottocode[i] <- languoid$id[w_parent3]
            } else if (languoid$level[w_parent3]=="dialect") {
              parent4 <- languoid$parent_id[w_parent3]
              w_parent4 <- which(languoid$id==parent4)
              if(languoid$level[w_parent4]=="language") {
                asjp_meta$Glottocode[i] <- languoid$id[w_parent4]
              } else if (languoid$level[w_parent3]=="dialect") {
                cat("at", i, "there is more than 4 levels of dialect")
              }
            }
          }
        }
      } else if(languoid$level[w_gcode]=="family") {
        if(gcode_old=="alba1267") {
          # Standard Albanian fixed by hand
          asjp_meta$Glottocode[i] <- "tosk1239"
        } else {
          # other cases fixed arbitrarily as the first encountered 
          # daughter language
          asjp_meta$Glottocode[i] <- 
            languoid$id[which(languoid$parent_id==gcode_old 
                              & languoid$level=="language")][1]
        }
      }
    }
  }
  return(asjp_meta)
}
# asjp_meta <- unify.level(asjp_meta)
# save(asjp_meta, file="asjp_meta.RData")
load("asjp_meta.RData")

# Prepare data on tones and word length
# read in the pho2 object with basic data on tones and word length
# which was produced by tones.R
load("pho2.RData")
# add a column with glottocodes, using the column with ASJP names for
# identifying glottocodes in asjp_meta
# each of the names elements can have several ASJP names;
# the match between the first and a glottocode is picked
# and put in a separate column
glottocode <- c()
for (i in 1:nrow(pho2)) {
	ASJPnames <- pho2$names[i]
	if ( is.na(ASJPnames) ) {
		glottocode[i] <- NA
	} else {
		ASJPnames_v <- strsplit(ASJPnames, ",")[[1]]
		w_name <- which(asjp_meta$Name==ASJPnames_v[1])
		if ( length(w_name) > 0 ) {
			glottocode[i] <- asjp_meta$Glottocode[w_name]
		}
	}
}
pho2b <- cbind(pho2, glottocode)
# reduce it to the essential data, namely the columns count_tones, glot_fam, 
# forty_mean, and glottocode, and deleting rows where any of these are missing
# (NA or "")
pho3 <- pho2b[,c("count_tones", "glot_fam", "forty_mean", "glottocode")]
# find rows where no cells are NA
no.nas <- apply(pho3, 1, function(x) sum(is.na(x)) == 0) 
# retain only those
pho3 <- pho3[no.nas,]

# change family names with _ in them to a space since the family names 
# in the Glottolog trees have spaces in them
pho3$glot_fam <- unlist(lapply(pho3$glot_fam, function(x) gsub("_", " ", x)))

# Prepare index to Glottolog tree data
# (was initially also used as sanity check to see if there 
# is a tree to be found for each family in pho3)
tryCatch.W.E <- function(expr) {
	W <- NULL
	w.handler <- function(w){ # warning handler
		W <<- w
		invokeRestart("muffleWarning")
	}
	list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
		warning = w.handler),
		warning = W)
}
fam.names.trees <- c()
warn <- list()
for (i in 1:length(trees)) {
	warn[[i]] <- tryCatch.W.E(tree <- read.tree(text=trees[i]))
	# cat(i, warn[[i]]$value$node.label[1], "\n")
	fam.names.trees[i] <- warn[[i]]$value$node.label[1]
}
# warn
# Extract the family names from fam.names.trees
efn <- function(x) {
	a1 <- strsplit(x, "\\'")[[1]][2]
	a2 <- strsplit(a1, " \\[")[[1]][1]
}
tree.index <- unlist(lapply(fam.names.trees, efn))

# get the number of attested languages for each family
sort(table(pho3$glot_fam), decreasing = TRUE)
# from these those that have 6 or more languages
# are selected, but among those, families are exluded 
# if not a single language has tones:
# Pama-Nyungan, Dravidian, Uralic, Turkic, Uto-Aztecan
# Nakh-Daghestanian, Arawakan, Algic, Mixe-Zoque,
# Nuclear Torricelli, Pano-Tacanan, Quechuan,
# Worroran, Western Daly

fams <- c("Austroasiatic", "Mande", "Central Sudanic", 
          "Nuclear Trans New Guinea", 
          "Indo-European", "Nilotic", "Austronesian", "Atlantic-Congo", 
          "Afro-Asiatic", "Sino-Tibetan", "Otomanguean", "Tai-Kadai", 
          "Ta-Ne-Omotic", "Athabaskan-Eyak-Tlingit", 
          "Kadugli-Krongo", "Salishan")

## RAW CORRELATIONS BETWEEN TONE AND MWL
# function to get correlations between tone count and
# word length within each family (using pho2 in order not
# to restrict the analysis to languages used in the 
# phylogenetic analysis)
load("pho2.RData")
get.correlations <- function() {
  pho2x <- pho2[,c("count_tones", "glot_fam", "forty_mean")]
  # find rows where no cells are NA
  no.nas <- apply(pho2x, 1, function(x) sum(is.na(x)) == 0) 
  # retain only those
  pho2x <- pho2x[no.nas,]
  for (i in seq_along(fams)) {
    fam <- gsub(" ", "_", fams[i])
    cor.dat <- pho2x[pho2x$glot_fam==fam,c("forty_mean","count_tones")]
    N <- nrow(cor.dat)
    cor.out <- cor.test(cor.dat$forty_mean, cor.dat$count_tones)
    r <- round(unname(cor.out$estimate),3)
    p <- round(unname(cor.out$p.value),4)
    cat(fams[i], "\t", r, "\t", p, "\t", N, "\n")
  }
}
# get.correlations()
# results:
# Austroasiatic 	 -0.525 	 0.002 	 32 
# Mande 	 -0.399 	 0.013 	 38 
# Central Sudanic 	 -0.557 	 0.0133 	 19 
# Nuclear Trans New Guinea 	 0.594 	 0.0418 	 12 
# Indo-European 	 -0.277 	 0.0012 	 134 
# Nilotic 	 0.004 	 0.9849 	 21 
# Austronesian 	 -0.099 	 0.401 	 74 
# Atlantic-Congo 	 -0.297 	 0 	 337 
# Afro-Asiatic 	 -0.196 	 0.0563 	 96 
# Sino-Tibetan 	 -0.166 	 0.1508 	 76 
# Otomanguean 	 -0.444 	 0.2313 	 9 
# Tai-Kadai 	 -0.218 	 0.4969 	 12 
# Ta-Ne-Omotic 	 -0.774 	 0.0242 	 8 
# Athabaskan-Eyak-Tlingit 	 -0.526 	 0.1455 	 9 
# Kadugli-Krongo 	 0.432 	 0.3917 	 6 
# Salishan 	 0.197 	 0.6728 	 7 

## PIPELINE FOR ADDING ASJP LDN BRANCH LENGTHS TO GLOTTOLOG TREES
# The pipeline contains the following:
#  find.index(fam)
#  -- looks up a tree index given the name of a Glottolog family
#  prep.matrix(matrix.file)
#  -- reads a distance matrix file and returns a matrix
# simplify(tr)
#  -- takes out internal node names and branch lengths
#  fix.non.br(tr)
#  -- removes non-branching nodes in a pruned tree
#  pruning(fam, matrix.file)
#  -- outputs a list of a phylo object with the pruned tree and a distance matrix
#  bl(fam)
#  -- calling function producing a distance matrix and a pruned Glottolog tree 
#     with ASJP LDN branch lengths using the above functions
# fit.of.distances()
# -- does a mantel test of original LDN distances in comparison to 
#    patristic distances within the LDN distances as fitte to Glottolog trees

# function for looking up a tree index given the name of
# a Glottolog family
find.index <- function(fam) {which(tree.index==fam)}
# for instance: find.index("Austroasiatic") # 329

# function for reading a distance matrix and turning row and
# column names into glottocodes;
# the function presupposes that asjp_meta has been
# read from file and in memory as per above
prep.matrix <- function(matrix.file) {
  # assign glottocodes as column names
  m <- read.table(matrix.file)
  names.m <- names(m)
  w_names <- match(names.m, asjp_meta$Name)
  gcodes.m <- asjp_meta$Glottocode[w_names]
  # remove cases where a glottocode is repeated,
  # using the one with the lowest index in the matrix
  u <- unique(gcodes.m)
  w_u <- match(u, gcodes.m)
  m <- m[w_u,w_u]
  names(m) <- u
  # remove cases where a doculect does not have a glottocode
  # in asjp_meta (a few cases where they could not be well identified),
  # or where a doculect name does not appear at all in asjp_meta,
  # which is not expected to ever happen
  w_nocode <- union(which(names(m)==""),which(is.na(names(m))))
  if ( length(w_nocode) > 0 ) {
    m <- m[-w_nocode,-w_nocode]
  }
  # now that it is made certain that all column names are unique
  # they can also be assigned as rownames, which would not otherwise
  # be possible
  rownames(m) <- colnames(m)
  m <- as.matrix(m)
  return(m)
}

# function for taking branch lengths (digits) 
# and internal node labels out of a newick tree
# takes a newick string as input
simplify <- function(tr) {
  new1 <- gsub("\\:[[:digit:]]*", "", tr)
  phy1 <- read.tree(text=new1)
  phy1$node.label <- c()
  new2 <- write.tree(phy1)
  return(new2)
} 

# function for detecting and fixing cases of an internal 
# non-branching node left "hanging" after tree pruning
# as done with Round's keep_as_tip() function
fix.non.br <- function(tr) { 
  library(stringr)
  # tr is a string representing a newick tree
  # pattern ()
  baddo1 <- "\\([[:alnum:]]{8}\\)"
  while ( length(grep(baddo1, tr))==1 ) {
    to.fix <- regmatches(tr,regexpr(baddo1,tr))
    before <- strsplit(tr, baddo1)[[1]][1]
    after <- unlist(str_split(tr,baddo1,n=2))[2]
    fixed <- str_sub(to.fix,2,-2)
    tr <- paste0(before, fixed, after)
  }
  # pattern ((...))
  s1 <- strsplit(tr,"")[[1]]
  to.remove <- c()
  left.friend <- sort(which(s1=="("), decreasing=TRUE)  # positions of ( 
  right.indices <- which(s1==")")  # positions of )
  right.friend <- c()  # to hold ) pair members
  # pair members
  for (i in 1:length(left.friend)) {
    right.friend[i] <- right.indices[which(right.indices > left.friend[i])[1]]
    right.indices <- right.indices[-which(right.indices > left.friend[i])[1]]
  }
  # find cases where the right friends of two consecutive left guys are
  # also consecutive and store the indices of the outermost friend pair 
  # for later removal
  for (i in length(left.friend):2) {
    if ( left.friend[i-1]-left.friend[i] == 1 & right.friend[i]-right.friend[i-1] == 1 ) {
      to.remove <- c(left.friend[i], right.friend[i], to.remove)
    }
  }
  if ( length(to.remove) > 0 ) {
    s1 <- s1[-to.remove]
  }
  tr <- paste(s1, collapse="")
  return(tr)
}
    
# given a Glottolog tree and an ASJP LDN matrix, prune the tree to
# only contain the languages in the matrix
# Needs a family name, e.g. Sino-Tibetan, and a file name for a distance matrix, 
# e.g. matrix.file = "ST_LDN.txt"
pruning <- function(fam, matrix.file) {
  index.number <- find.index(fam)
  t.s <- read.tree(text = trees[index.number])  # stands for tree string
	# turn labels into bare glottocodes
	get.gc <- function(s) {  # stands for get glottocode
		a1 <- strsplit(s, "\\[")[[1]][2]
		a2 <- strsplit(a1, "\\]")[[1]][1]
	}
	t.s$node.label <- unlist(lapply(t.s$node.label, get.gc))
	t.s$tip.label <- unlist(lapply(t.s$tip.label, get.gc))
	m <- prep.matrix(matrix.file)
	Round.tree.phylo <- keep_as_tip(t.s, rownames(m))  # Erics Round's function
	Round.tree.newick <- write.tree(Round.tree.phylo)
  simplified.tree.newick <- simplify(Round.tree.newick)
  corrected.tree.newick <- fix.non.br(simplified.tree.newick)
	pruned.tree.phylo <- read.tree(text=corrected.tree.newick)
  return(list(pruned.tree.phylo,m))
}

# calling function: given a language family name (fam)
# produces a distance matrix, prunes the corresponding Glottolog tree,
# and adds ASJP LDN branch lengths to the Glottolog tree,
# outputting a phylo object
bl <- function(fam) {  # stands for branch lengths
  library(ape)  # drop.tip function
  matrix.file <- paste0(gsub(" ", "_", fam), "_LDN.txt")
  pruning.out <- pruning(fam, matrix.file)
  pruned.tree.phylo <- pruning.out[[1]]
  pruned.matrix <- pruning.out[[2]]
  bltree.phylo <- nnls.tree(pruned.matrix, pruned.tree.phylo, method="unrooted")
  # drop tips that are not in pho3
  languages.in.pho <- pho3$glottocode[pho3$glot_fam==fam]
  languages.in.tree <- bltree.phylo$tip.label
  languages.not.in.pho <- setdiff(languages.in.tree, languages.in.pho) 
  bltree.phylo.pruned <- drop.tip(bltree.phylo, languages.not.in.pho)
  return(bltree.phylo.pruned)
}

# function to check if for different families all the languages in pho3 are in 
# the Glottolog tree with branch lengths
# the check was run, and this is the case
# check <- function(fam) {
#  in.tree <- bl(fam)$tip.label
#  in.pho <- pho3$glottocode[pho3$glot_fam==fam]
#  check.out <- in.pho[!(in.pho %in% in.tree)]
#  return(check.out)
# }

# function to see how well the original LDN distances 
# correlate with patristic distances in the Glottolog tree supplied 
# with LDN distances
fit.of.distances <- function() {
  for (i in seq_along(fams)) {
    m.tree <- cophenetic(bl(fams[i]))  # function from stats package
    matrix.file <- paste0(gsub(" ", "_", fams[i]), "_LDN.txt")
    m.orig <- prep.matrix(matrix.file) 
    overlap <- intersect(row.names(m.tree), row.names(m.orig))
    mantel.out <- mantel.rtest(dist(m.tree[overlap,overlap]), dist(m.orig[overlap,overlap]), nrepet=9999)
    mantelr <- mantel.out$obs
    p <- mantel.out$pvalue
    cat(fams[i], "\t", round(mantelr,3), "\t", p, "\n", sep="")
  }
}
# Austroasiatic	0.934	1e-04
# Mande	0.967	1e-04
# Central Sudanic	0.972	1e-04
# Nuclear Trans New Guinea	0.928	1e-04
# Indo-European	0.928	1e-04
# Nilotic	0.955	1e-04
# Austronesian	0.934	1e-04
# Atlantic-Congo	0.899	1e-04
# Afro-Asiatic	0.928	1e-04
# Sino-Tibetan	0.923	1e-04
# Otomanguean	0.981	1e-04
# Tai-Kadai	0.97	1e-04
# Ta-Ne-Omotic	0.81	5e-04
# Athabaskan-Eyak-Tlingit	0.944	1e-04
# Kadugli-Krongo	0.96	0.0015
# Salishan	0.772	0.0061

## RUNNING BAYESTRAITS

# check how many languages are in the large families amenable
# for phylogenetic correlation
sum(sapply(pho3$glot_fam, function(x) x %in% fams))  # 889

# produce input files for Bayestraits for each family
for (i in seq_along(fams)) {
  phylo <- bl(fams[i])
  write.nexus(phylo, file=paste0(gsub(" ", "_", fams[i]), ".nex"))
  wordlength_tones <- pho3[pho3$glot_fam==fams[i],c("glottocode", "count_tones", "forty_mean")]
  write.table(wordlength_tones, file=paste0(gsub(" ", "_", fams[i]),".dat"), row.names=FALSE, col.names=FALSE, sep="\t", quote=FALSE)
}

# Run Bayestrait trait correlations for families in fams
run_correlation <- function() {
cat("family\tcorr\tno_Corr\tlogBF\n", file="results_BT_correlation.txt")
  for (i in seq_along(fams)) {
    fam.name <- gsub(" ", "_", fams[i])
    nexus.file <- paste0(fam.name, ".nex")
    data.file <- paste0(fam.name, ".dat")
    # run analysis assuming correlation to be present
    system.command <- paste0("BayesTraitsV4.exe ", nexus.file, " ", data.file, " < correlation_assumed_present.txt")
    cat(system.command, file="runBT.bat")
    system("runBT")
    file.rename(paste0(fam.name, ".dat.Log.txt"), paste0(fam.name, ".dat.Log_corpre.txt"))
    file.rename(paste0(fam.name, ".dat.Schedule.txt"), paste0(fam.name, ".dat.Schedule_corpre.txt"))
    file.rename(paste0(fam.name, ".dat.Stones.txt"), paste0(fam.name, ".dat.Stones_corpre.txt"))
    # run analysis assuming correlation to be absent
    system.command <- paste0("BayesTraitsV4.exe ", nexus.file, " ", data.file, " < correlation_assumed_absent.txt")
    cat(system.command, file="runBT.bat")
    system("runBT")
    file.rename(paste0(fam.name, ".dat.Log.txt"), paste0(fam.name, ".dat.Log_corabs.txt"))
    file.rename(paste0(fam.name, ".dat.Schedule.txt"), paste0(fam.name, ".dat.Schedule_corabs.txt"))
    file.rename(paste0(fam.name, ".dat.Stones.txt"), paste0(fam.name, ".dat.Stones_corabs.txt"))
    cor.pre.file <- paste0(fam.name, ".dat.Stones_corpre.txt")
    cor.abs.file <- paste0(fam.name, ".dat.Stones_corabs.txt")
    a1 <- readLines(cor.pre.file)
    a2 <- a1[length(a1)]
    a3 <- as.numeric(strsplit(a2, "\\\t")[[1]][2])
    b1 <- readLines(cor.abs.file)
    b2 <- b1[length(b1)]
    b3 <- as.numeric(strsplit(b2, "\\\t")[[1]][2])
    logBF <- round(2 * (a3 - b3),2)
    cat(fams[i], "\t", round(a3,2), "\t", round(b3,2), "\t", logBF, "\n", file="results_BT_correlation.txt", append=TRUE, sep="")
  }
}
run_correlation()
