#ALL FUNCTIONS ASSUME FIRST SITE IN EACH PARTITION IS AN ASCERTAINMENT COLUMN

# extracts partitions from a nexus file
# reads the assumptions block and converts it to a 3-cloumn dataframe
# columns are partition name, from index and to index
find_partitions <- function(file){
  nex <- readLines(file)
  from <- grep("begin assumptions", nex, ignore.case=T)+1
  from <- from[grep("charset", nex[from])]
  if(length(from)==0){stop("assumptions block with character sets not found")}
  to <- which(nex=="end;")
  to <- to[to>from][1]-1
  cb <- nex[from:to]
  cb <- gsub(".*charset ", "", cb)
  cb <- gsub(";", "", cb)
  cb <- gsub(" = ", "\t", cb)
  cb <- gsub("=", "\t", cb)
  cb <- gsub("-", "\t", cb)
  cbd <- sapply(cb, strsplit, split="\t")
  cbd <- t(as.data.frame(cbd))
  rownames(cbd) <- NULL
  colnames(cbd) <- c("concept", "from", "to")
  cbd <- transform(cbd, from=as.numeric(from), to=as.numeric(to))
  return(cbd)
}

#makes a vector of character state labels read from nexus characterstatelabels block
find_site_names <- function(file){
  nex <- readLines(file)
  if(length(grep("begin charstatelabels", nex, ignore.case = T))>0){
    from <- grep("begin charstatelabels", nex, ignore.case=T)+1
    if(length(from)==0){stop("characterstatelabels block not found")}
    to <- which(nex=="end;")
    to <- to[to>from][1]-1
    cb <- nex[from:to]
    cb <- gsub("    ", "", cb)
    cb <- gsub(".*\\s", "", cb)
    return(cb)
  }else{
    from <- grep("charstatelabels", nex, ignore.case=T)+1
    if(length(from)==0){stop("characterstatelabels block not found")}
    to <- grep("^\\s*;\\s*$", nex)
    to <- to[to>from][1]-1
    cb <- nex[from:to]
    cb <- gsub("    ", "", cb)
    cb <- gsub(".*\\s", "", cb)
    cb <- gsub(",", "", cb)
    return(cb)
  }
}


# Makes a table of rate parameter links
# binning is by default 1-10, 11-20 (excl ascertainment)
rate_links <- function(wp, cutoff=10){
  n <- wp[,3]-wp[,2]
  wp <- wp[order(n),]
  n <- n[order(n)]
  names(n) <- wp[,1]
  b <- wp[,1:2]
  for(i in 1:ceiling(max(n)/cutoff)){
    b[which(n>(cutoff*(i-1)) & n<(cutoff*i+1)),2] <- names(n)[which(n>(cutoff*(i-1)) & n<(cutoff*i+1))][1]
  }
  names(b)[2] <- "rate"
  return(b)
}

order_partitions <- function(wp){
  n <- wp[,3]-wp[,2]
  wp <- wp[order(n),]
  return(wp)
}

#orders the partitions in increasing size
order_nexus_partitions <- function(file){
  wp <- find_partitions(file)
  nex <- readLines(file)
  from <- grep("begin assumptions", nex, ignore.case=T)+1
  from <- from[grep("charset", nex[from])]
  if(length(from)==0){stop("assumptions block with character sets not found")}
  to <- which(nex=="end;")
  to <- to[to>from][1]-1
  partitions_ordered <- order_partitions(wp)
  nex[from:to] <- paste0("\t", "charset ", partitions_ordered$concept, " = ", partitions_ordered$from, "-", partitions_ordered$to, ";")
  output <- sub(".nex", "_sorted.nex", file)
  writeLines(nex, output)
}


#makes logger of likelihood of every cognate set
siteprobs_blocks <- function(parts, names, logevery=1000){
  logger <- newXMLNode("logger")
  xmlAttrs(logger) <- c(id="Sitelikelihoods", fileName="site_likelihoods.txt", logEvery=logevery, mode="tree")
  for(i in 1:nrow(parts)){
    log <- newXMLNode("log", parent=logger)
    pnam <- parts[i,1]
    first <- parts[i, 2]
    last <- parts[i, 3]
    cnam <- names[first:last]
    
    xmlAttrs(log) <- c(id=paste("sitelik", pnam, sep="."), spec="babel.util.SiteLikelihoodLogger", likelihood=paste("@treeLikelihood", pnam, sep="."), value=paste(cnam, collapse=" "))
  }
  return(logger)
}

#compares site logger of 2 analyses and returns a table of significantly different cognates and a plot
topology_test <- function(sample1, sample2, label1="topology1", label2="topology2", burnin=0.1, hpd.level=0.95, colors=c("#d0968f", "#043fc9")){
  require(ggplot2)
  require(ggthemes)
  require(data.table)
  sitelik1 <- fread(sample1)
  sitelik2 <- fread(sample2)
  class(sitelik1) <- "data.frame"
  class(sitelik2) <- "data.frame"
  ascertained <- which(grepl("ascertainment", colnames(sitelik1)))
  sitelik1 <- sitelik1[round(burnin*nrow(sitelik1)):nrow(sitelik1), setdiff(1:ncol(sitelik1), ascertained)]
  sitelik2 <- sitelik2[round(burnin*nrow(sitelik2)):nrow(sitelik2), setdiff(1:ncol(sitelik2), ascertained)]
  hpd1 <- t(apply(sitelik1, 2, hpd, p=hpd.level))
  hpd2 <- t(apply(sitelik2, 2, hpd, p=hpd.level))
  median1 <- as.matrix(apply(sitelik1, 2, median))
  median2 <- as.matrix(apply(sitelik2, 2, median))
  results1 <- cbind(rownames(hpd1), median1, hpd1, label1)
  results2 <- cbind(rownames(hpd2), median2, hpd2, label2)
  support <- vector()
  for(i in 1:nrow(results1)){
    support[i] <- NA
    if(hpd1[i,1] > hpd2[i,2]){
      support[i] <- label1
    }
    if(hpd2[i,1] > hpd1[i,2]){
      support[i] <- label2
    }
  }
  results1 <- cbind(results1, support)
  results2 <- cbind(results2, support)
  results1 <- results1[which(is.na(support)==F),]
  results2 <- results2[which(is.na(support)==F),]
  results <- rbind(results1, results2)
  colnames(results) <- c("cognate", "median", "lower", "upper", "topology", "support")
  results <- transform(results, median=as.numeric(median),
                       lower=as.numeric(lower),
                       upper=as.numeric(upper))
  results[2:4] <- results[2:4]-results$median[1:(0.5*nrow(results))]
  results$cognate <- factor(results$cognate,
                            levels = results[,1][order(results$median[(0.5*nrow(results)+1):nrow(results)], decreasing=T)])
  results$support <- factor(results$support, levels=c(label1, label2))
  p <- ggplot(results, aes(x=cognate,
                           y=median,
                           color=topology)) +
    scale_color_manual(values=colors) +
    geom_point() +
    geom_errorbar(aes(ymin=lower,
                      ymax=upper)) +
    facet_grid(support~., space="free_y", scales="free_y") +
    theme_hc() +
    theme(axis.text.x = 
            element_text(angle = 90, 
                         vjust = 0.5, hjust=1), 
          legend.position="top", 
          #strip.text = element_text(size=12)
    ) +
    ylab("relative log likelihood") +
    coord_flip()
  
  out <- list(significant_cognates=results, plot=p)
}



# makes ancestral state reconstruction logger
asr_blocks <- function(parts, names=NULL, links, id="AncestralSequenceLogger", logevery=1000, taxonset, logOrigin=FALSE, fileName="asr_logger.txt"){
  logger <- newXMLNode("logger")
  xmlAttrs(logger) <- c(id=id, fileName=fileName, logEvery=logevery, mode="tree")
  for(i in 1:nrow(parts)){
    log <- newXMLNode("log", parent=logger)
    pnam <- parts[i,1]
    link <- links[which(links[,1]==pnam),2]
    first <- parts[i, 2]
    last <- parts[i, 3]
    xmlAttrs(log) <- c(id=paste(id, pnam, sep="."), spec="beastlabs.evolution.likelihood.AncestralStateLogger", data=paste("@orgdata", pnam, sep="."), siteModel=paste("@SiteModel.s:", link, sep=""), branchRateModel="@RelaxedClock.c:clock", tree="@Tree.t:tree", taxonset=paste("@", taxonset, sep=""))
    if(is.null(names)){
      xmlAttrs(log) <- c(value=paste(paste0(pnam, ".ascertainment"), paste0(pnam, ".", 1:(last-first), collapse=" ")))
    } else{
      cnam <- names[first:last]
      xmlAttrs(log) <- c(id=paste(id, pnam, sep="."), spec="beastlabs.evolution.likelihood.AncestralStateLogger", data=paste("@orgdata", pnam, sep="."), siteModel=paste("@SiteModel.s:", link, sep=""), branchRateModel="@RelaxedClock.c:clock", tree="@Tree.t:tree", value=paste(cnam, collapse=" "), taxonset=paste("@", taxonset, sep=""))
    }
    if(logOrigin==TRUE){
      xmlAttrs(log) <- c(logParent="true", logMRCA="false")
    }
  }
  return(logger)
}


## Make the logger XML blocks for multiple taxon sets with unique names for the ID and output
## files and write everything to a single XML file that can be copied and pasted into the
## BEAST XML
asr_blocks_multitaxa <- function(taxa, names, links, logevery, outfile="loggers.xml") {
    for (taxon in taxa) {
        xml_origin <- asr_blocks(wp, names=names, links,
                                 id=paste("AncestralSequenceLoggerOrigin", taxon, sep=""),
                                 logevery=logevery, taxonset=taxon, logOrigin=T,
                                 fileName=paste("asr_logger_", taxon, "_parent.txt", sep=""))
        xml_mrca <- asr_blocks(wp, names=wn, links,
                               id=paste("AncestralSequenceLoggerMRCA", taxon, sep=""),
                               logevery=logevery, taxonset=taxon, logOrigin=F,
                               fileName=paste("asr_logger_", taxon, ".txt", sep=""))
        origin_str <- toString.XMLNode(xml_origin)
        mrca_str <- toString.XMLNode(xml_mrca)
        write(paste(origin_str, "\n"), file=outfile, append=TRUE)
        write(paste(mrca_str, "\n"), file=outfile, append=TRUE)
    }
}

## Collapses the raw covarion states to 0 or 1 for presence / absence.
## Uses external Python script `collapse_covarion.py`. Processes all files in
## current working directory with the prefix "asr_logger"
library(reticulate)
collapse_covarion <- function() {
    py_run_file("collapse_covarion.py")
}


# calculates synonymy at ancestral state reconstruction node
# input is the logger output by the beast analysis after it has had collapse_covarion.py run on it
# asr logger is read in with fread
asr_synonymy <- function(asr, wp, burnin=0.1, thinfactor=10){
  b <- round(burnin*nrow(asr))
  n <- nrow(asr)
  nc <- ncol(asr)
  asr <- asr[seq(b, n, by=thinfactor)]
  n <- nrow(asr)
  syn <- matrix(NA, nrow = n, ncol = nrow(wp))
  for(i in 1:nrow(wp)){
    for(j in 1:n){
      t <- asr[j, wp[i,2]:wp[i,3]]
      syn[j,i] <- sum(t)
    }
  }
  return(syn)
}

#find changes on a branch. Takes asr from the nodes at either end of the branch
get_innovations <- function(asr_mrca, asr_parent, burnin=0.1, siglevel=0.5, mode="innovations"){
  b <- round(burnin*nrow(asr_mrca))
  n <- nrow(asr_mrca)
  asr_mrca <- asr_mrca[b:n,]
  asr_parent <- asr_parent[b:n,]
  
  if(mode=="innovations"){
    asr_comp <-ifelse(asr_parent==0 & asr_mrca==1, 1, 0)
  }
  if(mode=="losses"){
    asr_comp <-ifelse(asr_parent==1 & asr_mrca==0, 1, 0)
  }
  
  asr_mean <- apply(asr_comp, 2, mean)
  sig <- asr_mean[asr_mean>siglevel]
  sig <- sig[order(sig, decreasing=T)]
  return(sig)
}

# find innovations on terminal branches i.e. the data at the tips is known and fixed
# takes a vector of tip states
# MUST be in same order as asr file suggested code:
# wn <- find_site_names("../make_asr_xml/ie.nex")
# cognateorder <- match(names(asr_T), wn)
# taxon_data <- nex[[taxon]]
# taxon_data <- taxon_data[cognateorder]

get_innovations_terminal <- function(taxon_data, asr_parent, burnin=0.1, siglevel=0.5, mode="innovations"){
  b <- round(burnin*nrow(asr_parent))
  n <- nrow(asr_parent)
  asr_parent <- asr_parent[b:n,]
  asr_parent <- as.data.frame(asr_parent)
  
  if(mode=="innovations"){
    asr_parent_test <- asr_parent[,which(taxon_data==1)]
    asr_comp <- 1-asr_parent_test
  }
  if(mode=="losses"){
    asr_comp <- asr_parent[,which(taxon_data==0)]
  }
  
  asr_mean <- apply(asr_comp, 2, mean)
  sig <- asr_mean[asr_mean>siglevel]
  sig <- sig[order(sig, decreasing=T)]
  return(sig)
}


# make monophyly constraint from vector of taxon names
constraint_XML <- function(name, taxonset){
  dist <- newXMLNode(
    "distribution", 
    attrs=c(id=paste0(name, ".prior"),
            spec="beast.base.evolution.tree.MRCAPrior",
            monophyletic="true",
            tree="@Tree.t:tree"))
  taxset <- newXMLNode(
    "taxonset",
    parent=dist,
    attrs=c(id=name, 
            spec="TaxonSet")
  )
  for(i in taxonset){
    newXMLNode(
      parent=taxset,
      "taxon",
      attrs=c(idref=i)
    )
  }
  return(dist)
}

# make monophyly constraint with rogue taxa from vector of included taxa and vector of rogues
rogueconstraint_XML <- function(name, taxonset, rogues){
  dist <- newXMLNode(
    "distribution", 
    attrs=c(id=paste0(name, ".prior"),
            spec="beastlabs.math.distributions.MRCAPriorWithRogues",
            monophyletic="true",
            tree="@Tree.t:tree"))
  taxset <- newXMLNode(
    "taxonset",
    parent=dist,
    attrs=c(id=name, 
            spec="TaxonSet")
  )
  for(i in taxonset){
    newXMLNode(
      parent=taxset,
      "taxon",
      attrs=c(idref=i)
    )
  }
  roguesxml <- newXMLNode(
    "rogues",
    parent=dist,
    attrs=c(id=paste0(name, ".rogues"), 
            spec="TaxonSet")
  )
  for(i in rogues){
    newXMLNode(
      parent=roguesxml,
      "taxon",
      attrs=c(idref=i)
    )
  }
  return(dist)
}

