# functions for ancestral state reconstruction and topology tests using beast2

This package is primarily designed to work on linguistic data. In particular lexical data organised into partitions corresponding to meaning classes, where the first site in each meaning partition is an all zero ascertainment column.

### `find_partitions`
reads nexus assumptions block and outputs a data frame with the partition name and the to and from indices.
Works on a couple of nexus file styles, but there may be some types of nexuses that do not work.

### `find_site_names`
reads nexus character state labels and makes a vector of cognate names.
Works on a couple of nexus file styles, but there may be some types of nexuses that do not work.

### `rate_links`
when beast partitions rates (e.g. when binning meanings by number of cognate sets) the linked parameters will bear the name of the first meaning in the group of partitions with the shared rate. This function makes a table showing the rate paramter name for each meaning partition.

### `order_partitions`
takes the output of find_partitions and orders them by number of cognate sets.

### `order_nexus_partitions`
all in one function that rewrites nexus file with charsets (i.e. meaning partitions) in order of number of cognate sets. Useful to set up rate binning.

### `siteprobs_blocks`
Makes xml logger block that saves the log likelihood of every cognate set.

### `topology_test`
Takes output of two analyses with the xml code produced by siteprobs_blocks. Will find cognate sets that differ between the two analyses.

### `asr_blocks`
Makes an xml block for performing ancestral state reconstruction of every cognate set at a given node can either be at the most recent common ancestor node or its parent. Doing both allows for the reconstruction of innovations.

### `innovation_blocks`
Makes an xml block for logging innovations of every cognate set on the branch leading to a given node. Makes use of a custom java class InnovationLogger in the [innovations](https://github.com/king-ben/innovations) beast2 addon package.

### `asr_blocks_multitaxa`
Given a list of multiple taxon sets present in the BEAST2 XML, creates uniquely named MRCA and origin ancestral state loggers for each set, set to write to uniquely named log files, and saves them all to a single XML file (default: "loggers.xml") which can then be pasted into the logger section of the BEAST2 XML. Logfiles will be named "asr_logger_[taxonset].txt" and "asr_logger_[taxonset]_parent.txt"

### `collapse_covarion`
Calls external python script `collapse_covarion.py` to collapse the raw covarion states to 0 and 1 for absence / presence for all files prefixed with "asr_logger" in cwd. Must be run with `collapse_covarion.py` in current working directory (Isaac will fix this at some point)

### `get_innovations`
Takes the output from a mrca and parent asr logger and returns innovations on that branch with probabilities.

### `asr_synonymy`
Finds the number of cognate sets in each meaning in each sampled generation, from the output of a beast2 asr.

### `get_innovations_terminal`
As for get innovations but uses the data instead of a mrca asr, to get innovations on terminal branches.


