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
Makes an xml block for performing ancestral state reconstruction of every cognate set at a given node can either be at the most recent common ancestor node or its parent. Doning both allows for the reconstruction of innovations.

### `get_innovations`
Takes the output from a mrca and parent asr logger and returns innovations on that branch with probabilities.

### `asr_synonymy`
Finds the number of cognate sets in each meaning in each sampled generation, from the output of a beast2 asr.

### `get_innovations_terminal`
As for get innovations but uses the data instead of a mrca asr, to get innovations on terminal branches.


