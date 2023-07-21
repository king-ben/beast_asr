# This is a simple example of a topology test on a linguistic phylogeny

Step 1: decide on 2 topology hypotheses that need to be compared

Step 2: make xml files which run beast2 analyses. Each analysis is constrained to one of the two hypotheses. This means one or both analyses will have monophyly constraints

Step 3: run the preprocessing script which creates xml code that saves the likelihood of each cognate during the beast run

Step 4: copy and paste these into the xml files from step 2 (under where the other loggers are specified, i.e. near the bottom of the xml)

Step 5: run the two analyses

Step 6: run the postprocessing script to find out which cognates support different tree topologies



This example tests the North-East-West subgrouping of Lynch 2016 for Malekula languages against a completely unconstrained tree