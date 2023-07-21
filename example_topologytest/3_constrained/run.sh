#!/bin/sh
#SBATCH --job-name mal
#SBATCH --get-user-env
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --output output.%j.run
#SBATCH --mem 50G

/home/king/BEASTv2.7.3/beast/bin/beast -threads 4 -beagle -overwrite -working Malekula.xml > runinfo.txt


