#!/bin/bash

#PBS -N LisaSimulation
#PBS -lnodes=1
#PBS -lwalltime=05:00:00

module load openmpi/gnu
#module load R/3.2.3
#export R_LIBS=$HOME/rpackages:$R_LIBS

cp -r "$HOME"/LisaExample "$TMPDIR"
cd "$TMPDIR"/LisaExample

Rscript --vanilla Simulation.R iter

cp -r ./*.txt "$HOME"/LisaExample/output





