#!/bin/bash
#$ -M awillso2@nd.edu
#$ -m abe
#$ -pe smp 4
#$ -q long
#$ -N reduced_taxa-all_cov

module load R

R CMD BATCH ~/gjam-master/R/Runs/Reduced_taxa-all_cov/Run.R output_reduced_taxa-all_cov.out