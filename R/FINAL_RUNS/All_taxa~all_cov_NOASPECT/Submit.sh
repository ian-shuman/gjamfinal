#!/bin/bash
#$ -M ishuman2@nd.edu
#$ -m abe
#$ -pe smp 10
#$ -q long
#$ -N all_taxa_all_cov_NOASPECT

module load R

R CMD BATCH ~/gjam-master/R/FINAL_RUNS/All_taxa~all_cov_NOASPECT/Run.R output_all_taxa-all_cov_NOASPECT.out