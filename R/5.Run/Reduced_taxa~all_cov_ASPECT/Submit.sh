#!/bin/bash
#$ -M ishuman2@nd.edu
#$ -m abe
#$ -pe smp 10
#$ -q long
#$ -N reduced_taxa-all_cov_ASPECT

module load R

R CMD BATCH ~/gjam-master/R/FINAL_RUNS/Reduced_taxa~all_cov_ASPECT/Run.R output_reduced_taxa-all_cov_ASPECT.out