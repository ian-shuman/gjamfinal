#!/bin/bash
#$ -M awillso2@nd.edu
#$ -m abe
#$ -pe smp 4
#$ -q long
#$ -N all_taxa-all_cov

module load R

R CMD BATCH ~/gjam-master/R/Runs/All_taxa~all_cov/Run.R output_all_taxa-all_cov.out