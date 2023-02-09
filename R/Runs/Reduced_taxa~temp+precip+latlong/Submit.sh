#!/bin/bash
#$ -M ishuman2@nd.edu
#$ -m abe
#$ -pe smp 4
#$ -q long
#$ -N reduced_taxa-temp_precip_latlong

module load R

R CMD BATCH ~/gjam-master/R/Runs/Reduced_taxa~temp+precip_latlong/Run.R output_reduced_taxa-temp_precip_latlong.out