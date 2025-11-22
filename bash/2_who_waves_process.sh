#!/bin/bash
# =====================================================
# Script: who_waves_process.sh
# Purpose: Process raw accelerometer data using OxfordWearables packages for WAVES analysis
# Author: Ben Maylor (ben.maylor@ndph.ox.ac.uk)
# Date: 27/10/2025
# =====================================================

set -e

### USER INPUT ###
# =====================================================
	# Define the input and output folders
		input_folder="C:\Users\benm\WAVES\data" # Where the raw data is stored
		output_folder="C:\Users\benm\WAVES\outputs" # Where you want the outputs stored
# =====================================================


### AUTOMATED FROM HERE ###
# =====================================================

### DIRECTORY SETUP ###

	# Create outputs environment
		mkdir -p "$output_folder"
		mkdir -p "$output_folder"/{stepcount,walmsley,ssl}

	# Source conda environments
		source "$(conda info --base)/etc/profile.d/conda.sh"


### PROCESS DATA ###

	# STEPCOUNT #
		conda activate WHO_WAVES_stepcount
		# Loop through all files in the input folder
		for file in "$input_folder"/*; do
		  stepcount "$file" -o "$output_folder/stepcount"
		done


	# ACCELEROMETER - Walmsley Random Forest #
		conda activate WHO_WAVES_accelerometer
		for file in "$input_folder"/*; do
		  accProcess "$file" -o "$output_folder/walmsley"
		done


	### ACTINET - SSL ###
		conda activate WHO_WAVES_actinet
		for file in "$input_folder"/*; do
		  actinet "$file" -o "$output_folder/ssl"
		done

# =====================================================

echo "All files processed succesfully!"

### END ###