#!/bin/bash
# =====================================================
# Script: who_waves_setup.sh
# Purpose: Create and initialize the WAVES conda environments
# Author: Ben Maylor (ben.maylor@ndph.ox.ac.uk)
# Date: 23/10/2025
# =====================================================

set -e

### STEPCOUNT ###
# =====================================================
	ENV_NAME="WHO_WAVES_stepcount"

	echo "Creating conda environment: $ENV_NAME"
	conda create -y -n $ENV_NAME python=3.9 openjdk pip

	echo "Activating environment..."
	source "$(conda info --base)/etc/profile.d/conda.sh"
	conda activate $ENV_NAME

	echo "Installing stepcount"
	pip install stepcount==3.5
	echo "'$ENV_NAME' setup complete!"
# =====================================================

### Actinet ###
# =====================================================
	ENV_NAME="WHO_WAVES_actinet"

	echo "Creating conda environment: $ENV_NAME"
	conda create -y -n $ENV_NAME python=3.9 openjdk pip

	echo "Activating environment..."
	conda activate $ENV_NAME

	echo "Installing actinet"
	pip install actinet==0.4.2
	echo "'$ENV_NAME' setup complete!"
# =====================================================

### Accelerometer ###
# =====================================================
	ENV_NAME="WHO_WAVES_accelerometer"

	echo "Creating conda environment: $ENV_NAME"
	conda create -y -n $ENV_NAME python=3.9 openjdk pip

	echo "Activating environment..."
	conda activate $ENV_NAME

	echo "Installing accelerometer"
	pip install accelerometer==7.3.0
	echo "'$ENV_NAME' setup complete!"
# =====================================================

### Forest ###
# =====================================================
	ENV_NAME="WHO_WAVES_oak"

	echo "Creating conda environment: $ENV_NAME"
	conda create -y -n $ENV_NAME python=3.12 timezonefinder==8.1.0

	echo "Activating environment..."
	conda activate $ENV_NAME

	echo "Installing forest"
  pip install git+https://github.com/onnela-lab/forest@ffb36be508d6161e8fbfe70a27048e218cc9394d
	echo "'$ENV_NAME' setup complete!"
# =====================================================

echo "WAVES environments setup succesfully!"

### END ###
