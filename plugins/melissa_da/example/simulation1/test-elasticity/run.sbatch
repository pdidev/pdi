#!/bin/bash
#SBATCH -N 18
#SBATCH -A igf@cpu
#SBATCH --time=0:25:0

source ms

python3 -u script.py
