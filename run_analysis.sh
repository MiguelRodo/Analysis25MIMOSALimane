#!/usr/bin/env bash
#SBATCH --nodes=1
#SBATCH --ntasks=2
#SBATCH --job-name="mimosa"
#SBATCH --partition=ada

# Record the start time
start_time=$(date +%s)

echo "HOSTNAME: $HOSTNAME"

# Check if the SIF file exists using apptainer-exists
if ! apptainer-exists -s comp25mimosa; then
    echo "SIF file does not exist. Pulling it in..."
    apptainer-pull -r Comp25MIMOSA -u MiguelRodo
elseq
    echo "SIF file exists. No action needed."
fi

echo " "
echo " "
echo " "

echo "-------------------"
echo "Run pipeline"
date
# apptainer-rscript -s comp25mimosa "<r code without any double quotes>"
apptainer-rscript -s comp25mimosa "quarto::quarto_render('simluation.qmd')"
echo "Completed running pipeline"
date
echo "-------------------"
echo " "

# Record the end time
end_time=$(date +%s)

# Calculate the duration
duration=$((end_time - start_time))

# Convert duration to human-readable format
hours=$((duration / 3600))
minutes=$(( (duration % 3600) / 60 ))
seconds=$((duration % 60))

# Append the duration to the Slurm standard output log
echo "--- Script Duration ---"
printf "Elapsed time: %02d:%02d:%02d\n" $hours $minutes $seconds
echo "-----------------------"