#!/bin/bash

# Loop through all .csv files in the current folder
for file in *.csv; do
    # Check if the file contains a comment line with trailing commas
    if grep -q '^#.*,\+$' "$file"; then
        # Process the file and remove trailing commas in comment lines
        sed -i '/^#/s/,\+$//' "$file"
        echo "Processed: $file (trailing commas removed from comment lines)"
    else
        echo "Skipped: $file (no trailing commas in comment lines)"
    fi
done
