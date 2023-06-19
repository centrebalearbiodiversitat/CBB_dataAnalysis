# This bash proces remove the sequences from the file GenBank(full) downloaded from NCBI.
for file in Desktop/TEST/*; do  
new_file="${file%.txt}_removed.txt"
sed '/ORIGIN/,/\/\//d' "$file" > "$new_file"
done