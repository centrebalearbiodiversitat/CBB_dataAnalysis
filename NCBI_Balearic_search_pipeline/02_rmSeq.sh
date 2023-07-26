# You can use the sed command in Bash to add an empty line after // if it is not already present.

# Add empty line after //
# awk '{if (!/\// || !/\/\//) print; else print $0 "\n"}' TEST1/1_Eunicella_verrucosa.txt > temp && mv temp TEST1/1_Eunicella_verrucosa.txt

folder_path="TEST1"

for file in "$folder_path"/*; do
  if [ -f "$file" ]; then
    awk '{if (!/\// || !/\/\//) print; else print $0 "\n"}' "$file" > temp && mv temp "$file"
  fi
done

# Remove double empty lines
# sed '/^$/N;/\n$/D' TEST1/1_Eunicella_verrucosa.txt > temp && mv temp TEST1/1_Eunicella_verrucosa.txt

for file in "$folder_path"/*; do
  if [ -f "$file" ]; then
    sed '/^$/N;/\n$/D' "$file" > temp && mv temp "$file"
  fi
done


# This bash proces remove the sequences from the file GenBank(full) downloaded from NCBI.
for file in Desktop/TEST1/*; do  
new_file="${file%.txt}_removed.txt"
sed '/ORIGIN/,/\/\//d' "$file" > "$new_file"
done
