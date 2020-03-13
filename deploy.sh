./get_data.sh

R CMD batch --vanilla code_global.R
R CMD batch --vanilla code_all_regions.R
#R CMD batch --vanilla code_increase.R
#R CMD batch --vanilla code_custom_regions.R

git add .
git commit -m "Update figures"

git push origin master


