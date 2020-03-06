cd COVID-19
git pull
cd ..

R CMD batch --vanilla code_generate_figures.R

git add .
git commit -m "Update figures"

git push origin master


