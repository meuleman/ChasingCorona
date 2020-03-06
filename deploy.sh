cd COVID-19
git fetch upstream
git checkout master
git merge upstream/master
cd ..

R CMD batch --vanilla code_generate_figures.R

git add .
git commit -m "Update figures"

git push origin master


