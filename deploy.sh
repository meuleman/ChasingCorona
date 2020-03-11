cd COVID-19
git fetch upstream
git checkout master
git merge upstream/master
git push
cd ..

R CMD batch --vanilla code_global.R
R CMD batch --vanilla code_selection.R

git add .
git add --force PDF_figures
git add --force PNG_figures
git commit -m "Update figures"

git push origin master


