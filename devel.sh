cd COVID-19
git fetch upstream
git checkout master
git merge upstream/master
git push
cd ..

## make sure we're in sync with master
##git rebase master
## or!
#git pull origin master

R CMD batch --vanilla code_global.R
R CMD batch --vanilla code_selection.R

git add .
git commit -m "Code development"

git push origin devel


