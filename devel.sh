
# make sure we're in sync with master
#git rebase master
# or!
git pull origin master

#cd COVID-19
#git fetch upstream
#git checkout master
#git merge upstream/master
#git push
#cd ..

#R CMD batch --vanilla code_generate_figures.R

git add .
git commit -m "Code development"

git push origin devel


