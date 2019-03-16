git branch -d master
git checkout --orphan master
git add .
git commit -m "update site"
git push --force --set-upstream origin master

cp CNAME public/CNAME
git push origin `git subtree split --prefix public master`:master --force
