echo "Deploying to github"

cd public

git init
git remote add origin git@github.com:dynverse/dynverse.github.io.git
git add .
git commit --allow-empty -m "Website build"
git push origin master --force

cd ../
