# deploy commands

# add a gh-pages branch -- https://jiafulow.github.io/blog/2020/07/09/create-gh-pages-branch-in-existing-repo/
# git submodule add  https://github.com/gcushen/hugo-academic.git themes/hugo-academic
# cd ./themes/hugo-academic
# git checkout 6f36c16
# 64b7d51


echo "Deleting old public"
rm -rf public
mkdir public
git worktree prune
rm -rf .git/worktrees/public/

echo "Checking out gh-pages branch into public"
# upstream/gh-pages origin/gh-pages
git worktree add -B gh-pages public origin/gh-pages

echo "Removing existing files"
rm -rf public/*

#find . -name "*.html" -type f -delete
# DO NOT DO THIS!
# rm -rf themes/hugo-academic
# On first time checkout use --init
# git submodule update --init --recursive
# To update
# git submodule update --recursive --remote

echo "Generating site"
Rscript -e "blogdown::build_site()"

echo "Updating gh-pages branch"
cd public && git add --all && git commit -m "Publishing to gh-pages (publish.sh)"

echo "Pushing to github"
git push --all

cd ../
