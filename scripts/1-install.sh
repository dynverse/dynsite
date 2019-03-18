
echo 'Installing dynsite'
R -e 'setRepositories(ind = 1:4); devtools::install("package", dependencies = TRUE, upgrade = TRUE)'

echo 'Installing html-proofer'
gem install html-proofer
