#!/bin/sh

echo -e "\033[0;32mDeploying rendered website updates to GitHub...\033[0m"

# Build and send the rendered project to directory terryhahm.github.io
hugo -d ../terryhahm/github.io

# Go to the folder
cd ../terryhahm/github.io

# Add, commit and push 
git add .
git commit -m "Rebuilding site"
git push origin master

# Come back to project root
cd ../blog


