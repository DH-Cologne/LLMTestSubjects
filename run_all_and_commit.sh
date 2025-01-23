#! /bin/bash

# Create a random branch name using timestamp
BRANCH_NAME="results-$(date +%Y%m%d-%H%M%S)"

# Create and checkout new branch
git checkout -b $BRANCH_NAME

# Run tests
cd Code/LLaMA
fish ./run_all.fish
cd ../..

# Add results
git add Pronomina/LLMAnswers/
git commit -m "Automated results commit"
git push origin $BRANCH_NAME
