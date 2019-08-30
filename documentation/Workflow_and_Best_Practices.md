---
title: "Development Workflow and Best Practices"
output:
  html_document: 
    keep_md: yes
    self_contained: yes
    toc: yes
    toc_float: yes
---

This document covers our workflow and procedures for making changes to the `2019_development` repo. It's designed to give us the simplest system that allows version control, multiple collaborators, code review and reproducibility.

The document assumes you have basic familiarity with R, Git and GitHub, and that you've got [Git Bash](https://gitforwindows.org/), [R](https://www.r-project.org/) and [RStudio](https://www.rstudio.com/) installed.





# Set up the repository locally

The most up-to-date version of the code is on GitHub at https://github.com/Greater-London-Authority/population_projections/.

The SETUP document in this folder explains how to clone the repository to your local drive and summarises the folder structure. Make sure you install the `popmodules` package contained within the project. In RStudio:
```
devtools::install("model_code/popmodules")
```





# Workflow

We're using a [Feature Branch Workflow](https://www.atlassian.com/git/tutorials/comparing-workflows/feature-branch-workflow), where all development work is done on branches, and the `master` branch is never modified directly.

If you're only going to take away one thing from this document it needs to be **NEVER MAKE CHANGES TO THE MASTER BRANCH DIRECTLY**.

Instead, this is how your workflow should go. The description here is long, but most steps are usually quick.



### 1: Start a new branch

```
git checkout -b name-of-your-feature-branch
```
creates a new branch. Aim to implement just one feature per branch (e.g. functions for a new births module, unit tests for the scripts that generate housing projections, a bugfix, a report) as this makes the review process much easier. Name the branch with your initials (optional) and something informative about what it'll do - e.g. this document's first commit by Chris Fairless was on the branch `cf-setup-documentation`.



### 2: Implement a feature

Make regular commits to keep track of your work. Whenever you do a thing, commit with a message - a one line summary of what it does followed by a more detailed explanation if necessary. Your future self will be grateful for this detail.

Note: if you just want to update or edit the previous commit (and you haven't pushed), you can use
```
git commit --amend
```
to combine things with the previous commit and edit the commit message.



### 3: (Optional) Interactive rebase

If you're not familiar with rebasing, [this article](https://www.atlassian.com/git/tutorials/rewriting-history/git-rebase) gives an introduction.

When you've finished the feature, or you just want to make your work available on GitHub, you have the option of rebasing. This is a polite way to neaten up your commit history and combine commits that together form something larger. Most of the time this is a chance to combine staging-post commits and bugfixes. The goal is to make the project's history easier to follow.

Note that *you should never rebase commits that have been already pushed to the shared repository* - even if they haven't yet been merged with another branch. This is because the rebase creates a new commit history which is no longer compatible with the version in the repository. It's better to have a messy history than an incompatible one, becuse you'll never need to force an overwrite.

To rebase interactively, looking at all changes since you branched from `master`, use
```
git rebase -i master
```
Here you can choose which commits you want to keep, which you want to combine together (squash or fixup), and which you want to reword to better describe the combined product. Git will then combine the commits, prompting you when it needs additonal input. If you're not sure what to do, or it runs into trouble, you can always stop with `git rebase --abort`

If part of your branch history has already been pushed, you may want to replace `master` in the above command with a different location on the git branches e.g. `HEAD~3` looks at the last three commits, or `26c182cf801b4b449127d3e7d43050befa1b014a` to refers to everything since that particular commit.



### 4: Bring things up to date with the shared repository

Other people may have made edits to the central repository since you started work on your branch, and that means the `master` you branched from is different from the `master` you want to merge with.

You can check if this is the case by running
```
git fetch --dry-run
```
If no changes are reported to the `master` branch you can skip to the next step.

Otherwise you can run
```
git checkout master
git pull
git checkout name-of-your-feature-branch
git merge master
```
This merges the updated `master` branch into your feature branch (and leaves `master` unchanged). Git will attempt to replay the new edits to `master` on top of your branch. If it encounters any conflicts you will be asked to fix them before you can continue. This gives you the chance to fix things locally before you push your code for review. Remember you can always abort a merge with `git merge --abort`.

If you're spending a long time working on a single branch, it's a good idea to do this every now and again, in case someone else has been working on the same code (or there's helpful new code).

You're now ready to share your work!



### 5: Push to the shared repository

```
git push -u origin name-of-your-feature-branch
```
This uploads your branch to the shared repository. The -u flag makes it a tracked branch, so if someone else pushes commits to it, `git pull` will update your local version.



### 6: Request a code review

On the repository's [GitHub](https://github.com/Greater-London-Authority/population_projections/) you can set up a pull request. This gives the team a chance to review your code before it's added to the `master` branch.

On the GitHub page, click the 'Pull requests' tab and then 'New pull request'. This will let you select your branch and the branch you want to merge it to (usually `master`). In the right-hand sidebar you can request reviewers from the team who will look over your request.

GitHub then provides a page for everyone to inspect, comment, discuss, and make changes to the code.

When reviewing someone else's request you may also want to check out the branch and test the code on your own machine with 
```
git pull origin/name-of-their-feature-branch
```

GitHub will also tell you if there will be any merge conflicts with the branch you're merging into. If this is the case, follow the instructions in Step 4 above to merge the branches locally, resolve conflicts and push again - the pull request should be updated automatically.

Once everyone is satisfied with the changes, it's finally possible to merge the feature into `master`.



### 7: Merge into `master`

GitHub will handle this for us at the click of a button, assuming all merge conflicts have been resolved. Hit the big green 'Merge pull request' button. You'll be given the option of deleting the feature branch at the same time - it's usually good to say yes and keep things uncluttered.

You can then update your local version as well. Synchronise your local repository with GitHub, anld remove remote deleted remote branches
```
git fetch -p
```

Merge the changes into your local copy of `master`
```
git checkout master
git pull
```

And delete your local copy of the feature branch now that it's merged
```
git branch -d name-of-your-feature-branch
```




# The model on the Q:/ drive

We keep a copy of the model on the Q:/ drive so that people without access to Git can run the model, and so that runs can be archived in an easy-to-access location.

On the Q:/ drive everything is stored in a bare (uneditable) repository at `Q:\Teams\D&PA\Demography\Projections\2019_development.git` with a Git hook to recreate it at `Q:\Teams\D&PA\Demography\Projections\2019_development` after each push.

You'll never need to check anything out of the repository - always work via GitHub - but after implementing new features you'll want to update the repository here as well.

The first time you do this, you'll need to set up the folder as a remote repository and give it a nickname. I've called mine `qdrive`:

```
git remote add qdrive "Q:/Teams/D&PA/Demography/Projections/2019_development.git"
```

You can then update the `master` branch on the Q:/ drive with
```
git push qdrive master
```

Since this is a bare repository, you can run the models from the Q:/ drive, but you can't make lasting changes to the code, since it'll be overwritten the next time anyone pushes. (Model run data will not be affected since these folders are in .gitignore).






# Project best practices

We'll update this list as we go along! But to start with:

### Model conventions

We're still developing this, but we've roughly settled on the following:

*  We have some standard names for columns in our population data frames. A population data frame will have the columns `year`, `gss_code`, `age` and `sex`. The population will be in a column called `popn` but we prefer more descriptive names for other counts, e.g. `births`, `deaths`. When writing a package function, these can be used as default values. When writing a model, the data can (usually) be assumed to have these names
*  When writing functions, parameter names are only semi-standardised if we're honest. An input population will usually be called `popn`. A variable referring to column name(s) will start with `col_*`


### Coding best practices

*  When writing a module that will be reused across models (these will generally be in the `popmodules` package), try to make it flexible and general - build it to work with different data formats, column names, etc. Write checks and tests to make sure it behaves as expected and throws an informative warning or error when it encounters something it's not expecting. On the other hand, when writing something that's for one particular model and which will be used only for specific purposes (e.g. reading a particular file, wrangling something model-specific) there's no need to do this extra work, and it's safe to assume predictable input etc. (which is not to say that a couple of checks are a bad idea...). If you find yourself re-using this code in other models later, think about upgrading and generlaising it then
*  Try to write everything as functions. Keep functions and the code that executes them separate
*  Settings are a kind of data - don't include them in code. Put all options and settings into a model config file. The goal is to build models where the users won't need to edit any code to run them: everything will be done via the config file
*  Don't hard-code file paths
    +  Store them as variables in a config file
    +  Use `path()` and `rprojroot()` to build paths to locations within the repository, `path()` - the code will work on others' machines, and on both Windows and Linux
*  Try to build in checks to your functions to validate input and make sure that everything is behaving as expected. The `assertthat()` function is your friend
*  If you change a function that more than one model or module depends on, let people know in the pull request so that they can fix dependencies. Try not to make breaking changes!
*  Don't use `require()` - if it fails it doesn't tell you
*  Remove feature branches after you merge them

### Working in packages

It's really easy to add a function to a package - just write the function, put it in the `R/` folder and add some basic `roxygen2` documentation:
```
#' Do a Thing
#' @export
```
Then `devtools::document()` and `devtools::install()` will set it up ready to go.

But to get the most value out of packages, its worth using their other capabilities. This book by Hadley Wickham is a good reference http://r-pkgs.had.co.nz. Here are a few of the best practices:

*  Documenting functions can be easy and semi-automated with `roxygen2` syntax, explained [here](http://r-pkgs.had.co.nz/man.html). This gives a simple format to work with, and then the command `devtools::document()` automatically creates the documentation files and the helpfile queryable with `?functionname`
*  Don't use anything that invisibly modifies the larger R environment:
    +  `rm(list=ls())` and `setwd()` aren't necessary when programming functions
    +  `library()` loads too many things invisibly and can overwrite other functions. When you only need to make a couple of calls to a package use `packagename::function()` instead. This reduces the likelihood of conflicts between package namespaces. If you're using extra operators (e.g. `%>%`) or are making a lot of calls to a package, `@ImportFrom` in your roxygen comments might be your friend - e.g.
    ```
    #' @ImportFrom dplyr group_by summarise
    ```
    for more details see http://r-pkgs.had.co.nz/namespace.html
*  In functions that will be used in a number of (potentially unknown) ways, write some tests using `testthat`. These use very simple inputs and check that the function creates the outputs you'd expect. They go in the folder `tests/testthat/` and have a simple structure - see http://r-pkgs.had.co.nz/tests.html. Your tests can be as brief or as in-depth as you find useful. I like to write tests for edge-cases of my functions, checking they work with difficult column names, factors or grouped tibbles, missing values etc. Tests can be re-run them every time you update the model to check that everything is still working as expected. Tests are also great for debugging, because they provide you with simple example code to execute when you're trying to create a minimum reproducible example.
* You can run some standard checks on your package with the command `devtools::check(args="--no-install")` (the `--no-install` is something to do with the way the GLA network is set up - you can ignore it if you're on another machine). It will fail if the package isn't up to scratch and give you warnings suggesting non-essential improvements
* You can run some standard checks on your functions with the command `codetools::checkUsage(functionname, all = TRUE)`. It will spot common pitfalls (e.g. accessing a variable outside of the function environment) and advise on improvements
*  The package `usethis` can be pretty useful. Its goal is to standardise everything you do while building a package, so that packages all have near-identical structure. It takes a while to learn, and sometimes it's no better than making changes by hand. I've found it most useful when I want to do something moderately complex and I don't know packages well enough to understand how - you can create READMEs, vignettes, documentation, etc with `usethis`. A few choice commands are:
    +  `usethis::use_package()` to add a package to the DESCRIPTION file's list of dependencies
    +  `usethis::use_data()` to save an object into the package's `data/` folder
    +  `usethis::use_testthat()` and `usethis::use_test("functionname")` to set up testing for the first time, and to create a test script for a partcular function.
