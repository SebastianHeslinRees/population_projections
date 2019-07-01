---
title: "Development Workflow and Best Practices"
output:
  html_document: 
    keep_md: yes
    self_contained: no
    toc: yes
    toc_float: yes
---

This document covers our workflow and procedures for making changes to the `2019_development` repo. It's designed to give us the simplest system that allows version control, multiple collaborators, code review and reproducibility.

The document assumes you have basic familiarity with R, Git and (the hard one) Microsoft Teams, and that you've got [Git Bash](https://gitforwindows.org/), [R](https://www.r-project.org/) and [RStudio](https://www.rstudio.com/) installed.



# Set up the repository locally

You can't keep any changes that you make to the repository on the Q:/ drive. Every time someone pushes an update to the code the folders monitored by Git (code and documentation) are overwritten. This means that you can run the models from the Q:/ drive, but not make lasting changes to the code.

More technically: the repository on the Q:/ drive is a bare repository (with no code) in the `2019_development.git` folder, and it's set up with a Git hook that (over)writes the repository at `2019_development` whenever a new commit is pushed.

So you need to have a local copy to make any changes. In Git Bash and run
```
git clone "Q:\Teams\D&PA\Demography\Projections\2019_development.git" "M:/Path/to/local/directory"
```
you can then navigate to the local directory and work with the code there.



# Workflow

We're using a [Feature Branch Workflow](https://www.atlassian.com/git/tutorials/comparing-workflows/feature-branch-workflow), where all development work is done on branches, and the `master` branch is never modified directly.

If you're only going to take away one thing from this document it needs to be **NEVER MAKE CHANGES TO THE MASTER BRANCH DIRECTLY**.

Instead, this is how your workflow should go. The description here is long, but most steps are usually quick.

### 1: Start a new branch

```
git checkout -b name-of-your-feature-branch
```
creates a new branch. As a rule, you should only implement one feature per branch (e.g. functions for a new births model, unit tests for the scripts that generate housing projections, a bugfix, a report). Name the branch with your initials (optional) and something informative about what it'll do - e.g. this document's first commit by Chris Fairless is on the branch `cf-setup-documentation`.


### 2: Implement a feature

Make regular commits to keep track of your work. Whenever you do a thing, commit with a message - a one line summary of what it does followed by a more detailed explanation if necessary.

Note: if you just want to update or add to the previous commit, you can use
```
git commit --amend
```
to combine things with the previous commit and edit the commit message.

When you've finished the feature, or you want to make your work so far available to everyone else, move on to the next step.



### 3: (Optional) Interactive rebase

If you're not familiar with rebasing, [this article](https://www.atlassian.com/git/tutorials/rewriting-history/git-rebase) gives an introduction.

When you're ready to push your branch, it's polite to neaten up your commit history to combine commits that together form a larger change (e.g. fixes to earlier commits, or one big change split across several smaller ones) and make it easier to follow the project's history.

Note that *you should never rebase commits that have been already pushed to the shared repository* - even if they haven't yet been merged with another branch. This is because the rebase creates a new commit history which is no longer compatible with the version in the repository. It's better to have a messy history than an incompatible one, becuse you'll never need to force an overwrite.

To rebase interactively with `master` use
```
git rebase -i master
```
Here you can choose which commits you want to keep, which you want to combine together (squash or fixup), and which you want to reword to better describe the combined product. Git will then combine the commits, prompting you when it needs additonal input. If you're not sure what to do, or it runs into trouble, you can always stop with `git rebase --abort`

If part of your branch history has already been pushed, you may need to replace `master` in the above command with e.g. `HEAD~3` for the last three commits, or `26c182cf801b4b449127d3e7d43050befa1b014a` to refer to everything since that particular commit.



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
This merges the updated `master` branch into your feature branch (and leaves `master` unchanged). Git will attempt to replay the new edits on top of your branch. If it encounters any conflicts you will be asked to fix them before you can continue. This gives you the chance to fix any conflicts before you push your code for review. Remember you can always abort a merge with `git merge --abort`.

If you're spending a long time working on a single branch, it's a good idea to do this every now and again, in case someone else has been working on the same code (or there's helpful new code).

You're now ready to share your work!


### 5: Push to the shared repository

```
git push -u origin name-of-your-feature-branch
```
This uploads your branch to the shared repository. The -u flag makes it a tracked branch, so if someone else pushes commits to it, `git pull` will update your local version.



### 6: Request a code review

We're not doing full GitHub pull requests at the moment. Instead, send a message to the Microsoft Teams [Pull request](https://teams.microsoft.com/_#/conversations/Pull%20requests?threadId=19:2f451d422008462f94d1db898422eaf3@thread.skype&ctx=channel) channel.

This will give everyone the chance to check out and review your work. People can suggest changes or edits, or can make further commits.

If someone asks you to review their code, you can make a local copy of the branch with
```
git pull origin/name-of-their-feature-branch
```
or just work in the remote repository (remember that changes will get overwritten). There are a number of ways to inspect changes in code, the Git GUI app packaged with with Git Bash is one of them.

Once everyone is satisfied with the changes, it's finally possible to merge the feature into `master`.



### 7: Merge into `master` and push

There are a bunch of commands here, but this is the safest way to merge and push, in case something unexpected has changed.

First, get the latest versions of all the branches and check for any conflicts by re-merging master into the feature branch:
```
git checkout master
git pull            # get latest versions of master and your feature branch
git checkout name-of-your-feature-branch
git merge master
```
Resolve any conflicts as required. If there are big changes it may need to go back for review.

Merge the feature branch back into master:

```
git checkout master
git merge name-of-your-feature-branch
```

You can now delete the feature branch, since its full history is part of `master` and there's no reason to clutter the repository with an extra copy.

```
git branch -d name-of-your-feature-branch
```

And finally, you can push the new `master` branch
```
git push
```



# Project best practices

We'll update this list as we go along! But to start with:

*  Try to write everything as functions. Keep functions and the code that executes them separate
*  Settings are a kind of data - don't include them in code. Put all options and settings into a config file
*  Don't hard-code file paths
    +  Store them as variables in a config file
    +  Use `here()` to build paths to locations within the repository
*  Try to build in checks to your functions to validate input and make sure that everything is behaving as expected. The `assertthat()` function is your friend. If you find yourself making similar checks several times, split them into a separate module for everyone to use
*  If you change a function that more than one model or module depends on, let people know in the pull request so that they can fix dependencies. Try not to make breaking changes!
*  Don't use `require()`
*  Remove feature branches after you merge them

### Optional extras

These are things we may want to consider doing later on

*  Document your functions with `roxygen2` syntax, explained [here](http://r-pkgs.had.co.nz/man.html).
*  Don't use `rm(list=ls())`
*  Don't use `library()` when you only need to make a couple of function calls - use `packagename::function()` instead, which doesn't load the full package. This reduces the likelihood of conflicts between package namespaces
*  Build file paths with the `path()` function, which means they'll work on both Windows and Linux
