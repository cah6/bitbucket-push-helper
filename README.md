# Bitbucket Push Helper Script

This is a small script to create Bitbucket pull request with team specific defaults.

## Installation

### Quick
To make this easier to use, I've pre-compiled the script for Mac. So, to get this working all you likely need to do is:
- Clone this repo
- Create a config in `conf/` that looks like `exampleConfig.json`, or modify that. Or maybe check your team's slack 
channel for a pre-made one!
- You're ready to use the script, passing in your config file location with `-c`! Still, I suggest you set up an alias 
so that you can easily reference this. For example, if you cloned this to your home directory, you could add the following
to your `~/.bash_profile` (or similar):
`alias pushbb="~/bitbucket-push-helper/app/PushHelper -c ~/bitbucket-push-helper/conf/myTeam.json"`
While you're there, set your `OKTA_USERNAME` and `OKTA_PASSWORD`:
```
export OKTA_USERNAME="firstname.lastname@appdynamics.com"
export OKTA_PASSWORD="my_password"
```


### From source 
To build from source:
- install stack: `curl -sSL https://get.haskellstack.org/ | sh`
- go to project root, run `stack build` and then, once that's done, `stack ghc -- -O2 -threaded app/PushHelper.hs`.

## Usage
Once you've done your work -and committed it locally-, just use this script `pushbb` in replacement of 
`git push -u origin blah`.  

Note that the script makes the title of the PR the first line of your latest commit, so make sure your last commit is
the one that starts with your ticket information, like `ANALYTICS-1111: Fix the bug in the thing`. 

## Motivation
I got tired of the repetitive actions necessary whenever I wanted to push a local change to bitbucket and add my team
members. My usual workflow was (once I've committed changes locally): 
1. `git branch` to see what my local branch name was, i.e. `feature/christian-change-1`
2. copy that branch name
3. type in `git push -u origin ` then paste the branch name to run the full thing
4. copy the location of the remote branch that bitbucket returns when you push
5. go to my browser, paste that location, click `Create pull request`
6. use a sheet of paper on my desk to remember to add ~8-10 people I add to every review, click create

This script basically does 1-3 as you would do it on the command line, and does 4-6 via bitbucket's REST api. 