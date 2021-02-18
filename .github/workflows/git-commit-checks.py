#!/usr/bin/env python3

"""

Sanity tests on git commits in a Github Pull Request.

This script is designed to run as a Github Action.  It assumes environment
variables that are available in the Github Action environment.  Specifically:

* GITHUB_WORKSPACE: directory where the git clone is located
* GITHUB_SHA: the git commit SHA of the artificial Github PR test merge commit
* GITHUB_BASE_REF: the git ref for the base branch
* GITHUB_TOKEN: token authorizing Github API usage
* GITHUB_REPOSITORY: "org/repo" name of the Github repository of this PR
* GITHUB_REF: string that includes this Github PR number

This script tests each git commit between (and not including) GITHUB_SHA and
GITHUB_BASE_REF multiple ways:

1. Ensure that the committer and author do not match any bad patterns (e.g.,
"root@", "localhost", etc.).

2. Ensure that a proper "Signed-off-by" line exists in the commit message.
    - Merge commits and reverts are exempted from this check.

3. If required (by the git-commit-checks.json config file), ensure that a
"(cherry picked from commit ...)" line exists in the commit message.
    - Commits that are solely comprised of submodule updates are exempted from
      this check.
    - This check can also be disabled by adding "bot:notacherrypick" in the
      Pull Request description.

4. If a "(cherry picked from commit ...)" message exists, ensure that the commit
hash it mentions exists in the git repository.

If all checks pass, the script exits with status 0.  Otherwise, it exits with
status 1.

"""

import os
import re
import git
import json
import copy
import argparse

from github import Github

GOOD = "good"
BAD  = "bad"

GITHUB_WORKSPACE  = os.environ.get('GITHUB_WORKSPACE')
GITHUB_SHA        = os.environ.get('GITHUB_SHA')
GITHUB_BASE_REF   = os.environ.get('GITHUB_BASE_REF')
GITHUB_TOKEN      = os.environ.get('GITHUB_TOKEN')
GITHUB_REPOSITORY = os.environ.get('GITHUB_REPOSITORY')
GITHUB_REF        = os.environ.get('GITHUB_REF')

# Sanity check
if (GITHUB_WORKSPACE is None or
    GITHUB_SHA is None or
    GITHUB_BASE_REF is None or
    GITHUB_TOKEN is None or
    GITHUB_REPOSITORY is None or
    GITHUB_REF is None):
    print("Error: this script is designed to run as a Github Action")
    exit(1)

#----------------------------------------------------------------------------

"""
Simple helper to make a 1-line git commit message summary.
"""
def make_commit_message(repo, hash):
    commit  = repo.commit(hash)
    lines   = commit.message.split('\n')
    message = lines[0][:50]
    if len(lines[0]) > 50:
        message += "..."

    return message

#----------------------------------------------------------------------------

"""
The results dictionary is in the following format:

    results[GOOD or BAD][commit hash][check name] = message

If the message is None, there's nothing to print.

A git commit hash will be in either the GOOD or the BAD results -- not both.
"""
def print_results(results, repo, hashes):
    def _print_list(entries, prefix=""):
        for hash, entry in entries.items():
            print(f"{prefix}* {hash[:8]}: {make_commit_message(repo, hash)}")
            for check_name, message in entry.items():
                if message is not None:
                    print(f"{prefix}    * {check_name}: {message}")

    # First, print all the commits that have only-good results
    if len(results[GOOD]) > 0:
        print("\nThe following commits passed all tests:\n")
        _print_list(results[GOOD])

    # Now print all the results that are bad
    if len(results[BAD]) > 0:
        # The "::error ::" token will cause Github to highlight these
        # lines as errors
        print(f"\n::error ::The following commits caused this test to fail\n")
        _print_list(results[BAD], "::error ::")

#----------------------------------------------------------------------------

"""
Global regexp, because we use it every time we call
check_signed_off() (i.e., for each commit in this PR)
"""
prog_sob = re.compile(r'Signed-off-by: (.+) <(.+)>')

def check_signed_off(config, repo, commit):
    # If the message starts with "Revert" or if the commit is a
    # merge, don't require a signed-off-by
    if commit.message.startswith("Revert "):
        return GOOD, "skipped (revert)"
    elif len(commit.parents) == 2:
        return GOOD, "skipped (merge)"

    matches = prog_sob.search(commit.message)
    if not matches:
        return BAD, "does not contain a valid Signed-off-by line"

    return GOOD, None

#----------------------------------------------------------------------------

def check_email(config, repo, commit):
    emails = {
        "author"    : commit.author.email.lower(),
        "committer" : commit.committer.email.lower(),
    }

    for id, email in emails.items():
        for pattern in config['bad emails']:
            match = re.search(pattern, email)
            if match:
                return BAD, f"{id} email address ({email}) contains '{pattern}'"

    return GOOD, None

#----------------------------------------------------------------------------

"""
Global regexp, because we use it every time we call check_cherry_pick()
(i.e., for each commit in this PR)
"""
prog_cp = re.compile(r'\(cherry picked from commit ([a-z0-9]+)\)')

def check_cherry_pick(config, repo, commit):
    def _is_entirely_submodule_updates(repo, commit):
        # If it's a merge commit, that doesn't fit our definition of
        # "entirely submodule updates"
        if len(commit.parents) == 2:
            return False

        # Check the diffs of this commit compared to the prior commit,
        # and see if all the changes are updates to submodules.
        submodule_paths = [ x.path for x in repo.submodules ]
        diffs = repo.commit(f"{commit}~1").tree.diff(commit)
        for diff in diffs:
            if diff.a_path not in submodule_paths:
                # If we get here, we found a diff that was not exclusively
                # a submodule update.
                return False

        # If we get here, then all the diffs were submodule updates.
        return True

    # If this commit is solely comprised of submodule updates, don't
    # require a cherry pick message.
    if len(repo.submodules) > 0 and _is_entirely_submodule_updates(repo, commit):
        return GOOD, "skipped (submodules updates)"

    non_existent = dict()
    found_cherry_pick_line = False
    for match in prog_cp.findall(commit.message):
        found_cherry_pick_line = True
        try:
            c = repo.commit(match)
        except ValueError as e:
            # These errors mean that the git library recognized the
            # hash as a valid commit, but the GitHub Action didn't
            # fetch the entire repo, so we don't have all the meta
            # data about this commit.  Bottom line: it's a good hash.
            # So -- no error here.
            pass
        except git.BadName as e:
            # Use a dictionary to track the non-existent hashes, just
            # on the off chance that the same non-existent hash exists
            # more than once in a single commit message (i.e., the
            # dictionary will effectively give us de-duplication for
            # free).
            non_existent[match] = True

    # Process the results for this commit
    if found_cherry_pick_line:
        if len(non_existent) == 0:
            return GOOD, None
        else:
            str = f"contains a cherry pick message that refers to non-existent commit"
            if len(non_existent) > 1:
                str += "s"
            str += ": "
            str += ", ".join(non_existent)
            return BAD, str

    else:
        if config['cherry pick required']:
            return BAD, "does not include a cherry pick message"
        else:
            return GOOD, None

#----------------------------------------------------------------------------

def check_all_commits(config, repo):
    # Get a list of commits that we'll be examining.  Use the progromatic form
    # of "git log GITHUB_BASE_REF..GITHUB_SHA" (i.e., "git log ^GITHUB_BASE_REF
    # GITHUB_SHA") to do the heavy lifting to find that set of commits.
    git_cli = git.cmd.Git(GITHUB_WORKSPACE)
    hashes  = git_cli.log(f"--pretty=format:%h", f"origin/{GITHUB_BASE_REF}..{GITHUB_SHA}").splitlines()

    # The first entry in the list will be the artificial Github merge commit for
    # this PR. We don't want to examine this commit.
    del hashes[0]

    #------------------------------------------------------------------------

    # Make an empty set of nested dictionaries to fill in, below. We initially
    # create a "full" template dictionary (with all the hashes for both GOOD and
    # BAD results), but will trim some of them later.
    template = { hash : dict() for hash in hashes }
    results  = {
        GOOD : copy.deepcopy(template),
        BAD  : copy.deepcopy(template),
    }

    for hash in hashes:
        overall = GOOD

        # Do the checks on this commit
        commit  = repo.commit(hash)
        for check_fn in [check_signed_off, check_email, check_cherry_pick]:
            result, message = check_fn(config, repo, commit)
            overall         = BAD if result == BAD else overall

            results[result][hash][check_fn.__name__] = message

        # Trim the results dictionary so that a hash only appears in GOOD *or*
        # BAD -- not both. Specifically:
        #
        # 1. If a hash has BAD results, delete all of its results from GOOD.
        # 2. If a hash has only GOOD results, delete its empty entry from BAD.
        if overall == BAD:
            del results[GOOD][hash]
        else:
            del results[BAD][hash]

    return results, hashes

#----------------------------------------------------------------------------

"""
If "bot:notacherrypick" is in the PR description, then disable the
cherry-pick message requirement.
"""
def check_github_pr_description(config):
    g      = Github(GITHUB_TOKEN)
    repo   = g.get_repo(GITHUB_REPOSITORY)

    # Extract the PR number from GITHUB_REF
    match  = re.search("/(\d+)/", GITHUB_REF)
    pr_num = int(match.group(1))
    pr     = repo.get_pull(pr_num)

    if "bot:notacherrypick" in pr.body:
        config['cherry pick required'] = False

#----------------------------------------------------------------------------

def load_config():
    # Defaults
    config = {
        'cherry pick required' : False,
        'permit empty' : False,
        'bad emails' : [
            '^root@',
            'localhost',
            'localdomain',
        ],
    }

    # If the config file exists, read it in and replace default values
    # with the values from the file.
    filename = os.path.join(GITHUB_WORKSPACE, '.github',
                            'workflows', 'git-commit-checks.json')
    if os.path.exists(filename):
        with open(filename) as fp:
            new_config = json.load(fp)
        for key in new_config:
            config[key] = new_config[key]

    return config

#----------------------------------------------------------------------------

def main():
    config = load_config()
    check_github_pr_description(config)

    repo = git.Repo(GITHUB_WORKSPACE)
    results, hashes = check_all_commits(config, repo)
    print_results(results, repo, hashes)

    if len(results[BAD]) == 0:
        print("\nTest passed: everything was good!")
        exit(0)
    else:
        print("\nTest failed: sad panda")
        exit(1)

#----------------------------------------------------------------------------

if __name__ == "__main__":
    main()
