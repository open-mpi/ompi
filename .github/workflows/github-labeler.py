#!/usr/bin/env python3

"""

The script applies a label in the form "Target: {branchName}. If necessary, it
removes labels in the same form, but NOT for the target branch. For instance, if
someone edited the target branch from v4.0.x to v5.0.x

"""

import os
import re
import sys

from github import Github

# ==============================================================================

GITHUB_BASE_REF = os.environ.get('GITHUB_BASE_REF')
GITHUB_TOKEN = os.environ.get('GITHUB_TOKEN')
GITHUB_REPOSITORY = os.environ.get('GITHUB_REPOSITORY')
PR_NUM = os.environ.get('PR_NUM')

# Sanity check
if (GITHUB_BASE_REF is None or
    GITHUB_TOKEN is None or
    GITHUB_REPOSITORY is None or
    PR_NUM is None):
    print("Error: this script is designed to run as a Github Action")
    exit(1)

# ==============================================================================

# Given a pullRequest object, the function checks what labels are currently on
# the pull request, removes any matching the form "Target: {branch}" (if
# {branch} is not the current target branch), and adds the correct label.
def ensureLabels(pullRequest):
    needsLabel = True
    targetPrefix = "Target: "
    targetLabel = f"{targetPrefix}{GITHUB_BASE_REF}"
    for label in pullRequest.get_labels():
        if label.name.startswith(targetPrefix):
            if label.name == targetLabel:
                needsLabel = False
            else:
                print(f"Removing label '{label.name}")
                pullRequest.remove_from_labels(label)
    if needsLabel:
        print(f"Adding label '{targetLabel}")
        pullRequest.add_to_labels(targetLabel)
    return None

# ==============================================================================

g = Github(GITHUB_TOKEN)
repo = g.get_repo(GITHUB_REPOSITORY)
prNum = int(PR_NUM)
pr = repo.get_pull(prNum)
ensureLabels(pr)
