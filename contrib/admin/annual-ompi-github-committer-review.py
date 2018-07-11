#!/usr/bin/env python3

'''
Requirements:

$ pip3 install pygithub

Go to https://github.com/settings/tokens and make a personal access
token with full permissions to the repo and org.

'''

import csv

from github import Github
from pprint import pprint

#--------------------------------------------------------------------

# Read the oAuth token file.
# (you will need to supply this file yourself -- see the comment at
# the top of this file)
token_filename = 'oauth-token.txt'
with open(token_filename, 'r') as f:
    token = f.read().strip()
g = Github(token)

#--------------------------------------------------------------------

print("Getting open-mpi organization...")
org = 'open-mpi'
ompi_org = g.get_organization(org)

#--------------------------------------------------------------------

print("Loading organization repos...")
all_members = dict()
repos = dict()
for repo in ompi_org.get_repos():
    print("Found Org Repo: {repo}".format(repo=repo.name))

    # For each repo, get the teams on that repo
    repo_teams   = dict()
    for team in repo.get_teams():
        out = ("   Found team on repo {org}/{repo}: {team} ({perm}) "
               .format(org=ompi_org.name, repo=repo.name,
                       team=team.name, perm=team.permission))
        # We only care about teams with push permissions
        if team.permission == "pull":
            print("{out} -- SKIPPED".format(out=out))
            continue

        print(out)

        # Find all the members of this team
        team_members = dict()
        member_teams = dict()
        for member in team.get_members():
            print("      Found member: {name}"
                  .format(name=member.login))
            team_members[member.id] = member

            if member.id not in all_members:
                all_members[member.id] = {
                    'member'       : member,
                    'member_teams' : dict(),
                }

            # Find the member in the org and add this team to them
            all_members[member.id]['member_teams'][team.id] = team

        # Same the results
        repo_teams[team.id] = {
            'team'         : team,
            'team_members' : team_members,
        }

    # Save the results
    repos[repo.id] = {
        'repo'        : repo,
        'repo_teams'  : repo_teams,
    }


print("All the repos:")
pprint(repos)
pprint(all_members)

#--------------------------------------------------------------------

# Pre-load the field names with info about the user and repo
fieldnames = ['login', 'name', 'email', 'company']

# Add all the repo names
for _, rentry in repos.items():
    repo = rentry['repo']
    fieldnames.append("{org}/{repo}"
                      .format(org=ompi_org.login,
                              repo=repo.name))

#--------------------------------------------------------------------

# Now write out the CSV
outfile = 'permissions.csv'
print("Writing: ".format(outfile=outfile))
with open(outfile, 'w', newline='') as csvfile:
    writer = csv.DictWriter(csvfile, fieldnames=fieldnames,
                            quoting=csv.QUOTE_ALL)
    writer.writeheader()
    for mid, mentry in all_members.items():
        member = mentry['member']
        print("  Writing member: {member}".format(member=member.login))

        # Initial entries about the user
        row = {
            'login' : member.login,
            'name'  : member.name,
            'email' : member.email,
            'company' : member.company,
        }

        # Fill in values for each repo
        for _, rentry in repos.items():
            repo = rentry['repo']

            found = list()
            for tid, tentry in rentry['repo_teams'].items():
                if tid in mentry['member_teams']:
                    team = tentry['team']
                    found.append(team.name)

            row[repo.full_name] = ', '.join(found)

        writer.writerow(row)
