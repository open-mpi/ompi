This Python script is useful in generating a spreadsheet for the
annual Open MPI committers review.

It uses the Github Python API to query all the users, repos, and teams
in the "open-mpi" github.com organization.

It generates a CSV with users as rows and repos as columns.

A bunch of information is included for each user in an attempt to help
identify each user.

For each user, if that user has commit access to a given repo, the
github team name that gives them commit access to that repo is listed
in the corresponding cell.

The rationale is that if you need to remove someone's access to a
repo, that tells you exactly which Github team(s) to remove the user
from in order to remove their commit access to that repo.

To run this script:

- You need to have "python3" in your path.
- "pip3 install pygithub"
- Go to https://github.com/settings/tokens and make a personal access
  token with full permissions to the repo and org.
- Save that token in a single-line text file named 'oauth-token.txt'
  in the same directory as this script (I didn't make the script
  feature-full, e.g., to accept a CLI arg telling where the token file
  is -- sorry).

Running the script will generate "permissions.csv".

I suggest uploading this CSV to Google Drive (e.g., in the shared Open
MPI folder) and converting it to a Google Sheet.  Then you can color
the cells as appropriate, resize columns, wrap text, ...etc., and then
ask the community to validate all the committers.
