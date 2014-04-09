There are several files and scripts in this directory.

- Crontab for jaguar.crest.iu.edu (aka mtt.open-mpi.org, where much
  nightly processing for the Open MPI project occurs).

- Crontab for lion.crest.iu.edu (aka www.open-mpi.org, which you can't
  directly SSH login to from outside of iu.edu).

- Script used to send diff emails from github repos (because github
  refuses to send diff emails).  This script is fired from cron.

- README for how to setup diff emails for new github repos (i.e., how
  to use the above script to send diff emails for each github repo).

- Script that is run on lion.crest.iu.edu (aka www.open-mpi.org) to
  update the bitbucket mercurial mirror.  It is run via cron every 15
  minutes.

--> Note that the github SVN mirroring of the main OMPI SVN repo (ompi)
    is maintained by Mellanox.  Its script does not currently appear
    here in the SVN repo.
