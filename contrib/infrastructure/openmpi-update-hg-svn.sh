#!/bin/sh -f
#
# This script keeps the mercurial bitbucket repo in sync with the
# upstream OMPI SVN repository.
#
# This script runs on www.open-mpi.org under the mpiteam user.
# It is located in /home/ompi-hg/update-hg-svn.sh, and is fired
# via the following mpiteam crontab entry:
#
# 10,25,40,55 * * * * /home/ompi-hg/update-hg-svn.sh
#

logfile=/tmp/ompi-hg-cron-$$

. /etc/profile.d/modules.sh

module unload mercurial
module load mercurial subversion

doit() {
    rm -f $logfile
    $* 2> $logfile 1>> $logfile
    if test "$?" != "0"; then
       echo Mercurial SVN mirroring failed
       cat $logfile
       rm -f $logfile
       exit 1
    fi
    rm -f $logfile
}

cd /home/ompi-hg
doit hg convert --source-type svn http://svn.open-mpi.org/svn/ompi /home/ompi-hg/ompi-svn-mirror

# Also push up to bitbucket
cd ompi-svn-mirror
doit hg push ssh://hg@bitbucket.org/ompiteam/ompi-svn-mirror --new-branch

exit 0
