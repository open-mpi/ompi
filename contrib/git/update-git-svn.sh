#!/bin/bash -x

svn_url="http://svn.open-mpi.org/svn/ompi/trunk"

# Use newer git (1.8.3+) to convert from svn2git which contains many conversion fixes
GIT18="/scrap/OMPI_REPO/GIT-1.8.3.4/bin/git"

# 1st copy location after svn->git conversion
# This repo should be cloned from SVN manually like this:
# $GIT18 svn clone $1 --no-metadata -A ../authors.txt -t tags -b branches --tags=tags/v1.0-series --tags=tags/v1.1-series --tags=tags/v1.2-series --tags=tags/v1.3-series --tags=tags/v1.4-series --tags=tags/v1.5-series --tags=tags/v1.6-series --tags=tags/v1.7-series  -T trunk . | tee > $logfile

temp_repository=/scrap/OMPI_REPO/GIT/ompi-temp

# branches to mirror into destination git repo
BRANCHES='master v1.7 v1.6'

# FS path to destination git repo
DEST_REPO='/hpc/home/git/ompi-vendor.git'

# location to generate Authors files from svn to git format
path2author='/scrap/OMPI_REPO/GIT'

logfile=/tmp/git-svn-update.$$.log
admin_email="git-mirror-admin"

local_fs_git_mirror=/hpc/home/git


# github URL with username/passwd
github_auth_file=$(dirname $0)/github_url.txt

if [ ! -f $github_auth_file ]; then
    echo ERROR: unable to fine URL with github mirror
    exit 1
fi

github_url=$(cat $github_auth_file)


###############################################################################

doit() {
   cmd="$*"
   eval $cmd >> $logfile 2>&1
   status=$?
   if test "$status" != "0"; then
      echo "Git SVN update failed.  Log:"
	  tail -20 $logfile | mail -s "OMPI replication failed: $cmd" $admin_email
      cat $logfile
      exit $status
   fi
}

function create_authors(){
    pushd .
    mv -f $path2author/authors.txt $path2author/authors.txt.backup
    svn cat ${svn_url}/contrib/authors-to-cvsimport.pl > authors-to-cvsimport.pl
    svn cat ${svn_url}/AUTHORS > AUTHORS
    chmod 755 authors-to-cvsimport.pl
    ./authors-to-cvsimport.pl AUTHORS > $path2author/authors.txt
    popd
}


###############################################################################

force=''
if [ "$1" = "--force" ] || [ "$1" = "-f" ] ; then
    echo "WARNING!!!  Force-pushing to git repositories, may result in rewinding!"
    force='--force'
fi

rm -f $logfile
touch $logfile
cd $temp_repository

create_authors

doit $GIT18 svn fetch --fetch-all

doit $GIT18 checkout master
for i in `$GIT18 branch|grep -v trunk|grep -v master|grep -v '@'`; do
    doit $GIT18 checkout ${i}
    doit $GIT18 merge remotes/${i}
done
doit $GIT18 checkout master
doit $GIT18 svn fetch
doit $GIT18 merge remotes/trunk

# GitHub mirror
doit $GIT18 push $force $github_url '*:*'
doit $GIT18 push $force $github_url :trunk


# Local FS git mirror, ompi.git and ompi-replication.git should be cloned before running this script

if [ -d $local_fs_git_mirror ]; then

    for repo in ompi.git ompi-replication.git; do 
        if [ -d $local_fs_git_mirror/$repo ]; then
            doit $GIT18 push $force $local_fs_git_mirror/$repo '*:*'
            doit $GIT18 push $force $local_fs_git_mirror/$repo :trunk
        fi
    done

    # Repo ompi-vendor
    doit sudo chown -R $USER $DEST_REPO

    cd $temp_repository
    for repon in $BRANCHES; do
        doit $GIT18 push $force $DEST_REPO $repon:$repon
    done
fi

rm -f $logfile
exit 0
