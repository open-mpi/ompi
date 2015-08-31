When creating the github email hook thingy...

First, clone down the repo from github that you want:

  cd /u/mpiteam/git
  git clone ...

The rest of these instructions assume that you cloned into directory "foo".
Change "foo" as appropriate for your repo name.

Make a bare clone of your new clone:

  git clone --bare foo foo.git

In the non-bare repo, add a remote named "email" that is the bare repo:

  cd foo
  git remote add email /u/mpiteam/git/foo.git

Now in the foo repo, edit the config and in the remote "origin" section, add
the following line if it's not already there:

  emacs .git/config
  # Add this line in the "origin" section:

	fetch = +refs/heads/*:refs/remotes/origin/*

Still in that same config file, add a line in the remote "email" section:

	push = +refs/remotes/origin/*:refs/heads/*

Now exit the editor and remove the tracking branch of the origin:

    git branch -d -r origin/HEAD

IF YOU SCREW UP AND "push" to the email remote before deleting the origin/HEAD
tracking branch, then run the "git branch -d ..." and the following (both in
the original repo, not the bare repo):

    git push email :refs/heads/HEAD

Copy the hooks/post-receive script from any of the other bare repos to the
hooks subdir in this bare repo (e.g., from
/u/mpiteam/git/hwloc.git/hooks/post-receive).  Ensure that this script is
executable.

Edit the file named "description" in the bare repo and put a 1-word name of the
project in the file (this word will be used in the email subject).

Now in the bare repo, add the following "[hooks]" section -- MAKE SURE TO USE
THE CORRECT URL FOR YOUR GITHUB REPO, and set the email address properly:

[hooks]
    emailprefix = "GIT: "
    mailinglist = netloc-commits@open-mpi.org
    diffopts = --stat --summary --find-copies-harder
    emailmaxlines = 5000
    showrev="t=%s; printf https://github.com/open-mpi/netloc/commit/$t; echo; echo; git show --stat --patch -C $t; echo"
