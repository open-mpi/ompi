GitHub, Git, and related topics
===============================

GitHub
------

PMIx's Git repository is `hosted at GitHub
<https://github.com/openpmix/openpmix>`_.

#. First, you will need a Git client. We recommend getting the latest
   version available. If you do not have the command ``git`` in your
   path, you will likely need to download and install Git.
#. `openpmix <https://github.com/openpmix/openpmix/>`_ is the main PMIx
   repository where most active development is done.  Git clone this
   repository.  Note that the use of the ``--recursive`` CLI option is
   necessary because PMIx uses Git submodules::

      shell$ git clone --recursive https://github.com/openpmix/openpmix.git

Note that Git is natively capable of using many forms of web
proxies. If your network setup requires the user of a web proxy,
`consult the Git documentation for more details
<https://git-scm.com/>`_.

Git commits: open source / contributor's declaration
----------------------------------------------------

In order to remain open source, all new commits to the PMIx
repository must include a ``Signed-off-by:`` line, indicating the
submitter's agreement to the :ref:`PMIx Contributor's Declaration
<contributing-contributors-declaration-label>`.

.. tip:: You can use the ``-s`` option to ``git commit`` to
         automatically add the ``Signed-off-by:`` line to your commit
         message.

.. _git-github-branch-scheme-label:

Git branch scheme
-----------------

Generally, PMIx has two types of branches in its Git repository:

#. ``master``:

   * All active development occurs on the ``master`` branch (new features,
     bug fixes, etc.).

#. Release branches of the form ``vMAJOR.MINOR.x`` (e.g., ``v4.0.x``,
   ``v4.1.x``, ``v5.0.x``).

   * The ``.x`` suffix indicates that this branch is used to create
     all releases in the PMIx vMAJOR.MINOR series.
   * Periodically, the PMIx community will make a new release
     branch, typically from ``master``.
   * A Git tag of the form ``vMAJOR.MINOR.RELEASE`` is used to
     indicate the specific commit on a release branch from where
     official PMIx release tarball was created (e.g., ``v4.1.0``,
     ``v4.1.1``, ``v4.1.2``, etc.).

Once a bug is fixed or a new feature is implemented on ``master``, it is
cherry-picked over to the relevant release branch(es).

.. attention:: It may seem odd to some, but the PMIx community
               development model does *not* PR bug fixes or new
               features directly to release branches.  Instead,
               initial bug-fix / feature PRs are generally first made
               to ``master``.

               This helps us ensure that future releases (with
               ``master`` as a Git ancestor) will contain the bug fix /
               feature.

For example:

.. code:: sh

   shell$ git checkout master
   shell$ git pull --rebase
   shell$ git checkout pr/bug-fix

   # ... make changes / fix a bug / etc. ...

   shell$ git add ...
   shell$ git commit -s ...
   shell$ git push myfork

At this point, you go create a PR from your fork's ``pr/bug-fix``
branch to the PMIx community GitHub repo ``master`` branch.  Work
with the community to get the PR completed and merged.  Then you can
open a new PR to cherry pick the Git commits from that bug fix to each
of the relevant release branches.

Depending on how far the release branch has diverged from ``master``,
there may be some porting effort involved in the cherry-pick.

For example, if your bug fix on ``master`` is comprised of a single Git
commit hash ``123abc``:

.. code:: sh

   # Fetch all upstream git activity, including the merge of the "master" PR.
   shell$ get fetch --all

   # Check out the target release branch, and advance to the most recent commit.
   shell$ git checkout v5.0.x
   shell$ git pull --rebase

   # Make a branch for your bug fix
   shell$ git checkout -b pr/v5.0.x/bug-fix
   # Cherry pick the commit from the "master" branch
   shell$ git cherry-pick -x 123abc
   # Push to your fork
   shell$ git push myfork

The PMIx development community *requires* adding the following
line to the commit message of cherry-picked commits on release
branches:

.. code:: text

   (cherry picked from commit [git_hash_of_original_commit])

.. note:: Note the use of the ``-x`` option to ``git cherry-pick``.
          This option automatically adds the ``(cherry picked from
          ...)`` line to your commit message.

.. admonition:: Rationale
   :class: tip

   Git does not actually store any meta data about Git cherry-picks in
   the commit.  Having a standardized text line containing the source
   Git commit hash in the commit messages helps the PMIx
   development community track where commits came from on release
   branches, and therefore allows us to check whether all relevant
   commits have been ported to a given release branch.

Once your commits are ready and pushed up to your fork, make a PR to
the target release branch.

.. warning:: A GitHub PR CI job checks all commits on release branches
             for the ``(cherry picked from...)`` line. It will also
             ensure that the Git hash cited in that line actually
             exists on the ``master`` branch.

             This check ensures that commits are not made to release
             branches before their corresponding ``master`` PR was
             merged.

All this being said, sometimes there is a need for a non-cherry-picked
commit on a release branch. E.g., sometimes a release branch has
diverged so much that the bug no longer exists on ``master``.  It would
therefore not make sense |mdash| or even be impossible |mdash| to
commit the bug fix in question to ``master``.

In such cases, make a regular PR to the target branch (with commits
that do *not* include ``(cherry picked from ...)`` lines).  In the PR
description, add a line with the following token:

.. code:: text

   bot:notacherrypick

This tells the GitHub CI job that this PR contains commits that are
not cherry-picked from ``master``.

.. warning:: ``bot:notacherrypick`` should only be used when
             absolutely necessary.  It is not a license to avoid
             the process of PR'ing to ``master`` first.

CI (testing)
------------

The PMIx community generally runs a bunch of tests on each PR
(Continuous Integration / CI).  These tests are a mixture of
GitHub Actions and other CI systems (e.g., Jenkins).  Examples
include (but are not limited to):

   * Check each Git commit for bozo email addresses
   * Check that each Git commit contains a ``Signed-off-by`` line
   * Check that commits on release branches contain a cherry-pick
     notice
   * Build and publish the docs
   * Build PMIx in a variety of environments and run sanity tests
     with that installation
