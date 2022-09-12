GitHub, Git, and related topics
===============================

GitHub
------

Open MPI's Git repositories are `hosted at GitHub
<https://github.com/open-mpi/ompi>`_.

#. First, you will need a Git client. We recommend getting the latest
   version available. If you do not have the command ``git`` in your
   path, you will likely need to download and install Git.
#. `ompi <https://github.com/open-mpi/ompi/>`_ is the main Open MPI
   repository where most active development is done.  Git clone this
   repository.  Note that the use of the ``--recursive`` CLI option is
   necessary because Open MPI uses Git submodules::

      shell$ git clone --recursive https://github.com/open-mpi/ompi.git

Note that Git is natively capable of using many forms of web
proxies. If your network setup requires the user of a web proxy,
`consult the Git documentation for more details
<https://git-scm.com/>`_.

Git commits: open source / contributor's declaration
----------------------------------------------------

In order to remain open source, all new commits to the Open MPI
repository must include a ``Signed-off-by:`` line, indicating the
submitter's agreement to the :ref:`Open MPI Contributor's Declaration
<contributing-contributors-declaration-label>`.

.. tip:: You can use the ``-s`` option to ``git commit`` to
         automatically add the ``Signed-off-by:`` line to your commit
         message.

.. _git-github-branch-scheme-label:

Git branch scheme
-----------------

Generally, Open MPI has two types of branches in its Git repository:

#. ``main``:

   * All active development occurs on the ``main`` branch (new features,
     bug fixes, etc.).

#. Release branches of the form ``vMAJOR.MINOR.x`` (e.g., ``v4.0.x``,
   ``v4.1.x``, ``v5.0.x``).

   * The ``.x`` suffix indicates that this branch is used to create
     all releases in the Open MPI vMAJOR.MINOR series.
   * Periodically, the Open MPI community will make a new release
     branch, typically from ``main``.
   * A Git tag of the form ``vMAJOR.MINOR.RELEASE`` is used to
     indicate the specific commit on a release branch from where
     official Open MPI release tarball was created (e.g., ``v4.1.0``,
     ``v4.1.1``, ``v4.1.2``, etc.).

Once a bug is fixed or a new feature is implemented on ``main``, it is
cherry-picked over to the relevant release branch(es).

.. attention:: It may seem odd to some, but the Open MPI community
               development model does *not* PR bug fixes or new
               features directly to release branches.  Instead,
               initial bug-fix / feature PRs are generally first made
               to ``main``.

               This helps us ensure that future releases (with
               ``main`` as a Git ancestor) will contain the bug fix /
               feature.

For example:

.. code:: sh

   shell$ git checkout main
   shell$ git pull --rebase
   shell$ git checkout pr/bug-fix

   # ... make changes / fix a bug / etc. ...

   shell$ git add ...
   shell$ git commit -s ...
   shell$ git push myfork

At this point, you go create a PR from your fork's ``pr/bug-fix``
branch to the Open MPI community GitHub repo ``main`` branch.  Work
with the community to get the PR completed and merged.  Then you can
open a new PR to cherry pick the Git commits from that bug fix to each
of the relevant release branches.

Depending on how far the release branch has diverged from ``main``,
there may be some porting effort involved in the cherry-pick.

For example, if your bug fix on ``main`` is comprised of a single Git
commit hash ``123abc``:

.. code:: sh

   # Fetch all upstream git activity, including the merge of the "main" PR.
   shell$ get fetch --all

   # Check out the target release branch, and advance to the most recent commit.
   shell$ git checkout v5.0.x
   shell$ git pull --rebase

   # Make a branch for your bug fix
   shell$ git checkout -b pr/v5.0.x/bug-fix
   # Cherry pick the commit from the "main" branch
   shell$ git cherry-pick -x 123abc
   # Push to your fork
   shell$ git push myfork

The Open MPI development community *requires* adding the following
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
   Git commit hash in the commit messages helps the Open MPI
   development community track where commits came from on release
   branches, and therefore allows us to check whether all relevant
   commits have been ported to a given release branch.

Once your commits are ready and pushed up to your fork, make a PR to
the target release branch.

.. warning:: A GitHub PR CI job checks all commits on release branches
             for the ``(cherry picked from...)`` line. It will also
             ensure that the Git hash cited in that line actually
             exists on the ``main`` branch.

             This check ensures that commits are not made to release
             branches before their corresponding ``main`` PR was
             merged.

All this being said, sometimes there is a need for a non-cherry-picked
commit on a release branch. E.g., sometimes a release branch has
diverged so much that the bug no longer exists on ``main``.  It would
therefore not make sense |mdash| or even be impossible |mdash| to
commit the bug fix in question to ``main``.

In such cases, make a regular PR to the target branch (with commits
that do *not* include ``(cherry picked from ...)`` lines).  In the PR
description, add a line with the following token:

.. code:: text

   bot:notacherrypick

This tells the GitHub CI job that this PR contains commits that are
not cherry-picked from ``main``.

.. warning:: ``bot:notacherrypick`` should only be used when
             absolutely necessary.  It is not a license to avoid
             the process of PR'ing to ``main`` first.

CI (testing)
------------

The Open MPI community generally runs two flavors of testing:

#. A bunch of tests on each PR (Continuous Integration / CI).  These
   tests are a mixture of GitHub Actions and other CI systems (e.g.,
   Jenkins).  Examples include (but are not limited to):

   * Check each Git commit for bozo email addresses
   * Check that each Git commit contains a ``Signed-off-by`` line
   * Check that commits on release branches contain a cherry-pick
     notice
   * Build and publish the docs
   * Build Open MPI in a variety of environments and run sanity tests
     with that installation

#. Daily testing via the MPI Testing Tool (MTT).

   * These are generally tests that take much longer to run than on a
     per-PR basis.  `A "nightly snapshot" tarball
     <https://www.open-mpi.org/nightly/>`_ is created for ``main`` and
     each relevant release branch.
   * MTT tests are run with this snapshot tarball so that all
     organizations are testing with the same snapshots.
   * `Results are available here <https://mtt.open-mpi.org/>`_.
