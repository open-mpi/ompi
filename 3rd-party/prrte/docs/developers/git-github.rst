GitHub, Git, and related topics
===============================

GitHub
------

PRRTE's Git repositories are `hosted at GitHub
<https://github.com/openpmix/prrte>`_.

#. First, you will need a Git client. We recommend getting the latest
   version available. If you do not have the command ``git`` in your
   path, you will likely need to download and install Git.
#. `prrte <https://github.com/openpmix/prrte/>`_ is the main PRRTE
   repository where most active development is done.  Git clone this
   repository.  Note that the use of the ``--recursive`` CLI option is
   necessary because PRRTE uses Git submodules::

      shell$ git clone --recursive https://github.com/openpmix/prrte.git

Note that Git is natively capable of using many forms of web
proxies. If your network setup requires the user of a web proxy,
`consult the Git documentation for more details
<https://git-scm.com/>`_.

Git commits: open source / contributor's declaration
----------------------------------------------------

In order to remain open source, all new commits to the PRRTE
repository must include a ``Signed-off-by:`` line, indicating the
submitter's agreement to the :ref:`PRRTE Contributor's Declaration
<contributing-contributors-declaration-label>`.

.. tip:: You can use the ``-s`` option to ``git commit`` to
         automatically add the ``Signed-off-by:`` line to your commit
         message.

.. _git-github-branch-scheme-label:

Git branch scheme
-----------------

Generally, PRRTE has two types of branches in its Git repository:

#. ``main``:

   * All active development occurs on the ``main`` branch (new features,
     bug fixes, etc.).

#. Release branches of the form ``vMAJOR.MINOR.x`` (e.g., ``v4.0.x``,
   ``v4.1.x``, ``v5.0.x``).

   * The ``.x`` suffix indicates that this branch is used to create
     all releases in the PRRTE vMAJOR.MINOR series.
   * Periodically, the PRRTE community will make a new release
     branch, typically from ``main``.
   * A Git tag of the form ``vMAJOR.MINOR.RELEASE`` is used to
     indicate the specific commit on a release branch from where
     official PRRTE release tarball was created (e.g., ``v4.1.0``,
     ``v4.1.1``, ``v4.1.2``, etc.).
