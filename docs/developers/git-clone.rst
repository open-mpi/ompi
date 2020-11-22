Obtaining a Git clone
=====================

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
