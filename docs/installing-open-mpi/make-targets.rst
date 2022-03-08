``make`` targets
================

Open MPI supports all the ``make`` targets that are provided by GNU
Automake, such as:

* ``all``: build the entire Open MPI package
* ``install``: install Open MPI
* ``uninstall``: remove all traces of Open MPI from the installation tree
* ``clean``: clean out the build tree

Once Open MPI has been built and installed, it is safe to run ``make
clean`` and/or remove the entire build tree.

Generally speaking, the only thing that users need to do to use Open
MPI is ensure that ``PREFIX/bin`` is in their ``PATH`` and
``PREFIX/lib`` is in their ``LD_LIBRARY_PATH``.  Users may need to
ensure to set the ``PATH`` and ``LD_LIBRARY_PATH`` in their shell
setup files (e.g., ``.bashrc``, ``.cshrc``) so that non-interactive
``ssh``-based logins will be able to find the Open MPI executables.
