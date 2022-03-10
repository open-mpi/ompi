.. _developers-installing-sphinx-label:

Installing Sphinx
=================

The Sphinx documentation recommends installing Sphinx (and its
required Python dependencies) via ``pip``, which typically requires
connectivity to the general internet.

.. note:: If you are running on MacOS, you may be tempted to use
   Homebrew or MacPorts to install Sphinx.  The Sphinx documentation
   recommends **against** this.  Instead, you should use ``pip`` to
   install Sphinx.

There are three general ways to install Sphinx; you only need one of
them.

Install Sphinx in a Python virtual environment
----------------------------------------------

The preferred method of installing Sphinx for Open MPI documentation
development is to install Sphinx in a Python virtual environment.
This places Sphinx in a sandbox that will not conflict with other
``pip``-installed Python modules.  This example installs Sphinx and
other Python modules in the ``ompi-docs-venv`` tree under your Open
MPI Git clone directory:

.. code-block:: sh

   # Create the Python virtual environment
   shell$ cd TOP_OF_OPEN_MPI_GIT_CLONE
   shell$ python3 -m venv ompi-docs-venv
   # Or: python3 -m virtualenv ompi-docs-venv
   # Or: virtualenv --python=python3 ompi-docs-venv

   # Activate the virtual environment
   shell$ . ./ompi-docs-venv/bin/activate

   # Notice that the shell prompt changes
   # Now install the required Python modules
   (ompi-docs-venv) shell$ pip3 install -r docs/requirements.txt
   # Or: python3 -m pip install install -r docs/requirements.txt

Note that sourcing the ``activate`` script will change your prompt to
put the name of your virtual environment directory at the front, just
as a visual reminder that you are operating in a Python virtual
environment.  You can run ``deactivate`` to leave the virtual
environment.

.. important:: You will need to source the ``activate`` script to put
               Sphinx in your ``PATH`` *before* running ``configure``.

Install Sphinx globally
-----------------------

If Python virtual environments are not desirable on your system, you
can install Sphinx globally on your system (you may need to run with
root privileges):

.. code-block:: sh

   shell$ cd TOP_OF_OPEN_MPI_GIT_CLONE
   shell$ pip3 install -r docs/requirements.txt
   # Or: python3 -m pip install install -r docs/requirements.txt

This will install Sphinx and some Python modules required for building
the Open MPI documentation in a system-wide location.

This will likely install the ``sphinx-build`` executable in a location
that is already in your ``PATH``.  If the location is not already in
your ``PATH``, then you need to add it to your ``PATH``.

.. important:: You will need to ensure that Sphinx is in your ``PATH``
               *before* running ``configure``.


Install Sphinx locally
----------------------

If you cannot or do not want to install Sphinx globally on your
system, the following will install Sphinx somewhere under your
``$HOME``.  It is the same ``pip`` command as shown above, but with
the addition of the ``--user`` flag (you should not need ``root``
permissions to run this command):

.. code-block:: sh

   shell$ cd TOP_OF_OPEN_MPI_GIT_CLONE
   shell$ pip3 install --user -r docs/requirements.txt
   # Or: python3 -m pip install install -r docs/requirements.txt

This will install Sphinx and some Python modules required for building
the Open MPI documentation in a system-wide location.

You will likely need to find the location where ``sphinx-build`` was
installed and add it to your ``PATH``.

.. note:: On MacOS, look for ``sphinx-build`` under
          ``$HOME/Library/Python/VERSION/bin`` (where ``VERSION`` is
          the version number of Python).  Or it may have installed to
          ``/usr/local/bin/sphinx-build``.  YMMV.

.. important:: You will need to ensure that Sphinx is in your ``PATH``
               *before* running ``configure``.
