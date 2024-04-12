.. _developers-installing-sphinx-label:

Installing and running Sphinx (building the OpenPMIx docs)
==========================================================

As with all content in the Developer's Guide, this section is only
relevant for developers who work in the OpenPMIx code base itself.
End users who install a binary OpenPMIx package or build from an
official OpenPMIx distribution tarball do not need to have Sphinx
installed.

Installing Python
-----------------

The `Sphinx tool <https://www.sphinx-doc.org/>`_ is written in Python,
and therefore needs to have Python available.  As of late 2022, Sphinx
requires Python >= v3.7.

This documentation does not contain detailed instructions for
installing a Python version sufficient for using Sphinx.  Consult your
local OS documentation for how to obtain Python >= v3.7, or search the
internet for further information.

Installing Sphinx
-----------------

`The Sphinx documentation
<https://www.sphinx-doc.org/en/master/usage/installation.html>`_
recommends installing Sphinx (and its required Python dependencies)
via ``pip``, which typically requires connectivity to the general
internet.

.. note:: If you are running on MacOS, you may be tempted to use
   Homebrew or MacPorts to install Sphinx.  The Sphinx documentation
   recommends **against** this.  Instead, you should use ``pip`` to
   install Sphinx.

There are three general ways to install Sphinx; you only need one of
them.

Install Sphinx in a Python virtual environment
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The preferred method of installing Sphinx for OpenPMIx documentation
development is to install Sphinx in a Python virtual environment.
This places Sphinx in a sandbox that will not conflict with other
``pip``-installed Python modules.  This example installs Sphinx and
other Python modules in the ``ompi-docs-venv`` tree under your OpenPMIx
Git clone directory:

.. code-block:: sh

   # Create the Python virtual environment
   shell$ cd TOP_OF_OpenPMIx_GIT_CLONE
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
^^^^^^^^^^^^^^^^^^^^^^^

If Python virtual environments are not desirable on your system, you
can install Sphinx globally on your system (you may need to run with
root privileges):

.. code-block:: sh

   shell$ cd TOP_OF_OpenPMIx_GIT_CLONE
   shell$ pip3 install -r docs/requirements.txt
   # Or: python3 -m pip install install -r docs/requirements.txt

This will install Sphinx and some Python modules required for building
the OpenPMIx documentation in a system-wide location.

This will likely install the ``sphinx-build`` executable in a location
that is already in your ``PATH``.  If the location is not already in
your ``PATH``, then you need to add it to your ``PATH``.

.. important:: You will need to ensure that Sphinx is in your ``PATH``
               *before* running ``configure``.


Install Sphinx locally
^^^^^^^^^^^^^^^^^^^^^^

If you cannot or do not want to install Sphinx globally on your
system, the following will install Sphinx somewhere under your
``$HOME``.  It is the same ``pip`` command as shown above, but with
the addition of the ``--user`` flag (you should not need ``root``
permissions to run this command):

.. code-block:: sh

   shell$ cd TOP_OF_OpenPMIx_GIT_CLONE
   shell$ pip3 install --user -r docs/requirements.txt
   # Or: python3 -m pip install install -r docs/requirements.txt

This will install Sphinx and some Python modules required for building
the OpenPMIx documentation in a system-wide location.

You will likely need to find the location where ``sphinx-build`` was
installed and add it to your ``PATH``.

.. note:: On MacOS, look for ``sphinx-build`` under
          ``$HOME/Library/Python/VERSION/bin`` (where ``VERSION`` is
          the version number of Python).  Or it may have installed to
          ``/usr/local/bin/sphinx-build``.  YMMV.

.. important:: You will need to ensure that Sphinx is in your ``PATH``
               *before* running ``configure``.

Running Sphinx
--------------

OpenPMIx's build environment is setup to invoke Sphinx automatically;
you should not need to invoke Sphinx manually.

.. important:: You will need to ensure that Sphinx is in your ``PATH``
               *before* running ``configure``.

As long as ``configure`` found Sphinx, ``make`` will invoke Sphinx to
build the documentation.  You can also run ``make`` directly in the
``docs/`` directory to build *just* the docs and skip building the
rest of the OpenPMIx software.  This can be a huge time-saver when
iteratively writing, rendering, and viewing/proofing documentation.

.. note:: The fully-built HTML and man page docs are included in
          official OpenPMIx distribution tarballs.  Meaning: if you
          download an official distribution OpenPMIx tarball,
          the pre-built HTML and man page files are included
          in the tarball.

          Sphinx is a requirement for *developers* who want to build
          the OpenPMIx docs.  End users do *not* need to have Sphinx
          available to build OpenPMIx or have its docs installed from
          an official distribution tarball.

Sphinx execution time
^^^^^^^^^^^^^^^^^^^^^

Sphinx is stateful: subsequent runs can be significantly faster
because Sphinx will only re-render HTML files that have changes.  This
is a nice time saver for OpenPMIx (e.g., if you are iterating over
writing the docs and running ``make`` to see how they rendered in
HTML).

.. caution:: Sphinx is only *somewhat* smart in its partial
             re-rendering.  If you change a title in an RST file, for
             example, Sphinx will (by default) only re-render *that*
             page.  The Tables of Contents / left hand navigation on
             other pages may not be updated.

             You can always force a full re-render via:

             .. code:: sh

                shell$ cd docs
                shell$ rm -rf _build
                shell$ make

Checking Sphinx HTML links
^^^^^^^^^^^^^^^^^^^^^^^^^^

``make linkcheck`` will invoke Sphinx's functionality to check all the
external links in the documentation:

.. code:: sh

   shell$ cd docs
   shell$ make linkcheck

.. important:: You will need to be on a computer that has good access
               to the internet when running this command.

Viewing docs locally
^^^^^^^^^^^^^^^^^^^^

Once you have built the docs in your local Git clone, you can view
them locally in the build tree:

#. Open ``docs/_build/html/index.html`` in a browser to view the HTML
   docs.  For example, on MacOS, the following command opens the build
   tree docs in the default web browser:

   .. code:: sh

      shell$ open docs/_build/html/index.html

#. Use the ``man`` command to view the Nroff files in
   ``docs/_build/man`` (you may need to use an absolute or relative
   filename to prevent ``man`` from using its search paths).  For
   example:

   .. code:: sh

      shell$ cd docs/_build/man
      shell$ man ./PMIx_Abort.3

Alternatively, you can view these files in their installed locations
after running ``make install``:

#. The HTML docs are installed (by default) to
   ``$prefix/share/doc/pmix/html``.  You can use a web browser to
   open the ``index.html`` in that directory to view the docs locally.
   For example, on MacOS, the following command opens the installed
   docs in the default web browser:

   .. code:: sh

      shell$ open $prefix/share/doc/pmix/html/index.html

#. The man pages are installed (by default) to ``$preix/share/man``.
   If your man page search path includes this location, you can invoke
   commands similar to the following to see the same content that you
   see in these HTML pages:

   .. code:: sh

      shell$ man PMIx_Abort
