.. _man1-plookup:

plookup
=========

.. include_body

plookup |mdash| lookup the value of keys posted using the ``PMIx_Publish`` API


SYNOPSIS
--------

``plookup [options] <KEY1>,<KEY2>,...``


DESCRIPTION
-----------

``plookup`` performs a ``PMIx_Lookup`` for each of the provided
keys and returns the results


OPTIONS
-------

``plookup`` accepts the following options:

* ``-h`` | ``--help <arg0>``: Show help message. If the optional
  argument is not provided, then a generalized help message similar
  to the information provided here is returned. If an argument is
  provided, then a more detailed help message for that specific
  command line option is returned.

* ``-v`` | ``--verbose``: Enable debug output.

* ``-V`` | ``--version``: Print version and exit.

* ``--pmixmca <arg0> <arg1>``: Set MCA parameter value

* ``--pid <arg0>``: PID of the daemon to which we should connect (int => PID or file:<file> for file containing the PID

* ``--tmpdir <arg0>``: Set the root for the session directory tree

* ``--timeout <arg0>``: Max number of seconds to wait for data to become available

* ``--wait``: Wait for data to become available if not immediately present


EXIT STATUS
-----------

Returns 0 upon success, or else a non-zero error code. Prints any returned values to stdout.


EXAMPLES
--------

Examples of using this command.

.. seealso::
   :ref:`openpmix(5) <man5-openpmix>`
