.. _man1-pevent:

pevent
=========

.. include_body

pevent |mdash| inject specified event into the system

SYNOPSIS
--------

``pevent [options] <EVENT>``


DESCRIPTION
-----------

``pevent`` requests that the specified event be passed to
processes in the given range.


OPTIONS
-------

``pevent`` accepts the following options:

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

* ``--range <arg0>``: Range of event to be sent. If not provided, the request will default to the ``PMIX_GLOBAL`` range


EXIT STATUS
-----------

Returns 0 if the event was successfully sent, a non-zero error code if otherwise.


EXAMPLES
--------

Examples of using this command.

.. seealso::
   :ref:`openpmix(5) <man5-openpmix>`
