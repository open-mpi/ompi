.. _man1-pps:

pps
=========

.. include_body

pps |mdash| report identity of jobs running
in the system


SYNOPSIS
--------

``pps [options]``


DESCRIPTION
-----------

``pps`` provides the namespace of jobs currently executing
in the system.


OPTIONS
-------

``pps`` accepts the following options:

* ``-h`` | ``--help <arg0>``: Show help message. If the optional
  argument is not provided, then a generalized help message similar
  to the information provided here is returned. If an argument is
  provided, then a more detailed help message for that specific
  command line option is returned.

* ``-v`` | ``--verbose``: Enable debug output.

* ``-V`` | ``--version``: Print version and exit.

* ``--uri <arg0>``: Specify the URI of the server to which we are to connect, or the name of the file (specified as file:filename) that contains that info

* ``--namespace <arg0>``: Namespace of the daemon to which we should connect

* ``--pid <arg0>``: PID of the daemon to which we should connect (int => PID or file:<file> for file containing the PID

* ``--system-server-first``: First look for a system server and connect to it if found

* ``--system-server``: Connect to a system-level server

* ``--tmpdir <arg0>``: Set the root for the session directory tree

* ``--wait-to-connect <arg0>``: Delay specified number of seconds before trying to connect

* ``--num-connect-retries <arg0>``: Max number of times to try to connect

* ``--nodes``: Display Node Information


EXIT STATUS
-----------

Returns 0 for success, a non-zero error code otherwise.


EXAMPLES
--------

Examples of using this command.

.. seealso::
   :ref:`openpmix(5) <man5-openpmix>`
