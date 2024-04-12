.. _man1-pattrs:

pattrs
=========

.. include_body

pattrs |mdash| Display information about supported attributes

SYNOPSIS
--------

``pattrs [options]``


DESCRIPTION
-----------

``pattrs`` provides information on the attributes supported by the
client, server, and tool interfaces of the PMIx library being used.


OPTIONS
-------

``pattrs`` accepts the following options:

* ``-h`` | ``--help <arg0>``: Show help message. If the optional
  argument is not provided, then a generalized help message similar
  to the information provided here is returned. If an argument is
  provided, then a more detailed help message for that specific
  command line option is returned.

* ``-v`` | ``--verbose``: Enable debug output.

* ``-V`` | ``--version``: Print version and exit.

* ``--pmixmca <arg0> <arg1>``: Set MCA parameter value

* ``--do-not-connect``: Do not connect to a server

* ``--uri <arg0>``: Specify the URI of the server to which we are to connect, or the name of the file (specified as file:filename) that contains that info

* ``--namespace <arg0>``: Namespace of the daemon to which we should connect

* ``--pid <arg0>``: PID of the daemon to which we should connect (int => PID or file:<file> for file containing the PID

* ``--system-server-first``: First look for a system server and connect to it if found

* ``--system-server``: Connect to a system-level server

* ``--tmpdir <arg0>``: Set the root for the session directory tree

* ``--wait-to-connect <arg0>``: Delay specified number of seconds before trying to connect

* ``--num-connect-retries <arg0>``: Max number of times to try to connect

* ``--client <arg0>``: Comma-delimited list of client functions whose attributes are to be printed (function or "all")

* ``--server <arg0>``: Comma-delimited list of server functions whose attributes are to be printed (function or "all")

* ``--tool <arg0>``: Comma-delimited list of tool functions whose attributes are to be printed (function or "all")

* ``--host <arg0>``: Comma-delimited list of host functions whose attributes are to be printed (function or "all")

* ``--client-fns``: List the functions supported in this client library

* ``--server-fns``: List the functions supported in this server library

* ``--tool-fns``: List the functions supported in this tool library

* ``--host-fns``: List the functions supported by this host environment


EXIT STATUS
-----------

Returns 0 for successful completion or a non-zero error code. The command outputs the requested information to stdout.


EXAMPLES
--------

Examples of using this command.

.. seealso::
   :ref:`openpmix(5) <man5-openpmix>`
