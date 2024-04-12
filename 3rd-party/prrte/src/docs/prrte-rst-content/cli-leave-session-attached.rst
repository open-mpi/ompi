.. -*- rst -*-

   Copyright (c) 2022-2024 Nanook Consulting  All rights reserved.
   Copyright (c) 2023 Jeffrey M. Squyres.  All rights reserved.

   $COPYRIGHT$

   Additional copyrights may follow

   $HEADER$

.. The following line is included so that Sphinx won't complain
   about this file not being directly included in some toctree

Do not discard stdout/stderr of remote PRRTE daemons. The primary use
for this option is to ensure that the daemon output streams (i.e.,
stdout and stderr) remain open after launch, thus allowing the user to
see any daemon-generated error messages. Otherwise, the daemon will
"daemonize" itself upon launch, thereby closing its output streams.
