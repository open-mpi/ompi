.. _pdb-label:

Inspecting Hung MPI Jobs with the ``pdb`` Helper
================================================

Open MPI ships a small, dependency-free shell tool,
``contrib/pdb``, that attaches a serial debugger (``gdb`` or
``lldb``) to *every* local process of an MPI job owned by the current
user, then drops you into a single interactive command loop that drives
all of those debuggers at once.  It is intended for the common
"my job is stuck |mdash| where is everybody?" situation: instead of
attaching to ranks one at a time, you collect and compare all of the
stacks together, and you can run a few Open-MPI-aware inspection
commands that decode internal request and shared-memory state.

The tool is deliberately written in portable POSIX ``sh`` and does not
require Open MPI to be installed; it only needs a supported debugger in
your ``PATH``.

Running the tool
----------------

The general form is:

.. code-block:: sh

   shell$ contrib/pdb [-d debugger] [--dump filename] [-p|--pids pid[,pid]...] [program-name]

``program-name``
   The basename of the executable to find.  ``pdb`` looks for local
   processes owned by the current user whose executable basename matches
   ``program-name``, resolves them to PIDs, attaches a debugger to each,
   and selects all of them.  If neither ``program-name`` nor ``--pids``
   is given, ``pdb`` starts unattached and you can use the ``attach``
   command later.

``-d debugger``
   Use a specific debugger module by name (for example, ``-d gdb`` or
   ``-d lldb``).  The default is ``auto``, which uses the first available
   debugger module.

``--dump filename``
   Before entering the interactive loop, save the process list and the
   raw debugger backtraces to ``filename`` as a simple text archive.
   This requires an initial ``program-name`` or ``--pids`` PID list.

``-p pid[,pid]...``, ``--pids pid[,pid]...``
   Attach to an explicit list of PIDs instead of searching by program
   name (``-p`` and ``--pids`` are equivalent).  The PIDs are passed
   straight to the ``attach`` command.  The list may be comma- or
   space-separated, and the option may be given more than once.  ``-p`` /
   ``--pids`` and ``program-name`` are mutually exclusive.  This is useful
   when several ranks share an executable name but you only want a subset,
   or when the processes you care about do not all share a basename.  For
   example:

   .. code-block:: sh

      shell$ contrib/pdb --pids 140122,140123,140124
      shell$ contrib/pdb -p 140122 -p 140123           # repeated also works

``-h``, ``--help``
   Print usage and exit.

For example, to attach to every local ``my_app`` process and start
exploring:

.. code-block:: sh

   shell$ contrib/pdb my_app
   Attached gdb to PIDs: 140122 140123 140124 140125
   ...
   attach[140122..140125(4)]> backtrace

The prompt has the form ``<PROMPT>[<selection>]>``.  The ``<selection>``
part is the *compressed PID list* of the currently *selected* PIDs (the
targets of the next command): a short list is shown verbatim (for
example, ``140122,140123``), a long list is summarized as
``first..last(count)``, and ``none`` means nothing is selected.

The ``<PROMPT>`` part is held in the shell variable ``PROMPT`` and
defaults to ``ompi-stack``.  When you ``attach``, it becomes ``attach``
(so the prompt reads ``attach[<selected-pids>]``); when the last debugger
is detached it returns to ``ompi-stack``.  Commands can change the prompt
for other contexts too |mdash| see :ref:`Changing the prompt
<pdb-prompt-label>` in the command-author guide below.

How the command loop works
~~~~~~~~~~~~~~~~~~~~~~~~~~~

At the prompt, the first word is the command name and the rest are its
arguments.  Name resolution happens in this order:

#. ``quit`` / ``exit`` leaves the loop, and ``help`` prints the command
   list.
#. ``<command> help`` prints the detailed help for that command.
#. If the first word matches a file in the command directory, that
   command is executed.
#. Otherwise, the entire line is sent verbatim to every selected
   debugger as a *passthrough* command, and the raw output is printed per
   PID.

The passthrough fallback means you can issue native ``gdb``/``lldb``
commands (for example, ``print some_global``) without leaving ``pdb``;
they simply run in all selected debuggers at once.

.. _pdb-commands-label:

Existing commands
-----------------

The commands below are the built-in commands shipped under
``contrib/pdeb/commands/``.  In all cases, ``<command> help`` prints
more detailed usage at the prompt.

Session and selection
~~~~~~~~~~~~~~~~~~~~~~

``attach <pid> [pid...]``
   Attach one debugger to each of the given PIDs and select all of them
   (the PIDs do not have to share an executable name).  If debuggers are
   already attached, they are detached first.  On success the prompt
   becomes ``attach[<selected-pids>]``.  To attach by program name, pass
   the name on the ``pdb`` command line (or via ``-p`` / ``--pids`` for
   explicit PIDs); ``pdb`` resolves a program name to PIDs before calling
   ``attach``.

``detach [all]``
   Detach the *selected* debuggers (the default), or every attached
   debugger with ``detach all``.  Detached debuggers are sent ``detach``
   and ``quit``, their feeders are stopped, and they are removed from the
   attached set.  When the last debugger is detached the ``attach`` prompt
   is removed; otherwise the prompt's PID list updates to the PIDs that
   remain selected.

``select all`` / ``select <pid> [pid...]``
   Choose which attached PIDs receive subsequent ``backtrace``,
   ``frame``, ``do``, and passthrough commands.  The initial selection is
   all attached PIDs.

``show pids``
   Print all attached PIDs and the currently selected PIDs.

Stacks and frames
~~~~~~~~~~~~~~~~~

``backtrace [raw] [file <filename>]``
   Collect a backtrace from every selected debugger.  By default the
   output is parsed into a report that shows the *longest common
   contiguous* stack shared by all parsed PIDs, followed by the frames
   that differ per PID and the trimmed per-PID stacks |mdash| this makes
   it easy to see at a glance whether all ranks are stuck in the same
   place.  ``raw`` prints the unparsed debugger output per PID instead,
   and ``file <filename>`` writes the output to a file rather than
   ``stdout``.

``frame <function>``
   In each selected debugger, find the first frame in the current thread
   whose backtrace line contains ``<function>`` and select that frame.
   This is handy for jumping all debuggers to the same routine before
   inspecting locals.

``do``
   Read debugger commands from your terminal until two consecutive empty
   lines, then run the whole block in every selected debugger and print
   the raw output per PID.  Use this when you want to send several native
   debugger commands as one batch.

Open MPI internal state
~~~~~~~~~~~~~~~~~~~~~~~

These commands decode Open MPI internal data structures.  They are only
available when the active debugger module implements them (currently
``gdb``), and they require debug symbols for the Open MPI libraries.

``req_dump [<requests_ptr> <count>]``
   Report every non-completed request for each selected PID.  With no
   arguments, the command locates the active
   ``ompi_request_default_wait_all`` frame and uses its ``requests`` and
   ``count`` locals.  With arguments, ``requests_ptr`` and ``count`` are
   evaluated by the debugger.

``pml_dump [<start> <count>]``
   Decode the OB1 PML global pending lists and selected send-request
   fields for each selected PID.  With no arguments, the first few
   incomplete requests in the active ``wait_all`` frame are shown; with
   arguments, requests in the range ``[start, start + count)`` are shown.

``btlsm_dump``
   Inspect the shared-memory BTL state for each selected PID: the
   incoming FIFO contents, fragment free-list allocation counters,
   per-endpoint pending-send state, and the pending-endpoint list length.

Built-in commands
~~~~~~~~~~~~~~~~~

In addition to the files above, the loop itself provides ``help``,
``<command> help``, and ``quit`` (also spelled ``exit``).

.. _pdb-writing-commands-label:

Writing new commands
--------------------

``pdb`` is intentionally extensible: every command is just a file in the
command directory, and the tool discovers commands by listing that
directory.  You do not edit the ``pdb`` script itself to add a command.

Where commands live
~~~~~~~~~~~~~~~~~~~

Commands are loaded from ``contrib/pdeb/commands/``.  The directory can
be overridden with the ``PDEB_COMMAND_DIR`` environment variable, which
is convenient while developing a command out of tree:

.. code-block:: sh

   shell$ PDEB_COMMAND_DIR=$HOME/my-pdb-commands contrib/pdb my_app

The file's basename is the command name typed at the prompt, so a file
named ``mycmd`` is invoked as ``mycmd``.  Files whose names begin with a
dot are treated as shared helpers: they are *sourced once* at startup
(before any command runs), which is the place to put functions shared by
several commands.

Anatomy of a command file
~~~~~~~~~~~~~~~~~~~~~~~~~

A command file is a POSIX ``sh`` fragment that is sourced on demand.  It
may define up to four functions:

``help`` (recommended)
   Print a one-line summary, indented by two spaces, for the ``help``
   listing.  If absent, only the command name is shown.

``detailed_help`` (recommended)
   Print multi-line usage for ``<command> help``.  If absent, ``pdb``
   falls back to ``help``.

``run`` (**required**)
   The command body.  It receives the command's arguments as ``"$@"``.
   A non-zero return aborts the command.

``output`` (optional)
   Custom formatting of debugger output; see below.

To keep the namespace clean, each of these well-known function names is
unset before a command is sourced and after it finishes, so commands do
not leak definitions into each other.  Any *other* function or variable
your command defines should be prefixed to avoid clashing with the
infrastructure (the internal helpers use the ``pdeb_`` prefix).

A minimal command looks like this:

.. code-block:: sh

   #
   # Copyright (c) 2026      Your Institution.  All rights reserved.
   # $COPYRIGHT$
   #
   # Additional copyrights may follow
   #
   # $HEADER$
   #

   help()
   {
       printf "  hello\n"
   }

   detailed_help()
   {
       cat <<EOF
   hello
     Print a greeting and the list of selected PIDs.
   EOF
   }

   run()
   {
       require_selection || return 1

       if [ 0 -lt $# ] && [ "help" = "$1" ] ; then
           detailed_help
           return 0
       fi

       printf "Hello from pdb; selected PIDs:%s\n" "$(printf " %s" $selected_pids)"
   }

Two kinds of commands
~~~~~~~~~~~~~~~~~~~~~

There are two ways a command can do its work:

#. **Drive the debuggers yourself.**  Your ``run`` function does
   everything and prints its own output, like the ``frame`` command.
   This is the most flexible style and is appropriate when the logic is
   per-PID and sequential.

#. **Let the infrastructure run a debugger command block.**  If ``run``
   sets the variable ``pdeb_debugger_commands`` to a block of debugger
   commands, the infrastructure runs that block once per *selected* PID
   after ``run`` returns, collecting each PID's output into a file.  This
   is how ``backtrace``, ``req_dump``, ``pml_dump``, and ``btlsm_dump``
   work.  Set ``pdeb_output_dir`` to a unique directory for the captured
   output (a common idiom is
   ``"$tmpdir/<name>.$$.$(date +%s)"``).

When you use the second style and want to format the result yourself,
define an ``output`` function.  It is called with one raw-output file
path per selected PID, in selected-PID order, and the matching PID list
is available in ``pdeb_output_pids``.  If you do not define ``output``,
each PID's raw output is printed under a ``PID <pid>:`` header.

Infrastructure available to commands
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Because commands are sourced into the running ``pdb`` process, they can
call its helper functions and read its state variables directly.  The
most useful ones are:

State variables
   * ``pids`` |mdash| all attached PIDs.
   * ``selected_pids`` |mdash| the PIDs the command should act on.
   * ``program_base`` |mdash| basename of the attached program.
   * ``debugger`` |mdash| name of the active debugger module.
   * ``tmpdir`` |mdash| per-session scratch directory (cleaned up on
     exit).

Helper functions
   * ``require_selection`` |mdash| return non-zero (with a message) if no
     PIDs are selected; call this first in commands that act on PIDs.
   * ``pdeb_function_defined <name>`` |mdash| test whether a function
     (for example, an optional debugger hook) exists.
   * ``run_debugger_commands <pid> <commands> <output-file>`` |mdash| run
     a block of debugger commands in one PID's debugger and capture the
     output.
   * ``ensure_debugger`` |mdash| make sure a debugger module is loaded and
     available.
   * ``prompt_push <text>`` / ``prompt_pop`` |mdash| change the interactive
     prompt for the current context; see below.
   * ``compressed_pids <pid...>`` |mdash| format a PID list compactly (a
     short list verbatim, a long one as ``first..last(count)``); used to
     render the selected PIDs inside the prompt's brackets.

.. _pdb-prompt-label:

Changing the prompt
~~~~~~~~~~~~~~~~~~~

The text before the ``[<selection>]>`` part of the prompt is held in the
shell variable ``PROMPT`` (default ``ompi-stack``).  A command that
enters a sub-mode |mdash| for example, a nested read loop that collects
its own input |mdash| can change the prompt so the user knows where they
are, then restore it on exit.

The built-in ``attach`` and ``detach`` commands use this mechanism: an
``attach`` pushes the ``attach`` label (so the prompt reads
``attach[<selected-pids>]``), and detaching the last debugger pops it.

``PROMPT`` is backed by a stack with two accessors:

   * ``prompt_push <text>`` |mdash| save the current ``PROMPT`` and set it
     to ``<text>``.
   * ``prompt_pop`` |mdash| restore the most recently pushed ``PROMPT``.
     It returns non-zero (and leaves ``PROMPT`` unchanged) if the stack is
     empty.

Always pair every ``prompt_push`` with a ``prompt_pop`` so the prompt is
restored even on early exits:

.. code-block:: sh

   run()
   {
       prompt_push "mycmd"

       # ... interact with the user in a sub-mode ...

       prompt_pop
   }

You may also set ``PROMPT`` directly for a permanent change, but using
the push/pop pair keeps contexts properly nested.

Using the active debugger portably
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To stay portable across ``gdb`` and ``lldb``, do not hard-code debugger
syntax.  Instead, ask the active debugger module to produce the right
command text through its ``debugger_*`` functions, which live in
``contrib/pdeb/debugger/`` (overridable with ``PDEB_DEBUGGER_DIR``).
For example, ``debugger_backtrace_command`` returns the
"backtrace all threads" command for the current debugger, and
``debugger_select_frame_command <n>`` returns the "select frame" command.

If your command relies on a debugger feature that only some modules
implement (as the Open-MPI-aware dumps do), guard it with
``pdeb_function_defined`` and fail gracefully:

.. code-block:: sh

   if ! pdeb_function_defined debugger_my_dump_command ; then
       echo "mycmd is not supported by debugger: $debugger" >&2
       return 1
   fi

   pdeb_debugger_commands=$(debugger_my_dump_command)
   pdeb_output_dir="$tmpdir/mycmd.$$.$(date +%s)"

You would then add a matching ``debugger_my_dump_command`` to each
debugger module under ``contrib/pdeb/debugger/`` that can support it.
Adding a whole new debugger is the same idea at a larger scale: drop a
new file in the debugger directory that implements the ``debugger_*``
contract (``debugger_available``, ``debugger_start``,
``debugger_backtrace_command``, ``debugger_parse_stack``, and so on),
modeled on the existing ``gdb`` and ``lldb`` modules.
