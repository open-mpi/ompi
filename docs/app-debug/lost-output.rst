Application Output Lost with Abnormal Termination
=================================================

There many be many reasons for application output to be lost when
an application abnormally terminates. The Open MPI Team strongly
encourages the use of tools (such as debuggers) whenever possible.

One of the reasons, however, may come from inside Open MPI itself.  If
your application fails due to memory corruption, Open MPI may
subsequently fail to output an error message before terminating.
Open MPI attempts to aggregate error
messages from multiple processes in an attempt to show unique error
messages only once (vs. once for each MPI process |mdash| which can be
unwieldy, especially when running large MPI jobs).

However, this aggregation process requires allocating memory in the
MPI process when it displays the error message.  If the process's
memory is already corrupted, Open MPI's attempt to allocate memory may
fail and the process will simply terminate, possibly silently.  When Open
MPI does not attempt to aggregate error messages, most of its setup
work is done when the MPI library is initiaized  and no memory is allocated
during the "print the error" routine.  It therefore almost always successfully
outputs error messages in real time |mdash| but at the expense that you'll
potentially see the same error message for *each* MPI process that
encountered the error.

Hence, the error message aggregation is *usually* a good thing, but
sometimes it can mask a real error.  You can disable Open MPI's error
message aggregation with the ``opal_base_help_aggregate`` MCA
parameter.  For example:

.. code-block:: sh

   shell$ mpirun --mca opal_base_help_aggregate 0 ...
