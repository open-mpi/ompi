Contributing to Open MPI
========================

There are many ways to contribute.  Here are a few:

#. Subscribe to `the mailing lists
   <https://www.open-mpi.org/community/lists/ompi.php>`_ and become
   active in the discussions.

#. Obtain `a Git clone <https://github.com/open-mpi/ompi/>`_ of Open
   MPI's code base and start looking through the code.

   .. note:: Be sure to see the :doc:`Developers guide
             </developers/index>` for technical details about the
             code base and how to build it).

#. Report bug fixes to the main code base.

   * First, ensure the bug was not already reported by searching
     `existing      GitHub Issues
     <https://github.com/open-mpi/ompi/issues>`_.
   * If you're unable to find an open issue addressing the problem,
     `open a new one <https://github.com/open-mpi/ompi/issues/new>`_.

#. Submit bug fixes to the main code base.

   * Awesome!  Open a new GitHub pull request with the patch.

     * Please submit bug fixes / new features on the ``main`` branch
       first, and then port them to the relevant release branch(es)
       after they have been accepted on ``main``.  :ref:`See this
       section for more information <git-github-branch-scheme-label>`.

   * Ensure the PR description clearly describes the problem and
     solution. If there is an existing GitHub issue open describing
     this bug, please include it in the description so we can track
     them together.
   * Be sure to see the :ref:`Open source contributions
     <contributing-open-source-label>` section, below.

#. Submit feature enhancements and/or new components to the main code
   base.

   * Awesome!  We love new ideas!
   * You might want to suggest your change on the `devel mail list
     <https://www.open-mpi.org/community/lists/ompi.php>`_ before you
     start writing code.  The `developer level technical information
     on the internals of Open MPI
     <https://www.open-mpi.org/faq/?category=developers>`_ may also be
     useful for large scale features.
   * If you're contributing a large new piece of functionality, that
     will be best viewed if you |mdash| or someone, anyone |mdash| is
     also stepping up to help maintain that functionality over time.
     We love new ideas and new features, but we do need to be
     realistic in what we can reliably test and deliver to our users.
   * Be sure to see the :ref:`Open source contributions
     <contributing-open-source-label>` section, below.

#. Submit fixes and/or entirely new content to this documentation.

   * Docs are great!  We always need help with documentation.
   * These docs are authored using ReStructured Text markup with the
     Sphinx rendering tool under the ``docs/`` directory in the
     repository.  This means you can submit a pull request with your
     docs updates, just like you would for any Open MPI code contribution.
   * :ref:`See this section <developers-installing-sphinx-label>` and
     :ref:`also this section <developers-rst-for-markdown-expats>` for
     information about how to install Sphinx locally and some
     high-level logistical instructions on how to write these docs.

#. Provide testing resources:

   #. For Github Pull Request Continuous Integration (CI)
   #. For nightly snapshot builds and testing

.. _contributing-open-source-label:

Open source contributions
-------------------------

All code contributions are submitted as pull requests on the `Open
MPI GitHub repository <https://github.com/open-mpi/ompi/>`_.

.. important:: All commits must include a ``Signed-off-by:`` line,
               indicating the submitter's agreement to the :ref:`Open
               MPI Contributor's Declaration
               <contributing-contributors-declaration-label>`.

.. _contributing-contributors-declaration-label:

Contributor's Declaration
^^^^^^^^^^^^^^^^^^^^^^^^^

In order to ensure that we can keep distributing Open MPI under our
:doc:`open source license </license/index>`, we need to ensure that
all contributions are compatible with that license.  Put differently:
we need to have an established intellectual property pedigree of the
code in Open MPI.  This means being able to ensure that all code
included in Open MPI is free, open source, and able to be distributed
under :doc:`the BSD license </license/index>`.

Open MPI has therefore adopted requirements based on the signed-off-by
process as described in Section 11 of the Linux kernel document on
`Submitting Patches
<https://www.kernel.org/doc/html/latest/process/submitting-patches.html#sign-your-work-the-developer-s-certificate-of-origin>`_.
Each proposed contribution to the Open MPI code base must include the
text ``Signed-off-by:`` followed by the contributor's name and email
address.

.. admonition:: Pro tip
   :class: tip

   You can use the ``-s`` flag to the ``git commit`` command (i.e.,
   ``git commit -s ...``) to automatically add the appropriate
   ``Signed-off-by:`` line to your commit message.

The ``Signed-off-by:`` line is a developer's certification that he or
she has the right to submit the patch for inclusion into the project,
and indicates agreement to the Developer's Certificate of Origin:

    By making a contribution to this project, I certify that:

    #. The contribution was created in whole or in part by me and I
       have the right to submit it under the :doc:`Open MPI open
       source license </license/index>`; or

    #. The contribution is based upon previous work that, to the best
       of my knowledge, is covered under an appropriate open source
       license and I have the right under that license to submit that
       work with modifications, whether created in whole or in part by
       me, under the :doc:`Open MPI open source license
       </license/index/>` (unless I am permitted to submit under a
       different license); or

    #. The contribution was provided directly to me by some other
       person who certified (1) or (2) and I have not modified it.

    #. I understand and agree that this project and the contribution
       are public and that a record of the contribution (including all
       personal information I submit with it, including my sign-off)
       is maintained indefinitely and may be redistributed consistent
       with this project and the open source license(s) involved.

Proposed contributions failing to include the ``Signed-off-by:``
certification will not be accepted into any Open MPI code
repository. The community reserves the right to revert any commit
inadvertently made without the required certification.

.. note:: This policy prevents a situation where intellectual property
          gets into the Open MPI code base and then someone later
          claims that we owe them money for it.  Open MPI is a free,
          open source code base.  We intend it to remain that way.

If you have not already done so, please ensure that *every* commit in
your pull request contains the ``Signed-off-by:`` line.

Git commit messages
^^^^^^^^^^^^^^^^^^^

Please write a good Git commit message, with a short first line
describing *what* was done, and then a description of *why* it was
done.

.. admonition:: Pro tip
   :class: tip

   `See this blog entry <https://cbea.ms/git-commit/>`_ for an
   excellent description of how to write a good Git commit message.

Code style
^^^^^^^^^^

There are a small number of style rules for Open MPI:

#. For all code:

   * 4 space tabs.  No more, no less.
   * No tab characters **at all**.  2 indentations are 8 spaces
     |mdash| not a tab.
   * m4 code is a bit weird in terms of indentation: we don't have a
     good, consistent indentation style in our existing code.  But
     still: no tab characters at all.

#. For C code:

   * We prefer if all blocks are enclosed in ``{}`` (even 1-line
     blocks).
   * We prefer that if you are testing equality with a constant, put
     the constant on the **left** of the ``==``.  E.g., ``if (NULL ==
     ptr)``.
   * If there are no parameters to a C function, declare it with
     ``(void)`` (vs. ``()``).

Closed source contributions
---------------------------

While we are creating free / open-source software, and we would prefer
if everyone's contributions to Open MPI were also free / open-source,
we certainly recognize that other organizations have different goals
from us.  Such is the reality of software development in today's
global economy.

As such, it is perfectly acceptable to make non-free / non-open-source
contributions to Open MPI.  We obviously cannot accept such
contributions into the main code base, but you are free to distribute
plugins, enhancements, etc. as you see fit.  Indeed, the :doc:`the BSD
license </license/index>` is extremely liberal in its redistribution
provisions.
