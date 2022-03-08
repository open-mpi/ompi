Contributing to Open MPI
========================

There are many ways to contribute.  Here are a few:

#. Subscribe to `the mailing lists
   <https://www.open-mpi.org/community/lists/ompi.php>`_ and become
   active in the discussions.
#. Obtain `a Git clone <https://www.open-mpi.org/source/>`_ of Open
   MPI's code base and start looking through the code.

   .. note:: Be sure to see the :doc:`Developers guide
             </developers/index>` for technical details about the code
             base and how to build it).

#. Write your own components and contribute them back to the main code
   base.
#. Contribute bug fixes and feature enhancements to the main code
   base.
#. Provide testing resources:

   #. For Github Pull Request Continuous Integration (CI)
   #. For nightly snapshot builds and testing


.. _contributing-open-source-label:

Open source contributions
-------------------------

All code contributions are submitted as pull requests on the `Open MPI
GitHub repository <https://github.com/open-mpi/ompi/>`_.

We need to have an established intellectual property pedigree of the
code in Open MPI.  This means being able to ensure that all code
included in Open MPI is free, open source, and able to be distributed
under :doc:`the BSD license </license/index>`.

Open MPI has therefore adopted requirements based on the signed-off-by
process as described in the `Submitting patches
<https://www.kernel.org/doc/html/latest/process/submitting-patches.html#sign-your-work-the-developer-s-certificate-of-origin>`_
section of the Linux kernel documentation. Each proposed contribution
to the Open MPI code base must include the text "Signed-off-by:"
followed by the contributor's name and email address. This is a
developer's certification that he or she has the right to submit the
patch for inclusion into the project, and indicates agreement to the
Developer's Certificate of Origin:

   By making a contribution to this project, I certify that:

   #. The contribution was created in whole or in part by me and I
      have the right to submit it under the Open MPI open source
      license; or

   #. The contribution is based upon previous work that, to the best
      of my knowledge, is covered under an appropriate open source
      license and I have the right under that license to submit that
      work with modifications, whether created in whole or in part by
      me, under the Open MPI open source license (unless I am
      permitted to submit under a different license); or

   #. The contribution was provided directly to me by some other
      person who certified (1) or (2) and I have not modified it.

   #. I understand and agree that this project and the contribution
      are public and that a record of the contribution (including all
      personal information I submit with it, including my sign-off) is
      maintained indefinitely and may be redistributed consistent with
      this project and the open source license(s) involved.

Proposed contributions failing to include the "Signed-off-by:"
certification will not be accepted into any Open MPI code
repository. The community reserves the right to revert any commit
inadvertently made without the required certification.

This policy prevents a situation where intellectual property gets into
the Open MPI code base and then someone later claims that we owe them
money for it.  Open MPI is a free, open source code base.  We intend
it to remain that way.


Closed source contributions
---------------------------

While we are creating free / open-source software, and we would prefer
if everyone's contributions to Open MPI were also free / open-source,
we certainly recognize that other organizations have different goals
from us.  Such is the reality of software development in today's
global economy.

As such, it is perfectly acceptable to make non-free / non-open-source
contributions to Open MPI.

We obviously cannot accept such contributions into the main code base,
but you are free to distribute plugins, enhancements, etc. as you see
fit.  Indeed, the :doc:`the BSD license </license/index>` is extremely
liberal in its redistribution provisions.
