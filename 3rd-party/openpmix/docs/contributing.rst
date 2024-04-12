Contributing to OpenPMIx
========================

Open source contributions
-------------------------

All code contributions are submitted as pull requests on the `OpenPMIx
GitHub repository <https://github.com/openpmix/openpmix/>`_.

.. important:: All commits must include a ``Signed-off-by:`` line,
               indicating the submitter's agreement to the :ref:`OpenPMIx
               Contributor's Declaration
               <contributing-contributors-declaration-label>`.

.. _contributing-contributors-declaration-label:

Contributor's Declaration
^^^^^^^^^^^^^^^^^^^^^^^^^

In order to ensure that we can keep distributing OpenPMIx under our
:doc:`open source license </license>`, we need to ensure that
all contributions are compatible with that license.  Put differently:
we need to have an established intellectual property pedigree of the
code in OpenPMIx.  This means being able to ensure that all code
included in OpenPMIx is free, open source, and able to be distributed
under :doc:`the BSD license </license>`.

OpenPMIx has therefore adopted requirements based on the signed-off-by
process as described in Section 11 of the Linux kernel document on
`Submitting Patches
<https://www.kernel.org/doc/html/latest/process/submitting-patches.html#sign-your-work-the-developer-s-certificate-of-origin>`_.
Each proposed contribution to the OpenPMIx code base must include the
text ``Signed-off-by:`` followed by the contributor's name and email
address. This is a developer's certification that he or she has the
right to submit the patch for inclusion into the project, and
indicates agreement to the Developer's Certificate of Origin:

    By making a contribution to this project, I certify that:

    #. The contribution was created in whole or in part by me and I
       have the right to submit it under the :doc:`OpenPMIx open
       source license </license>`; or

    #. The contribution is based upon previous work that, to the best
       of my knowledge, is covered under an appropriate open source
       license and I have the right under that license to submit that
       work with modifications, whether created in whole or in part by
       me, under the :doc:`OpenPMIx open source license
       </license>` (unless I am permitted to submit under a
       different license); or

    #. The contribution was provided directly to me by some other
       person who certified (1) or (2) and I have not modified it.

    #. I understand and agree that this project and the contribution
       are public and that a record of the contribution (including all
       personal information I submit with it, including my sign-off)
       is maintained indefinitely and may be redistributed consistent
       with this project and the open source license(s) involved.

Proposed contributions failing to include the ``Signed-off-by:``
certification will not be accepted into any OpenPMIx code
repository. The community reserves the right to revert any commit
inadvertently made without the required certification.

.. note:: This policy prevents a situation where intellectual property
          gets into the OpenPMIx code base and then someone later
          claims that we owe them money for it.  OpenPMIx is a free,
          open source code base.  We intend it to remain that way.

If you have not already done so, please ensure that *every* commit in
your pull request contains the ``Signed-off-by:`` line.

.. admonition:: Pro tip
   :class: tip

   You can use the ``-s`` flag to the ``git commit`` command (i.e.,
   ``git commit -s ...``) to automatically add the appropriate
   ``Signed-off-by:`` line to your commit message.
