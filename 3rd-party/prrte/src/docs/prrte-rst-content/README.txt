This file covers two main topics:

1. How downstream projects should use PRRTE's RST files
2. How PRRTE developers should maintain these RST files


How downstream projects should use PRRTE's RST files
====================================================

The intent is that PRRTE will install some of its RST files in the
install tree so that downstream projects can use the RST
".. include::" directive to include them directly in their own RST
documentation.

Overall scheme
--------------

The overall scheme is relatively straightfoward:

1. Downstream projects should sym link or copy the PRRTE install
   directory $datadir/rst/prrte-rst-content (typically
   $prefix/share/prte/rst/prrte-rst-content) to the top directory of
   their RST doc tree.

2. Where relevant, use the RST include directive using the absolute
   path form:

   .. include:: /prrte-rst-content/<FOO>.rst

   where <FOO> is the name of whichever RST file you want to include.

   The RST files use the following RST block indicators in this order:

   Chapter: ===
   Section: ---
   Subsection: ^^^
   Subsubsection: +++

   It is advisable to start a new RST block immediately after
   including the public PRRTE RST files in order to precisely control
   the document section level after the PRRTE content.

3. Specific components may also install RST files in the same
   $datadir/rst tree (e.g., schizo components).  These RST files are
   suitable for inclusion in downstream project documentation; consult
   their guidance for usage.

   Note that PRRTE component RST filenames are not standardized by
   PRRTE, but they follow the same "use the absolute path form"
   convention as the public PRRTE RST files.


Sphinx pickyness
----------------

Sphinx is notably picky about two things:

1. How include paths propagate through files (and included files, and
   files that are included from included files, ... etc.).

   This is why it is recommended that downstream projects have
   "prrte-rst-content" at the top of their doc tree and then include
   with the absolute path form.  Attempting to use relative paths
   *might* be able to work, but is not advisable.

2. That every *.rst file in the doc tree is used (by default).

   The "prrte-rst-content" directory contains a bunch of individual
   *.rst files.  If downstream project documentation ends up only
   using *some* of those files, Sphinx will complain about the *.rst
   files that are not used.

   In such cases, downstream projects can either remove unneeded files
   (which may not be entirely safe across multiple different versions
   of PRRTE), or add the "prrte-rst-content" directory to their
   conf.py's exclude_patterns list.  Adding "prrte-rst-content" to
   exclude_patterns will prevent Sphinx from searching that entire
   directory for un-reference *.rst files (either via a TOC directive
   or the include:: directive).  Note that listing "prrte-rst-content"
   in exclude_patterns does *not* prevent explicitly include::'ing any
   files from that directory.


How PRRTE developers should maintain these RST files
====================================================

There are two types of files in this directory:

1. Files that are intended to be directly included by downstream
   project documentation.

   As described in the "How downstream projects should use PRRTE's RST
   files" section in this file, any files in this directory should
   include other PRRTE RST files using the absolute path form, and
   assume that /prrte-rst-content is a top-level directory in the RST
   doc tree.

   NOTE: The absolute path represents the root of the RST tree (not
   the root of the overall filesystem).

2. Files that are included (directly or indirectly) by the files from
   #1.

   They are meant to be self-contained chunks of information that can
   be included like building blocks in higher-level RST documentation
   files.

   a. Be wary of defining sections / subsections, especially for the
      "lowest" level files (that may be included multiple times in
      multiple places).  When creating new RST blocks, follow the RST
      block indicator sequence listed earlier in this file.

   b. Be ware of creating RST labels, especially for the "lowest"
      level files (that may be included multiple times in multiple
      places).
