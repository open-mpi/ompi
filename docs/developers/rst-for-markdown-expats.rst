.. _developers-rst-for-markdown-expats:

ReStructured Text for those who know Markdown
=============================================

You can think of RST as "Markdown, but much better".  Meaning:

#. RST is basically just as simple as Markdown
#. But RST is both more precise, and has more available formatting
   constructs (without getting crazy complicated)

The full Sphinx / RST documentation is available here:
https://www.sphinx-doc.org/en/master/index.html

Translating Markdown to RST
---------------------------

If you're familiar with Markdown, here's some tips to get you started
in RST:

* Whitespace and indenting

  * MD: Whitespace and indenting generally doesn't matter in most
    cases.  It does matter with bullets and sub bullets, but the rules
    get pretty weird, and vary between different Markdown renderers.

  * RST: As noted above, **indenting matters**.  A lot.  Just like
    Python.  In general, you indent all RST text to keep it within the
    same level.  For example, all this text would be a single
    paragraph

    **Blank lines also matter**.  A lot.  You use blank lines to
    delimit sections within an indenting level.  For example, the
    blank line before this paragraph denotes a paragraph break.

    .. note:: RST was created by the Python community.  Hence,
              whitespace is quite important.

              * Indenting matters
              * Blank lines between content matter

  Using a blank line and outdenting indicates the end of the previous
  item.  For example, this paragraph is not part of the bulleted list.

* Fixed width font

  * MD: Use single quotes:

    .. code-block::

       `hello world`

  * RST: Use a pair of single quotes:

    .. code-block::

       ``hello world``

* Italics

  * MD: ``*hello world*`` or ``_hello world_``
  * RST: ``*hello world*``

* Boldface

  * MD: ``**hello world**``
  * RST: Same as MD

* Chapter and section delimiters

  * MD: Either use one or more pound signs (#, ##, ###) to the left of
    the line of text, or underline the line of text with pound signs

  * RST: Have a single line of text, underlined by non-ASCII
    characters.

      * The length of the underlying *must* be at least as long as the
        line of text
      * Which non-ASCII character is used for the underlying does not
        matter, but the order in which they are used denotes chapters
        / sections / subsections / etc.  In these OMPI docs, the
        underline characters we use are:

        .. code-block::

           Chapter 1: hello world
           ======================

        .. code-block::

           Section 1: hello world
           ----------------------

        .. code-block::

           Subsection 1: hello world
           ^^^^^^^^^^^^^^^^^^^^^^^^^

        .. code-block::

           Subsubsection 1: hello world
           ````````````````````````````

        Meaning: underlines made of = denotes chapters, underlines
        made of - denotes sections, and underlines made of ^ denotes
        subsections.

* Multi-line code/fixed-width font

  * MD: Use three single quotes to delimit blocks of text.  Optionally
    include a token keyword to denote the syntax highlighting to use
    inside that block.

    .. code-block::

       ```c
       int main() { printf("Hello world\n"); return 0 }
       ```

  * RST: Use ``.. code-block:: KEYWORD`` to start a section of code.

    .. code-block::

       .. code-block:: c

          int main() { printf("Hello world\n"); return 0 }

    * KEYWORD indicates which syntax highlighting to use (e.g., ``c``,
      ``c++`` ``make``, ``sh``, ``ini``, ``Fortran``, ``diff``, ``python``, ``java``,
      ... etc.).
    * KEYWORD can be omitted if no specific highlighting is to be
      used.
    * There *MUST* be a blank line after the ``code-block`` line.
    * The lines in the block must be indented to the same column as
      the ``c`` in ``code-block``.  For example:

      .. code-block::

         .. code-block:: sh

            shell$ tar xf openmpi-<version>.tar.bz2
            shell$ cd openmpi-<version>
            shell$ ./configure --prefix=<path> |& tee config.out

      Note that the code block will be rendered at the same level as
      where the first ``.`` of ``.. code-block::`` starts.  In this
      case, the example code block will be rendered in the bulleted
      item.

    Whereas this parargraph and code block will be outside of the
    above bulleted list:

    .. code-block:: sh

       shell$ tar xf openmpi-<version>.tar.bz2
       shell$ cd openmpi-<version>
       shell$ ./configure --prefix=<path> |& tee config.out

    The code-block can contain blank lines.

    The code-block is terminated by a blank line and then outdent back
    to the same level as the first ``.`` in ``.. code-block::``.

* Un-numbered bullets

  * MD: Start lines with ``*`` or ``-``
  * RST: Start lines with ``*``.  You can wrap lines at the same
    indenting level to make paragraphs in the same bullet.

    Having a blank line and then more text at the same indenting level
    makes another paragraph in the same bullet.  You even put other
    directives in this same indenting level.

    * For example, you can start a sub bullet.

      This text is the next paragraph in the same sub bullet.

      .. code-block::

         This is a verbatim code block within this same sub bullet.
         More about code-blocks below.

      This is the next paragraph (after the code block) in the same
      sub bullet.

  * If you start a new bullet, that terminates the previous bullet.

  You ***MUST*** put blank lines between bullets!

* Numbered bullets:

  * MD: Start lines with ``#``
  * RST: Start lines with ``#.``

    .. important:: Yes, the trailing ``.`` is important

    For example:

    .. code-block::

       #. Item number 1
       #. The second item
       #. A third item

    All the same rules for indentation apply as described above.

* Comments

  * MD: Enclose content in ``<!--`` and ``-->`` (i.e., HTML comments,
    but they are included in the output)
  * RST: Start a line with two periods and a space.

    For example, the following block is a comment, and will not be
    included in the output:

    .. code-block::

       .. Hello world.  This is a comment.  This whole block is a
          comment.  You can leave it here in the final document, and it
          will not be included in the rendered output.

          Your comment can even include blank lines.  You terminate a
          comment -- just like most other things in RST -- by a blank
          line and then outdenting back out to the same column as the
          first ".".

       This line is no longer part of the comment.

* Including files

  * MD: You cannot include files in Markdown.
  * RST: Use the ``.. include:: FILENAME`` directive.  For example:

    .. code-block::

      .. include:: features-extensions.rst
      .. include:: features-java.rst

    Those directives include those 2 files right here in this RST
    file.  Chapter/section/subsection delimiters will be continued in
    those files as part of rendering this file.

* Hyperlinks to URLs

  * MD:

    .. code-block::

       [this is the link text](https://example.com/)

  * RST:

    .. code-block::

       `this is the link text <https://example.com/>`_

    .. important:: Yes, the trailing underscore in RST is important.
                   It's a little weird, but you'll cope.

* Hyperlinks to anchors:

  * MD: I forget offhand how to make anchors and links to them in MD.
  * RST: Use the ``:ref:`` directive.

    Make an anchor like this:

    .. code-block::

       .. _ANCHOR_NAME:

    It *must* start with and underscore and end with a colon.

    I've typically used anchor names that end in ``-label`` to make it
    blatantly obvious that it's a label. For example:

    .. code-block::

       .. _building-and-installing-section-label:

    Then you can use the ``:ref:`` directive:

    .. code-block::

       be sure to see :ref:`the VPATH build section
       <building-and-installing-section-label>`.

* Hyperlinks to other (RST) pages

  * MD:

    .. code-block::

       (link text)[page_name]

  * RST: Use the ``:doc:`` directive.

    General format:

    .. code-block::

       :doc:`link text <PAGE_PATH>`

    For example:

    .. code-block::

       You should read :doc:`the Developer's Guide </developers>`.

    The page path is relative to the ``docs`` dir in the OMPI git tree.

* Macros

  * MD: There are no macros in Markdown.
  * RST: We have defined a few OMPI-specific macros in RST.  You can
    insert these macros anywhere in RST content text.

    ``|ompi_ver|`` is the full Open MPI version number, including
    alpha/beta/rc/greek denotation.  For example ``5.0.0rc1``.

    ``|ompi_series|`` is the major/minor Open MPI version, e.g.,
    ``5.0.x``.

    .. important:: Never hard-code the Open MPI version number or
                   series!  Always use the above macros.

    ``|mdash|`` is a unicode long dash, an "em" dash.  Use it instead
    of ``--``.

    ``|rarrow|`` is a unicode right arrow.  Use it instead of ``->``
    or ``-->``.

* Brightly-colored boxes.

  * MD: There are no brightly-colored boxes in MD.

  * RST: You can use various directives to make brightly-colored
    "note" boxes (Called admonitions) in RST.  For example:

    .. important:: a green box with a "!" icon

       Standard indenting rules apply for the content in the box.  You
       can have multiple lines and multiple paragraphs, for example.

       Yippee.

       * You can even have bullets.

         .. code-block::

            You can even have code blocks inside the bullet inside the
            caution box.

       * All the standard indenting rules apply.

    .. hint:: a green box with a "!" icon

    .. note:: a blue box with a "!" icon

    .. caution:: an orange box with a "!" icon

    .. attention:: an orange box with a "!" icon

    .. warning:: an orange box with a "!" icon

    .. error:: a red box with a "!" icon

    .. danger:: a red box with a "!" icon

    .. admonition:: Custom title
       :class: tip

       Custom text for this custom admonition.  Note that the ``:class: <type>``
       will change the coloring to the color for the basic admonition of that
       type.
