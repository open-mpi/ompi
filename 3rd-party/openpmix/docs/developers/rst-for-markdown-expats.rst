.. _developers-rst-for-markdown-expats:

ReStructured Text for those who know Markdown
=============================================

You can think of RST as "Markdown, but much better".  Meaning:

#. RST is basically just as simple as Markdown
#. But RST is both more precise, and has more available formatting
   constructs (without getting crazy complicated)

The full Sphinx / RST documentation is available here:
https://www.sphinx-doc.org/en/master/index.html

If you're familiar with Markdown, the following sections contain some
tips to get you started in RST.

Whitespace and indenting
------------------------

* MD: Whitespace and indenting generally doesn't matter in most
  cases.  It does matter with bullets and sub bullets, but the rules
  get pretty weird, and vary between different Markdown renderers.

* RST: **Indenting matters**.  A lot.  Just like Python.  In
  general, you indent all RST text to keep it within the same level.
  For example, all this text would be a single paragraph

  **Blank lines also matter**.  A lot.  You use blank lines to
  delimit sections within an indenting level.  For example, the
  blank line before this paragraph denotes a paragraph break.

  .. note:: RST was created by the Python community.  Hence,
            whitespace is quite important.

            * Indenting matters
            * Blank lines between content matter

Using a blank line and outdenting indicates the end of the previous
item.  For example, this paragraph is not part of the MD/RST
bulleted list.

Fixed width font
----------------

* MD: Use single quotes:

  .. code-block:: md

     `hello world`

* RST: Use a pair of single quotes:

  .. code-block:: rst

     ``hello world``

Italics
-------

* MD: ``*hello world*`` or ``_hello world_``
* RST: ``*hello world*``

Boldface
--------

* MD: ``**hello world**``
* RST: Same as MD

Chapter and section delimiters
------------------------------

* MD: Either use one or more pound signs (#, ##, ###) to the left of
  the line of text, or underline the line of text with pound signs

* RST: Have a single line of text, underlined by non-ASCII
  characters.

  * The length of the underlying *must* be at least as long as the
    line of text
  * Which non-ASCII character is used for the underlying does not
    matter, but the order in which they are used denotes chapters
    / sections / subsections / etc.

    In these OpenPMIx docs, the sequence of underline characters we use
    are:

    .. code-block:: rst

       Chapter 1: hello world
       ======================

    .. code-block:: rst

       Section 1: hello world
       ----------------------

    .. code-block:: rst

       Subsection 1: hello world
       ^^^^^^^^^^^^^^^^^^^^^^^^^

    .. code-block:: rst

       Subsubsection 1: hello world
       ++++++++++++++++++++++++++++

    Meaning: underlines made of ``=`` denotes chapters, underlines
    made of ``-`` denotes sections, underlines made of ``^`` denotes
    subsections, and underlines made of ``+`` denote subsubsections.

Multi-line code/fixed-width font
--------------------------------

* MD: Use three single quotes to delimit blocks of text.  Optionally
  include a token keyword to denote the syntax highlighting to use
  inside that block.

  .. code-block:: md

     ```c
     int main() { printf("Hello world\n"); return 0 }
     ```

* RST: Use ``.. code-block:: KEYWORD`` to start a section of code.

  .. code-block:: rst

     .. code-block:: c

        int main() { printf("Hello world\n"); return 0 }

  * KEYWORD indicates which syntax highlighting to use (e.g., ``c``,
    ``c++`` ``make``, ``sh``, ``ini``, ``Fortran``, ``diff``,
    ``python``, ``java``, ``rst``, ... etc.).
  * KEYWORD can be omitted if no specific highlighting is to be
    used.
  * There *MUST* be a blank line after the ``code-block`` line.
  * The lines in the block must be indented to the same column as the
    first ``c`` in ``code-block``.  For example:

    .. code-block:: rst

       .. code-block:: sh

          shell$ tar xf pmix-<version>.tar.bz2
          shell$ cd pmix-<version>
          shell$ ./configure --prefix=<path> |& tee config.out

    Note that the code block will be rendered at the same level as
    where the first ``.`` of ``.. code-block::`` starts.  In this
    case, the example code block will be rendered in the bulleted
    item.

Whereas this parargraph and code block will be outside of the
above bulleted list:

.. code-block:: sh

   shell$ tar xf pmix-<version>.tar.bz2
   shell$ cd pmix-<version>
   shell$ ./configure --prefix=<path> |& tee config.out

   # Fun note: the code-block can contain blank lines.

The code-block is terminated by a blank line and then outdent back
to the same level as the first ``.`` in ``.. code-block::``.

Un-numbered bullets
-------------------

* MD: Start lines with ``*`` or ``-``
* RST: Start lines with ``*``.  You can wrap lines at the same
  indenting level to make paragraphs in the same bullet.

  Having a blank line and then more text at the same indenting level
  makes another paragraph in the same bullet.  You even put other
  directives in this same indenting level.

  * For example, you can start a sub bullet.

    This text is the next paragraph in the same sub bullet.

    .. code-block:: none

       This is a verbatim code block within this same sub bullet.
       More about code-blocks below.

    This is the next paragraph (after the code block) in the same
    sub bullet.

  * If you start a new bullet, that terminates the previous bullet.

  * You **MUST** put blank lines between bullets!

Numbered bullets:
-----------------

* MD: Start lines with ``#``
* RST: Start lines with ``#.``

  .. important:: Yes, the trailing ``.`` is important

  For example:

  .. code-block:: rst

     #. Item number 1
     #. The second item
     #. A third item

  All the same rules for indentation apply as described above.

Comments
--------

* MD: Enclose content in ``<!--`` and ``-->`` (i.e., HTML comments,
  but they are included in the output)
* RST: Start a line with two periods and a space.

  For example, the following block is a comment, and will not be
  included in the output:

  .. code-block:: rst

     .. Hello world.  This is a comment.  This whole block is a
        comment.  You can leave it here in the final document, and it
        will not be included in the rendered output.

        Your comment can even include blank lines.  You terminate a
        comment -- just like most other things in RST -- by a blank
        line and then outdenting back out to the same column as the
        first ".".

     This line is no longer part of the comment.

Including files
---------------

* MD: You cannot include files in Markdown.
* RST: Use the ``.. include:: FILENAME`` directive.  For example:

  .. code-block:: rst

     .. include:: features-extensions.rst
     .. include:: features-java.rst

  Those directives include those 2 files right here in this RST file.

  .. important:: Chapter/section/subsection delimiters will be
                 continued in those files as part of rendering this
                 file.

Hyperlinks to URLs
------------------

* MD:

  .. code-block:: md

     [this is the link text](https://example.com/)

* RST:

  .. code-block:: rst

     `this is the link text <https://example.com/>`_

  .. important:: Yes, the trailing underscore in RST is important.
                 It's a little weird, but you'll cope.

Hyperlinks to anchors
---------------------

* MD: I forget offhand how to make anchors and links to them in MD.
* RST: Use the ``:ref:`` directive.

  Make an anchor like this:

  .. code-block:: rst

     .. _ANCHOR_NAME:

  It *must* start with and underscore and end with a colon.

  I've typically used anchor names that either begin with ``label-``
  or end in ``-label`` to make it blatantly obvious that it's a
  label. For example:

  .. code-block:: rst

     .. _building-and-installing-section-label:

  Then you can use the ``:ref:`` directive:

  .. code-block:: rst

     be sure to see :ref:`the VPATH build section
     <building-and-installing-section-label>`.

Hyperlinks to other (RST) pages
-------------------------------

* MD:

  .. code-block:: md

     [link text](page_name)

* RST: Use the ``:doc:`` directive.

  General format:

  .. code-block:: rst

     :doc:`link text <PAGE_PATH>`

  For example:

  .. code-block:: rst

     You should read :doc:`the Developer's Guide </developers>`.

  The page path is relative to the ``docs`` dir in the OpenPMIx git tree.

Macros
------

* MD: There are no macros in Markdown.
* RST: We have defined a few OpenPMIx-specific macros in RST.  You can
  insert these macros anywhere in RST content text.

  * ``|ompi_ver|`` is the full OpenPMIx version number, including
    alpha/beta/rc/greek denotation.  For example ``5.0.0rc1``.

  * ``|ompi_series|`` is the major/minor OpenPMIx version, e.g.,
    ``5.0.x``.

    .. important:: Think twice about hard-coding the OpenPMIx version
                   number or series when referring to the current
                   version or series.  It can be appropriate to
                   hard-code an "x.y.0" version to denote a
                   generational epoch, but in most other cases, you
                   probably want to use one of the macros.

  * ``|mdash|`` is a unicode long dash, an "em" dash.  Use it instead
    of ``--``.

  * ``|rarrow|`` is a unicode right arrow.  Use it instead of ``->``
    or ``-->``.

Brightly-colored boxes
----------------------

* MD: There are no brightly-colored boxes in MD.

* RST: You can use various directives to make brightly-colored
  "note" boxes (Called admonitions) in RST.  For example:

  .. important:: a green box with a "!" icon

     Standard indenting rules apply for the content in the box.  You
     can have multiple lines and multiple paragraphs, for example.

     Yippee.

     * You can even have bullets.

       .. code-block:: none

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

     You can name this box whatever you want:

     .. code-block:: rst

        .. admonition:: Custom title
           :class: tip

           Content of your box here.

     Custom text for this custom admonition.  Note that the ``:class: <type>``
     will change the coloring to the color for the basic admonition of that
     type.  E.g., ``:class: tip`` makes the box be green.
