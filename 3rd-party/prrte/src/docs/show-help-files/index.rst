The only purpose of this file is for Sphinx to have a top-level source
RST file that includes all the others.

This file -- and the ``index.txt`` file that it generates -- is not
used by PRTE code.  **PRTE code only uses the individually
``help-*.txt`` files that Sphinx generates.**  The additional output
files that Sphinx generates in the ``_build/text`` tree are just
harmelss by-products; we ignore them.

.. toctree::

   Pinfo <help-prte-info>

   Prte <help-prte>

   Prted <help-prted>

   Prterun <help-prterun>

   Prun <help-prun>

   Prun <help-pterm>

   Psched <help-psched>

   Prte runtime <help-prte-runtime>

   Hwloc base <help-prte-hwloc-base>

   Regex <help-regex>

   CLI <help-cli>

   Util <help-prte-util>

   Hostfiles <help-hostfile>

   Dash host <help-dash-host>
