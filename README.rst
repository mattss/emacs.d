:Author: Matthew Russell <mattr@netsight.co.uk>
:Date:   2014-03-13

========================================
Hacking on the Netsight Emacs 24 pacakge
========================================

This is an Emacs (24) package.
It provides a variety of utilities and emacs goodies that
should make Emacsen :D.

Why does this exist? / Why changs from previous devtools emacs-24 config?
=========================================================================

The author wanted to be able to re-use the python
emacs config he uses at home and at work without having
to have rely upon work settings at home and vica-versa.

The author now authors 3 emacs packages:
 * ``pungi`` - Python integration with jedi
 * ``mgrbyte`` - Personal (local at home only package)
 * ``netsight`` - Work specific functions and settings.

There's actually a few functions in "netsight" the author would
like to factor out into something that could be used elsewhere,
like the date/time insertion tools.

Also:
 * Better pacakge dependencies
 * Saner(tm) development experience,
 * Integration with elpa/list-packages.


Hacking
=======

Checkout or copy the source directory whence this
file is located. In the example below, the author's local
bitbucket directory paths are used.

Create a directory that will be used to host local packages.
Below the example of "~/orion" is given, but it can be any directory.


Development cycle
=================

Initial Setup
-------------
Install ``cask`` for your operating system.
See http://cask.github.io/
OSX users can install ``cask`` with homebrew.
Once installed, run:

$ make clean
$ make $(cask version)
$ make install

Cycle
------
1. Add new feature/make changes to elisp files (.el extensions)
2. Bump the version number in the filename:Cask
3. run:
   $ cask install
4. run:
   $ emacs -Q -nw -l package
   M-x list-packages
   Search and select the ``netsight`` package.
   Mark for deletion with key ``d``.
   Execute deltetion with key ``x``.
   exit emacs.
5. run:
   $ make clean && make $(cask version)
6. run:
   $ emacs -Q -nw -l package
   M-x list-packages
   M-x install-package-file RET
   Specifcy path to tar file: 
       dist/$(cask version)/netsight-$(cask version).tar
   RET
6. Check that there are no errors.
   Preferably, ensure that there are no warnings.
7. commit and push.

list-packages provides a feature to update versions,
so when we're happy with a particular upgrade, we should update the dependency
and bump the version of this package.


Debugging tips
==============
Do not 'ffap to the line in the output to attempt to fix the error, since the file compiled is
in the 'build' directory, not the source.
You can however rely upon the line numbers of the errors :)

If no errors or warnings occur, then the compile log won't be shown in the package manager
- i.e a clean install.


Upgrading
=========

Upgrade using the pacakge manager.

Method:
-------
   1. list-packages
     1.1 list-packages
     1.2 mark netsight package for upgrade (list-package should indicate upgrade
         it available, if not, visit netsight-develop.el and evaluate the buffer,
         then run list-packages again)
     1.3 Hit Shift-u, x (upgrade and install) and confirm.

TDB:
----
Create a hosted package distribution directory such that
evaluation of the netsight-develop.el (to grok local package directory) is not
required.
