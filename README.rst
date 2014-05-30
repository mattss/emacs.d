=============================
Emacs development environment
=============================
One of the primary motivations for this package is to provide a consistent,
useful python_ development environment.

It is also opinionated, providing mode hooks for a host of extensions.

Each of these can of course been overridden/configured as you prefer.

use-package_ is used to configure the default set of packages that will be installed.

See the use-package_ statements in ``.emacs.d/init.el``.

This Emacs_ setup assumes Emacs 24 or higher.

Cask_  is used for package management and installation

Please see the documentation in docs_ for installation, custom configuration and more.

.. CAUTION:
   Since Emacs uses several different libraries,
   please check KNOWN_ISSUES.rst and apply any workarounds
   that may be required before proceeding to final installation.
 
Read HACKING.rst_ for a guide on developing emacs-netsight_.
   
Contributing
============

Git Hub
-------

See emacs-netsight_ on Github

If you think of a feature you'd like to add, or have found a bug,
please raise an issue on github.

.. _`Contribution guidelines`: blobs/master/CONTRIBUTING.rst
.. _Cask: https://github.com/cask/cask
.. _Emacs: https://www.gnu.org/software/emacs/
.. _HACKING.rst: HACKING.rst
.. _docs: docs
.. _emacs-netsight: https://github.com/netsight/emacs-netsight
.. _python: https://github.com/fgallina/python.el
.. _use-package: https://github.com/jwiegley/use-package

