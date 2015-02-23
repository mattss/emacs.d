=======
 Usage
=======

nesight-mode
============
At ``Netsight``, we use a custom minor-mode which defines a set of
``emacs`` functions and key bindings that we've found useful over
time.

By default, this minor mode is activated globally (as indicated in
your mode line).

You can turn this off by adding the following in file pointed to by
the ``custom-file`` variable:

.. code-block: lisp

   (netsight-mode 0)

As with all other modes, Key-bindings and functions provided by the
mode are documented in the ``info`` window when you invoke ``C-h m``
or ``M-x describe-mode`` and navigate to ``Netsight``.
 
python-mode
===========
This package use the built-in python_ mode provided in emacs24.

It has been configure to add hooks which load the pungi_ package.
Additionally, the ``netsight-mode`` function `py-insert-debug` is
enabled, which inserts the ``pdb.set_trace`` command on the current
line indicated by position of the cursor.

Custom settings and functions
=============================

The default ``custom-file`` is:

~/.emacs-customize.el

If you use the ``customize`` interface in Emacs_, then any saved
settings generated will also be appended to this file.

Your own settings and functions should be added to
``~/.emacs-custom.el``, this file will be created on your behalf (if
it doesn't already exist) upon installation. Move any personal
preferences, settings, and/or utility functions you've previously used
into this file.

If you require variables to differ depending on the project you're
working on, consider using `directory local variables`_.

Package management
==================
New packages can be added to Emacs by using the package manager ``M-x
list-packages``.

The pallet_ package automatically takes care of keeping the `Cask
file_` up to date with packages you may install or delete with
``list-packages``.

Python development
==================
By default, the netsight package uses the built-in python_ package,

The pungi_ package provides jedi_ and pyvenv_ integration for
buildout_ and virtualenv.

The flycheck_ package is used for PEP8 and syntax checking.

The sphinx-doc_ package provides auto-generation of documentation
strings for functions and methods.  Consult the python_ mode help for
commands to insert doc strings for other Python_ statements.


Related packages
================

  python-mode_
    An Emacs mode for editing Python code

  jedi_
    Python auto-completion for Emacs.

  pyvenv_
    Management of Python virtual environments.

  pungi_
    Integrates jedi with virtualenv and buildout_ python environments.

  flycheck_
    On-the-fly syntax checking (Flymake done right)
					

See the package documentation for each of the above for a synopsis on
the all the key-bindings and utilities available.

the pungi_ package detects if the file you are editing resides in
either virtualenv, or ``buildout``, and makes the
``jedi:goto-definition`` feature work in either environment.

.. _Emacs: http://www.gnu.org/software/emacs
.. _`directory local variables`: http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
.. _buildout: http://www.buildout.org/en/latest/
.. _flycheck: http://flycheck.readthedocs.org/en/latest/
.. _jedi: http://jedi.jedidjah.ch/en/latest/
.. _pallet: https://github.com/rdallasgray/pallet
.. _pungi: https://github.com/mgrbyte/pungi.git
.. _python: https://github.com/fgallina/python.el
.. _pyvenv: https://github.com/jorgenschaefer/pyvenv
.. _sphinx-doc: https://github.com/naiquevin/sphinx-doc.el


