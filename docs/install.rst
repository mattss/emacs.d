=========================
 Installing this package
=========================

Ensure that ``emacs``, ``cask`` are both
available as an executable on your shell's $PATH.

cask
====
If you've not used ``cask`` before, the key is to ensure that you have
a ``.cask`` directory in your $HOME directory, and that your shell
profile adds the ``cask`` binary to $PATH.

Use of this package assumes you know how to configure emacs and cask
appropriately for your platform.

You can find cask here: http://cask.readthedocs.io/en/latest/guide/installation.html


Migration from an existing configuration
========================================
Backing up your existing configuration:

.. code-block:: bash

   test -d ~/.emacs.d && mv ~/.emacs.d{,.bak}
   test -f ~/.emacs && mv ~/.emacs{,.bak}


Prerequisites
=============
This package uses ``init.el`` for ``emacs`` initialisation (as opposed
to .emacs which is more commonly used.

flycheck:

    Used for syntax checking in most modes, especially for ``python``.


Installation
============
If you want to run a stable version, please checkout a release tag

See https://github.com/netsight/emacs.d/releases

For the commands below we'll use the ``master`` branch.

.. code-block:: bash

  git clone https://github.com/netsight/emacs.d ~/.emacs.d
  cd ~/.emacs.d
  make

Launch ``emacs`` and evaluate the following emacs-lisp expression
(e.g in the ``*scratch*`` buffer, or invoke with ``M-x``):

.. code-block:: lisp

    (jedi:install-server)


Updating packages
=================
When installing new package with the emacs command ``list-packages``,
this will will update the ``cask`` configuration file
``.emacs.d/Cask``.

The emacs package ``pallet`` does this seamlessly
in the background.  Should you want to synchronise the packages
configured by cask in a running emacs without restarting, you can just
invoke:

   ``M-x pallet-update``

It is suggested to fork this package and maintain it using git should
you want to use packages not provided by default.  Alternatively, if
you think given package is really useful, please send a pull request
and we'll consider adding it to the default configuration.
