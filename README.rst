============================
Netsight - Emacs environment
============================

This Emacs_ setup assumes Emacs 24 or higher.

Cask_  is used for installation and continuous configuration,
and can be installed via your systems' normal package management
tool. 
 
Installation
============

Ensure that ``emacs``, ``cask` and ``virtualenv`` are all
available as an executable on your shell's $PATH.

OSX
---

.. code-block:: bash

   cd 
   curl -fsSkL https://raw.github.com/cask/cask/master/go | python


The OSX app, and Homebrew usually install an ``Emacs`` executable to::

  /Applicatons/Emacs.app/Contents/MacOS/Emacs 

Add either an alias to your shell profile::

  alias emacs="/Applicatons/Emacs.app/Contents/MacOS/Emacs"

Or create a symbolic link such that``Emacs``resolves correctly
using $PATH::

  test -d ~/bin || mkdir ~/bin/
  export PATH="$HOME/bin:$PATH"

.. code-block:: bash

   ln -s /Applicatons/Emacs.app/Contents/MacOS/Emacs ~/bin/emacs
 
Test that this works::

.. code-block:: bash

   emacs -nw

Migration from an existing configuration
----------------------------------------

.. code-block:: bash

   test -d ~/.emacs.d && mv ~/emacs.d{,.bak}
   test -f ~/.emacs && mv ~/.emacs{,.bak}


Install
-------

.. CAUTION:
   Since Emacs uses several different libraries,
   please check KNOWN_ISSUES.rst and apply any workarounds
   that may be required before proceeding to final installation.

.. code-block:: bash

  # flycheck 'checker' dependency (proxies to pycheckers.py from devtools)
  pip2 install --user flake8
  git clone https://github.com/netsight/emacs-netsight ~/.emacs.d
  cd ~/.emacs.d
  # If you want to run a stable version, checkout a release tag
  # See: https://github.com/netsight/emacs-netsight/releases
  # e.g: 
  #     git checkout v1.0
  # Otherwise, we'll be building from the master branch
  cask

Setting up jedi requires an additional step::  

    Enter emacs and run ``load-library RET jedi``
    Then ``jedi:install-server``

``emacs-netsight`` should now be installed.

Usage
=====
``netsight-mode`` can be enabled globally in your ``custom-file`` with:

.. code-block:: cl

    (global-netsight-mode 1)

Alternatively, you can invoke ``C-n n m`` , type ``M-x netsight-mode`` at any
time, or add a mode specific hook in your ``custom-file``:

.. code-block:: cl
		
    (add-hook 'python-mode-hook '(lambda () (netsight-mode 1)))

Key-bindings and functions provided by the mode are documented in the ``info``
window when you invoke ``C-h m`` or ``M-x describe-mode`` and navigate to ``Netsight``.

Custom settings and functions
-----------------------------

The default ``custom-file`` is "~/.emacs-custom.el".

Place any personal preference settings and utility 
functions in this file.

If you use the ``customize`` interface in Emacs_, then any saved settings will
be appended to this file.

If you require variables to differ depending on 
the project you're working on, 
consider using `directory local variables`_.

Package management
------------------
New packages can be added to Emacs by using the package manager ``M-x list-packages``.

The pallet_ package automatically takes care of keeping the `Cask file_` up to 
date with packages you may install or delete with ``list-packages``.

Experimental features
---------------------
As you discover new packages and try new features,
we'd like to use them without requiring them permanently in the 
main configuration.

In order to do this, we'll use the example of ``python-auto-magic``.

Write the lisp for the feature in the ~/.emacs.d/experimental directory, 
then add load it using your ``custom-file``.

.. code-block:: cl

  (load-experimental "python-auto-magic.el"))

Temporary experimentation with ``customize``
--------------------------------------------
Using the customize interface will allow easy introspection
of the relevant feature, providing options for values,
and some documentation as to their purpose.

You can experiment with the emacs commands ``customize-variable`` and
``customize-theme``.

When saving options using the above commands, 
the resulting ``emacs-lisp`` configuration is written to your 
``custom-file``.

Python development
------------------
By default, the netsight package uses the built-in python_ package,

The pungi_ package provides jedi_ integration for buildout_ and virtualenv.

The flycheck_ package is used for PEP8 and syntax checking.

The sphinx-doc_ package provides auto-generation of documentation strings for functions
and methods.  Consult the python_ mode help for commands to insert docstrings for other
Python_ statements.


Related packages
----------------

  python-mode_
    An Emacs mode for editing Python code

  jedi_
    Python auto-completion for Emacs.

  pungi_
    Integrates jedi with virtualenv and buildout python environments.

  flycheck_
    On-the-fly syntax checking (Flymake done right)
					

See the package documentation for each of the above for a
synopsis on the all the key-bindings and utilities available.

The easiest way to do so is to use the ``describe-package`` 
command, e.g::
  
  C-h P RET python-mode

Jumping to a given source file from a Python symbol is 
done with the jedi_ command:

.. code-block:: cl

   (jedi:goto-definition) 

which by default is bound to::

  C-c .

the pungi_ package detects if the file you are editing
resides in either virtualenv, or ``buildout``.

If your project uses buildout_, 
when the setting ``eggs-directory`` is defined 
in the project buildout, ensure that is is somewhere "above" 
the directory path ``/home/zope/<project/eggs``, or create a symlink.
    
When ``eggs-directory`` is shared, make sure it lives under:
``/home/eggs`` or ``/home/zope/eggs``.

In this way, ``jedi:goto-definition`` should always be able to
'jump' to the correct source file (if you've run buildout!)

Read HACKING.rst_ for a guide on developing emacs-netsight.
   
Contribute
==========

Git Hub
-------

See emacs-netsight_ on Github


If you think of a feature you'd like to add, or have found a bug,
please raise an issue on github.

.. _`Contribution guidelines`: blobs/master/CONTRIBUTING.rst
.. _Cask: https://github.com/cask/cask
.. _Emacs: https://www.gnu.org/software/emacs/
.. _flycheck: http://flycheck.readthedocs.org/en/latest/
.. _HACKING.rst: HACKING.rst
.. _buildout: http://www.buildout.org/en/latest/
.. _`directory local variables`: http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
.. _emacs-netsight: https://github.com/netsight/emacs-netsight
.. _jedi: http://jedi.jedidjah.ch/en/latest/
.. _marmalade: http://marmalade-repo.org
.. _pallet: https://github.com/rdallasgray/pallet
.. _pungi: https://github.com/mgrbyte/pungi.git
.. _python-mode: https://launchpad.net/python-mode
.. _python: https://github.com/fgallina/python.el
.. _sphinx-doc: https://github.com/naiquevin/sphinx-doc.el

:Author: Matthew Russell <mattr@netsight.co.uk> @mgrbyte
:Date:   2014-03-15
:Last-modified: 2014-04-26
