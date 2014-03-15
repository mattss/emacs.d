============================
Netsight - Emacs environment
============================

This Emacs_ setup assumes Emacs 24 or higher.
Requires Cask_  be installed.
Cask can also be installed via your systems' normal package management
tools (e.g homebrew, apt).

1.  Installation

  1.1  OSX

  1.2  Migration from an existing configuration

  1.3  Final Installation

2.  Usage

  2.1  Custom settings and functions

  2.2  Experimental features

  2.3  Temporary experimentation with ``customize``

  2.4  Interactive documentation

  2.5  Python development

  2.6  Related packages

3.  Contributing


Installation
============

Ensure that ``emacs`` and ``cask`` are available on your shell's $PATH.

OSX
---

The OSX app, and Homebrew usually install an ``Emacs`` executable to::

  /Applicatons/Emacs.app/Contents/MacOS/Emacs 

Add either an alias to your shell profile::

  alias emacs="/Applicatons/Emacs.app/Contents/MacOS/Emacs"

Or create a symbolic link such that``Emacs``resolves correctly
using $PATH::

  test -d ~/bin || mkdir ~/bin/
  export PATH="$HOME/bin:$PATH"

$ ln -s /Applicatons/Emacs.app/Contents/MacOS/Emacs ~/bin/emacs
 
Test that this works::

  $ emacs -nw

Migration from an existing configuration
----------------------------------------

.. code: bash

   test -d ~/.emacs.d && mv ~/emacs.d{,.bak}
   test -f ~/.emacs && mv ~/.emacs{,.bak}


Installation
-------------

.. CAUTION:
   Since Emacs uses several diff_erent libraries,
   please check KNOWN_ISSUES.rst and apply any workarounds
   that may be required before proceding to final installation.

.. code: bash
	  
  git clone https://github.com/netsight/emacs-netsight ~/.emacs.d
  cd ~/.emacs.d
  cask
  emacs -Q -nw -l package -l jedi --script --eval '(jedi:install-server)'


``emacs-netsight`` should now be installed.


Usage
=====
Install new packages via Emacs's default package manager::

  ``M-x list-pacakges``

The pallet_ package automatically
takes care of keeping the Cask_ file up to date with packages 
you may install or delete with ``list-packages``.

Custom settings and functions
-----------------------------

The default ``custom-file`` is "~/.emacs-custom.el".
Place any personal preference settings and utility 
functions in this file.

If you require variables to differ depending on 
the project you're working on, 
consider using `directory local variables`_.

Experimental features
---------------------

As you discover new packages and try new features,
we'd like to use them without requiring them permentaly in the 
main configuration.


In order to do this, we'll use the example of ``python-auto-magic`.

Write the lisp for the feature in the ~/.emacs.d/experimental directory, 
then add load it using your ``custom-file`` ::

.. code::lisp

     (load-experimental "python-auto-magic.el"))


Temporary experimentation with ``customize``
--------------------------------------------
Using the customize interface will allow easy introspection
of the relevant feature, providing options for values,
and some documentation as to their purpose.

.. code::lisp

  (customize-variable)
  (customize-theme)


When saved, ``emacs-lisp`` is written to your ``custom-file``.

Interactive documentation
-------------------------

When the cursor is over a symbol::

  * function: ``C-h f`` 
  * variable: ``C-h v``

Show information about the current ``major-mode``,
usually includes key-bindings::

  ``C-h m`` or ``M-x describe-mode``

Show information about a particular package::

  ``C-h P`` or ``M-x describe-package``

Python development
------------------
By default, the netsight package uses the python-mode_.
package from the marmalade_ repository.
Should you prefer, you can use the default mode that is
built-in to ``Emacs``, named ``python``.
In order to do so you'll need to uninstall the python-mode_
package.

Related packages
----------------

  jedi_
    Provides code completion, navigation and documentation helpers. 

  pungi_
    Integrates jedi_ with python-mode_.

  flymake-python-flymake_
    Integration of flymake_ with python-mode_.


To make ``jedi:goto-definition`` work correctly,
the pungi_ package detects if the file you are editing
resides in either virtualenv, or ``buildout``.

If your project uses buildout_, then::

  When ``eggs-directory`` is defined in the project buildout,
  ensure that is is somewhere "above" /home/zope/<project/eggs, 
  or create a symlink.
    
  When ``eggs-directory is shared, make sure it lives under:
  /home/eggs or /home/zope/eggs.

  In this way, ``jedi:goto-definition`` should always be able to
  'jump' to the correct source file (if you've run buildout!)
   
Contributing
============
If you think of a feature you'd like to add, or have found a bug,
please raise an issue on github.

Read HACKING.rst for a guide on developing emacs-netsight.
Thanks!

.. _Cask: https://github.com/cask/cask
.. _Emacs: https://www.gnu.org/software/emacs/
.. _Flymake: http://www.gnu.org/software/emacs/manual/html_node/flymake/index.html#Top
.. _buildout: http://www.buildout.org/en/latest/
.. _`directory local variables`: http://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html
.. _emacs-netsight: https://github.com/netsight/emacs-netsight
.. _jedi: http://jedi.jedidjah.ch/en/latest/
.. _marmalade: http://marmalade-repo.org
.. _pallet: https://github.com/rdallasgray/pallet
.. _pungi: https://github.com/mgrbyte/pungi.git
.. _python-mode: https://launchpad.net/python-mode

:Author: Matthew Russell <mattr@netsight.co.uk> @mgrbyte
:Date:   2014-03-15
