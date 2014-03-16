============================
Netsight - Emacs environment
============================

This Emacs_ setup assumes Emacs 24 or higher.

Cask_  is used for installation and continuous configuration,
and can be installed via your systems' normal package management
tool. 
 
Installation
============

Ensure that ``emacs`` and ``cask`` are available on your shell's $PATH.

OSX
---

.. code-block:: bash

   brew install cask


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
   Since Emacs uses several diff_erent libraries,
   please check KNOWN_ISSUES.rst and apply any workarounds
   that may be required before proceding to final installation.

.. code-block:: bash
	  
  git clone https://github.com/netsight/emacs-netsight ~/.emacs.d
  cd ~/.emacs.d
  cask
  emacs -Q -nw -l package -l jedi --script --eval '(jedi:install-server)'


``emacs-netsight`` should now be installed.


Usage
=====
Install new packages via Emacs's default 
package manager ``list-pacakges``.

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

You can experiemnt with the emacs commands ``customize-variable`` and
``customize-theme``.

When saving options using the above commands, 
the resulting ``emacs-lisp`` configuration is written to your 
``custom-file``.

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

  python-mode_
    An Emacs mode for editing Python code

  jedi_
    Python auto-completion for Emacs.

  pungi_
    Integrates jedi with virtualenv and buildout python environments.

  flycheck_
    On-the-fly syntax checking (Flymake done right)

A selection of "most useful" keybindings
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+----------------------+-------------------------------+--------------------------------+--------------------------+
|         mode         |          Key-binding          |            Command             |        Descripton        |
+----------------------+-------------------------------+--------------------------------+--------------------------+
|     python-mode      |            C-c C-l            |         py-shift-left          |       Shifts code        |
|                      |                               |                                |       region left        |
+----------------------+-------------------------------+--------------------------------+--------------------------+
|     python-mode      |            C-c C-r            |         py-shift-right         |       Shifts code        |
|                      |                               |                                |      region right.       |
|                      |                               |                                |                          |
+----------------------+-------------------------------+--------------------------------+--------------------------+
|                      |             C-c .             |      jedi:goto-definition      |       Jump to the        |
|         jedi         |                               |                                |      source code of      |
|                      |                               |                                |      the symbol at       |
|                      |                               |                                |          point.          |
+----------------------+-------------------------------+--------------------------------+--------------------------+
|         jedi         |             C-c /             |  anything-jedi-related-names   |   Lookup related names   |
|                      |                               |                                |        for symbol        |
|                      |                               |                                |        at point.         |
+----------------------+-------------------------------+--------------------------------+--------------------------+
|         jedi         |             C-c ?             |         jedi:show-doc          |           Show           |
|                      |                               |                                |      documentation       |
|                      |                               |                                |      for context at      |
|                      |                               |                                |          point.          |
+----------------------+-------------------------------+--------------------------------+--------------------------+
|       netsight       |       C-c C-d, C-c C-t        |  insert-current-date, insert   |      Inserts curent      |
|                      |                               |         -current-time          |      date, time at       |
|                      |                               |                                |          point.          |
+----------------------+-------------------------------+--------------------------------+--------------------------+
|     google-this      |            C-c / t            |          google-this           |      Search google       |
|                      |                               |                                |      for the active      |
|                      |                               |                                |  region or mini-buffer   |
|                      |                               |                                |         prompted         |
|                      |                               |                                |          query.          |
+----------------------+-------------------------------+--------------------------------+--------------------------+
 					

See the package documentation for each of the above for a
synopsis on the all the key-bindings and utilties available.

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
.. _HACKING.rst: blobs/master/HACKING.rst
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
