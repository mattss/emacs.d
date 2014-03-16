============
Known issues
============

Please add any known issues, following the legend below.

Legend
------
:Issue-ref: Should be a link to the issue in a bug-tracker, if possible,
	    otherwise a descriptive unique identifier of some kind. *required*
:Issue-found-date: The date whence found.
:Issue-resolved-date: The date whence resolved.
:Issue-status: One of ``broken``, ``work-around-possible``, ``resolved``. 
	       The master branch should never contain issues with status **broken**.  
	       *required*
:Issue-description: A breif description. *required*
:Issue-work-around: Instructions on how to fix the issue *required* if status is not ``broken``.
:Issue-resolved-desc: A synopsis of what was done to resolve the issue.


2014-03-15
==========
:Issue-ref: pyflakes-py-version-check-breaks-isort_
:Issue-found-date: 2014-03-15
:Issue-resolved-date: 2014-03-16
:Issue-status: resolved
:Issue-desc:
  pyflakes 0.7.3 (python package) is broken due to a lame Python
  version checking scheme.
  This has been reported on lanuchpad.
:Issue-work-around: 
   Remove pyflakes 0.7.3 from your system/user local/virtualenvs, e.g::

     	rm -rf ~/.local/lib/python2.7/site-packages/pyflakes*
   
   Checkout master from github::
     $ cd ~/git
     $ git clone https://github.com/pyflakes/pyflakes
     $ cd pyflakes
     $ pip install --user .
:Issue-resolved-desc:
   Removed packages ``flymake-python-flymake`', ``flymake-cursor``.
   Package ``flymake`` is builtin, so left that alone.
   Removed all ``pyflakes`` configuration.
   Installed flycheck_ and configured that.
   ``flycheck`` seems to provide a more stable and performant experience.
   It has been configured to use the netsight ``pycheckers.py`` script as
   flymake did before it.  
   This also aleiviates the need for others to mess around with their 
   python installation (i.e. the work around).
  
.. _pyflakes-py-version-check-breaks-isort: https://bugs.launchpad.net/pyflakes/+bug/1277606#4?comments=all
.. _flycheck: http://flycheck.readthedocs.org/en/latest/
