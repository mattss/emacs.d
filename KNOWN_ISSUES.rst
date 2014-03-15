============
Known issues
============

Please add any known issues, following the legend below.

Legend
------
:Issue-ref: Should be a link to the issue in a bug-tracker, if possible,
	    otherwise a descriptive unique identifier of some kind. *required*
:Issue-found-date: The date whence found.
:Issue-fixed-date: The date whence fixed.
:Issue-status: One of ``broken``, ``work-around-possible``, ``fixed``. 
	       The master branch should never contain issues with status **broken**.  
	       *required*
:Issue-description: A breif description. *required*
:Issue-work-around: Instructions on how to fix the issue *required* if status is not ``broken``.


2014-03-15
==========
:Issue-ref: pyflakes-py-version-check-breaks-isort_
:Issue-found-date: 2014-03-15
:Issue-fixed-date: N/A
:Issue-status: work-around-possible
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


.. _pyflakes-py-version-check-breaks-isort: https://bugs.launchpad.net/pyflakes/+bug/1277606#4?comments=all
