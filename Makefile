EMACS=$(shell which emacs)
EMACS_CUSTOMIZE_FILE=${HOME}/.emacs-customize.el
VENV_WORKON_HOME=${HOME}/.virtualenvs

all: pythonsetup

pythonsetup:
	$(shell test -f ${EMACS_CUSTOMIZE_FILE} || touch ${EMACS_CUSTOMIZE_FILE})
	$(shell test -d ${VENV_WORKON_HOME} || mkdir ${VENV_WORKON_HOME})
	pip install --user --upgrade flake8 virtualenv virtualenvwrapper
	cask update
	cask upgrade
	${EMACS} --batch -nw -Q \
		--load package \
		--eval "(when (require 'jedi nil :noerr) (jedi:install-server))"
clean:
	$(shell pip list | grep flake8 && pip uninstall flake8)
