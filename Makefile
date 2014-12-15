EMACS=$(shell which emacs)

all: pythonsetup

pythonsetup:
	pip install --user --upgrade flake8 virtualenv virtualenvwrapper
	cask update
	cask upgrade
	${EMACS} --batch -nw -Q \
		--load package \
		--eval "(when (require 'jedi nil :noerr) (jedi:install-server))"
clean:
	$(shell pip list | grep flake8 && pip uninstall flake8)
