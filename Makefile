EMACS="$(shell which emacs)"
EMACS_CUSTOMIZE_FILE="~/.emacs-customize.el"

all: pythonsetup

pythonsetup:
	test -f "$EMACS_CUSTOMIZE_FILE" || touch "$EMACS_CUSTOMIZE_FILE"
	pip install --user --upgrade flake8 virtualenv virtualenvwrapper
	cask update
	cask upgrade
	${EMACS} --batch -nw -Q \
		--load package \
		--eval "(when (require 'jedi nil :noerr) (jedi:install-server))"
clean:
	$(shell pip list | grep flake8 && pip uninstall flake8)
