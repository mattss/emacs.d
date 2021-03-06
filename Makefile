EMACS=$(shell which emacs)
EMACS_CUSTOMIZE_FILE=${HOME}/.emacs-customize.el
VENV_WORKON_HOME=${HOME}/.virtualenvs
USER_CONF_DIR=${HOME}/.config
FLAKE8_CONF_NAME=flake8rc
FLAKE8_CONF_SRC=${USER_CONF_DIR}/${FLAKE8_CONF_NAME}.template
FLAKE8_CONF_TARGET=${USER_CONF_DIR}/${FLAKE8_CONF_NAME}

all: pythonsetup

pythonsetup:
	$(shell test -d ${USER_CONF_DIR} || mkdir ${USER_CONF_DIR})
	$(shell test -f ${USER_CONF_DIR}/${FLAKE8_CONF_NAME} || cp ${FLAKE8_CONF_SRC} ${FLAKE8_CONF_TARGET})
	$(shell test -f ${EMACS_CUSTOMIZE_FILE} || touch ${EMACS_CUSTOMIZE_FILE})
	$(shell test -d ${VENV_WORKON_HOME} || mkdir ${VENV_WORKON_HOME})
	pip install --user --install-option="--prefix=" -r requirements.txt
	cask install
	${EMACS} --batch -nw -Q \
		--load package \
		--eval "(when (require 'jedi nil :noerr) (jedi:install-server))"
clean:
	$(shell pip uninstall -y -r requirements.txt)
