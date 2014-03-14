PACKAGE = netsight
VERSION = $(shell cask version)
EMACS = emacs
DIST_FILES = netsight.el \
			 netsight-dired.el \
			 netsight-flymake.el \
			 netsight-style.el \
			 netsight-keybindings.el \
			 netsight-rst.el \
			 netsight-python.el \
			 netsight-utils.el \
			 ${PACKAGE}-pkg.el
DIST_DIR = dist/${PACKAGE}-${VERSION}
PACKAGE_DIR = $(shell cask package-directory)

clean:
	rm -f ${PACKAGE}-pkg.el
	rm -rf dist/${PACKAGE}-${VERSION}

dist-clean:
	rm -f ${PACKAGE}-pkg.el
	rm -rf dist

dist/${PACAKGE}-${VERSION}.tar: ${PACKAGE}-${VERSION}.tar

${PACKAGE}-${VERSION}.tar: ${PACKAGE}-${VERSION}
	tar --directory dist -cvf dist/$@ $<

dist/${PACKAGE}-${VERSION}.tar.gz: ${PACKAGE}-${VERSION}
	tar --directory dist -cvzf dist/$@ $<

${PACKAGE}-${VERSION}: dist/${PACKAGE}-${VERSION}

dist/${PACKAGE}-${VERSION}:
	mkdir -p $@
	cask package
	cp -v ${DIST_FILES} $@

install:
	${EMACS} --batch -Q \
		--load package \
		--eval '(setq-default debug-on-error t)' \
		--eval "(setq package-archives ())" \
		--eval "(add-to-list 'package-archives \
					'(\"marmalade\" . \"http://marmalade-repo.org/packages/\"))" \
		--eval "(add-to-list 'package-archives \
					'(\"melpa\" . \"http://melpa.milkbox.net/packages/\"))" \
		--eval '(list-packages)' \
		--eval '(package-install-file "${PWD}/dist/${PACKAGE}-${VERSION}.tar")' \
		--eval "(when (require 'jedi nil :noerr) (jedi:install-server))"
