.POSIX:
# Emacs invocation
# Don't load an init file: -q
# Avoid processing X resources: -Q
# Send messages to stderr: --batch
EMACS = emacs -Q -q --batch

# Remove command
RM = rm

# Additional emacs load-path and autoload
LOAD_PATH := -L .
LOAD_AUTOLOAD := -l autoload

# Define Compile Command (COMPILE)
# Call batch-byte-compile function: -f
COMPILE := -f batch-byte-compile

# AUTOLOAD related variables
AUTOLOAD_DIR  := "${PWD}"
AUTOLOAD_FILE := "${PWD}/iclj-autoloads.el"
AUTOLOAD_EVAL := --eval '(make-directory-autoloads ${AUTOLOAD_DIR} ${AUTOLOAD_FILE})'

# Expand the source code files
EL := $(wildcard *.el)

# Expand the object (.elc) files
ELC := $(EL:.el=.elc)

# Entry Point
all: compile autoload

# Compile needed files
compile: $(ELC)

autoload:
	${EMACS} ${LOAD_AUTOLOAD} ${AUTOLOAD_EVAL}

.SUFFIXES: .el .elc
.el.elc:
	${EMACS} ${LOAD_PATH} ${COMPILE} $<

.PHONY: clean
clean:
	${RM} ${ELC}
