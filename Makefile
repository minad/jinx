# *- mode: makefile -*-
# ----------------------------------------------------------------------------
# Basic Setup
# ----------------------------------------------------------------------------

NAME 		= jinx

DESTDIR     ?=
PREFIX      ?= /usr
DATADIR     ?= $(PREFIX)/share
BINDIR      ?= $(PREFIX)/bin
LIBDIR      ?= $(PREFIX)/lib
ELDIR       ?= $(DATADIR)/emacs/site-lisp
DYNMODDRIR  ?= $(LIBDIR)/emacs

EMACS		?= /usr/bin/emacs


# ----------------------------------------------------------------------------
# Global Flags
# ----------------------------------------------------------------------------

CPPFLAGS += -D_GNU_SOURCE

CFLAGS   += -Wall
CFLAGS   += -Wmissing-prototypes
CFLAGS   += -Wunused-result
CFLAGS   += -W
CFLAGS   += -std=c99
CFLAGS   += -O2
CFLAGS   += -g

LDFLAGS  += -g
LDLIBS   += -Wl,--as-needed

# Command line flags for Emacs.
EMACSOPT = -batch

# The actual Emacs command run in the targets below.
emacs = '$(EMACS)' $(EMACSOPT)

# Extra flags to pass to the byte compiler
BYTE_COMPILE_FLAGS ?=

# 'make' verbosity.
V = 0
ifeq (${V},1)
V_at 	=
V_CC	=
V_CCLD	=
V_ELC 	=
V_ELN	=
V_ELA	=
else
V_at 	= @
V_CC 	= @$(info $   CC       $@)
V_CCLD	= @$(info $   CCLD     $@)
V_ELC	= @$(info $	  ELC	   $@)
v_ELN	= @$(info $	  ELN	   $@)
V_ELA	= @$(info $   ELA      $@)
endif

# flags from pkgconfig

PKG_NAMES    := enchant-2
PKG_CFLAGS   := $(shell pkg-config --cflags $(PKG_NAMES))
PKG_LDLIBS   := $(shell pkg-config --libs   $(PKG_NAMES))

ifeq ($(PKG_LDLIBS),)
$(error One of the dependency not found, dependencies: $(PKG_NAMES))
endif

PKG_CPPFLAGS := $(filter -D%,$(PKG_CFLAGS)) $(filter -I%,$(PKG_CFLAGS))
PKG_CFLAGS   := $(filter-out -I%, $(filter-out -D%, $(PKG_CFLAGS)))

CPPFLAGS += $(PKG_CPPFLAGS)
CFLAGS   += $(PKG_CFLAGS)
LDLIBS   += $(PKG_LDLIBS) -lrt

SO ?= .so

# ----------------------------------------------------------------------------
# Top Level Targets
# ----------------------------------------------------------------------------


TARGETS		  += jinx-mod$(SO)
ELISP_TARGETS += jinx.elc jinx-autoloads.el

.PHONY: build elisp clean install

all:: build elisp

build:: $(TARGETS)

elisp:: $(ELISP_TARGETS)

clean::
	$(RM) *.o $(TARGETS) $(ELISP_TARGETS)

# ----------------------------------------------------------------------------
# Pattern rules
# ----------------------------------------------------------------------------

install-%-dynamic-module:
	$(if $<, install -m755 -d $(DESTDIR)$(DYNMODDRIR))
	$(if $<, install -m755 $^ $(DESTDIR)$(DYNMODDRIR))

install-%-el:
	$(if $<, install -m755 -d $(DESTDIR)$(ELDIR))
	$(if $<, install -m644 $^ $(DESTDIR)$(ELDIR))


%$(SO): LDFLAGS += -shared -Wl,-soname,$@

%$(SO):
	$(V_CCLD)$(CC) -o $@  $^ $(LDFLAGS) $(LDLIBS)

%.pic.o : CFLAGS += -fPIC
%.pic.o : CFLAGS += -fvisibility=hidden

%.pic.o : %.c
	$(V_CC)$(CC) -o $@ -c $< $(CPPFLAGS) $(CFLAGS)

%.o     : %.c
	$(V_CC)$(CC) -o $@ -c $< $(CPPFLAGS) $(CFLAGS)

%.elc: %.el
	$(V_ELC)$(emacs) $(BYTE_COMPILE_FLAGS) \
	-l comp -f batch-byte-compile $<

%-autoloads.el: %.el
	$(V_ELA)$(emacs) -l autoload -l cl-lib --eval "\
(let ((file (expand-file-name \"$@\"))\
      (autoload-timestamps nil) \
      (backup-inhibited t)\
      (version-control 'never)\
      (coding-system-for-write 'utf-8-emacs-unix))\
  (write-region (autoload-rubric file \"package\" nil) nil file nil 'silent)\
  (cl-letf (((symbol-function 'progress-reporter-do-update) (lambda (&rest _)))\
            ((symbol-function 'progress-reporter-done) (lambda (_))))\
    (let ((generated-autoload-file file))\
      (update-directory-autoloads default-directory))))" \
	2>&1 | sed "/^Package autoload is deprecated$$/d"


# ----------------------------------------------------------------------------
# jinx-mod
# ----------------------------------------------------------------------------

jinx_mod_src = \
	jinx-mod.c

jinx_mod_obj = $(jinx_mod_src:.c=.pic.o)

jinx-mod$(SO) : $(jinx_mod_obj)

# ----------------------------------------------------------------------------
# jinx
# ----------------------------------------------------------------------------

jinx.elc: jinx.el

# ----------------------------------------------------------------------------
# packaging
# ----------------------------------------------------------------------------

install: $(addprefix install-, jinx-el jinx-dynamic-module)

install-jinx-el: $(ELISP_TARGETS) $(ELISP_TARGETS:.elc=.el)

install-jinx-dynamic-module: jinx-mod$(SO)
