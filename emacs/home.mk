### emacs rules

emacs_dir := $(HOME)/.emacs.d
emacs_init := $(emacs_dir)/init.el
emacs_backup := $(emacs_dir)/backup

all: $(emacs_init) $(emacs_backup)

# as "Save Options" writes .emacs the clean target should not remove it
#clean_files += $(HOME)/.emacs.d/init.el
clean_dirs += $(HOME)/.emacs.d/backup

# Make sure that the emacs init file starts with our load.
# 
emacs_load_command := (load "~/a/emacs/my-emacs.el" t t t)
$(emacs_init): $(top)/emacs/my-emacs.el | $(emacs_dir)
	@if test -f $@ && grep -q '$(emacs_load_command)' '$@'; then \
	  echo '$@ already contains $(emacs_load_command)' ; \
	  touch '$@' ; \
	else \
	  echo 'Prepending $(emacs_load_command) to $@' ; \
	  t=$$(mktemp) ; \
	  echo '$(emacs_load_command)' > $$t ; \
	  test -f '$@' && cat '$@' >> $$t ; \
	  mv -f $$t $@ ; \
	fi

$(emacs_dir):
	mkdir $@

$(emacs_backup): | $(emacs_dir)
	mkdir $@

