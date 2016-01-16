.PHONY: all clean

all:
clean:

### generic setup
path_from_home := a/scripts
clean_files :=
clean_dirs :=
top := $(HOME)/a

### rules for populating the executable dir from PATH

EXE_DIR := $(HOME)/bin
exe_list :=

$(EXE_DIR):
	mkdir -p $@

# Get the list of all executable scripts from this dir
list := $(addprefix $(EXE_DIR)/,$(notdir $(shell \
                        find $(CURDIR) -mindepth 1 -maxdepth 1 -type f -perm /100)))

exe_list += $(list)
$(list) : $(EXE_DIR)/% : %
	ln -sf $(CURDIR)/$* $@

# Get the list of all executable scripts from mozilla dir
mozilla_dir := $(realpath $(CURDIR)/../mozilla)

# mozilla_dir is empty when a/mozilla does not exist
# so check for executables there under if
ifneq ($(strip $(mozilla_dir)),)
mozilla_scripts := $(addprefix $(EXE_DIR)/,$(notdir $(shell \
                        find $(mozilla_dir) -mindepth 1 -maxdepth 1 -type f -perm /100)))

exe_list += $(mozilla_scripts)

$(mozilla_scripts) : $(EXE_DIR)/% : $(mozilla_dir)/%
	ln -sf $(mozilla_dir)/$* $@
endif

# add extra names for some scripts

list := egreptree fgreptree
list := $(addprefix $(EXE_DIR)/,$(list))
exe_list += $(list)
$(list) : greptree
	ln -sf $(CURDIR)/$< $@

list := u-root-term u-dserver-term u-hippyru-term u-kino-term u-thip-term
list := $(addprefix $(EXE_DIR)/,$(list))
exe_list += $(list)
$(list) : u-term
	ln -sf $(CURDIR)/$< $@

list := u-vnc-ar  u-vnc-f2
list := $(addprefix $(EXE_DIR)/,$(list))
exe_list += $(list)
$(list) : u-vnc
	ln -sf $(CURDIR)/$< $@

all: $(EXE_DIR) $(exe_list)

clean_files += $(exe_list)
clean_dirs += $(EXE_DIR)

### generic rules for dot files that are symlinked to home

home_dot_files := $(addprefix $(HOME)/.,$(basename $(wildcard *.dot)))
clean_files += $(home_dot_files)
all: $(home_dot_files)
$(home_dot_files) : $(HOME)/.% : %.dot
	ln -sf $(path_from_home)/$*.dot $@

### extra bash symlinks besides .bashrc

clean_files += $(HOME)/.bash_profile
all: $(HOME)/.bash_profile
$(HOME)/.bash_profile: bashrc.dot
	ln -sf $(path_from_home)/$< $@

### .config rules

# The rule tries to remove the existing Terminal target which can be either
# a synlink or a system-created directory
xfce_term_conf := $(HOME)/.config/xfce4/terminal/terminalrc
clean_files += $(xfce_term_conf)
all: $(xfce_term_conf)
$(xfce_term_conf): xfce-terminal.conf
	mkdir -p $(dir $@)
	cp -f $< $@

clean_files += $(HOME)/.config/lxterminal/lxterminal.conf
all: $(HOME)/.config/lxterminal/lxterminal.conf
$(HOME)/.config/lxterminal/lxterminal.conf: lxterminal.conf
	mkdir -p $(dir $@)
	cp -f $< $@

all: $(HOME)/.config/autostart/autostart.desktop
clean_files += $(HOME)/.config/autostart/autostart.desktop
clean_dirs += $(HOME)/.config/autostart

$(HOME)/.config/autostart:
	mkdir -p $@

$(HOME)/.config/autostart/autostart.desktop: autostart.desktop | $(HOME)/.config/autostart
	cp -f $< $@

all: $(HOME)/.local/share/applications/u-term.desktop
clean_files += $(HOME)/.local/share/applications/u-term.desktop
clean_dirs += $(HOME)/.local/share/applications

$(HOME)/.local/share/applications:
	mkdir -p $@

$(HOME)/.local/share/applications/u-term.desktop: u-term.desktop | $(HOME)/.local/share/applications
	cp -f $< $@


### emacs rules

-include $(top)/emacs/home.mk

### .vnc rules
all: $(HOME)/.vnc/xstartup
clean_files += $(HOME)/.vnc/xstartup

$(HOME)/.vnc:
	mkdir $@
	chmod 700 $(@)

$(HOME)/.vnc/xstartup : | $(HOME)/.vnc

$(HOME)/.vnc/xstartup : u-xstartup-vnc
	ln -sf $(CURDIR)/$< $@

-include $(top)/gnupg/home.mk

### env rules
all: $(HOME)/.env
clean_files += $(HOME)/.env
$(HOME)/.env: u-build-env
	bash $< $@

###  clean rules

clean:
	rm -f $(clean_files)
	for i in $(clean_dirs); do test -d $$i && rmdir --ignore-fail-on-non-empty $$i; done
