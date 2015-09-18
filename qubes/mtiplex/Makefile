.PHONY: all clean

all:
clean:


clean_files :=
clean_dirs :=

build_dir := obj

clean_files += $(build_dir)/mtiplex

$(build_dir)/mtiplex : mtiplex.c | $(build_dir)
	gcc -o $@ -g -Wall -Wextra -levent $^

$(build_dir):
	mkdir -p $(build_dir)

clean_dirs += $(build_dir)

all: $(build_dir)/mtiplex

clean:
	rm -f $(clean_files)
	for i in $(clean_dirs); do test -d $$i && rmdir $$i > /dev/null 2>&1 ; done
 
