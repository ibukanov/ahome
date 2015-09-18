
SHELL	= /bin/sh

SYSTYPE := $(shell uname)
ifeq ($(SYSTYPE),HP-UX)
	MATHLIB = -lM
else
	MATHLIB = -lm
endif

# C++
CXX	= g++
CXXOPTS	= -O2 -Wall -Wcast-qual -Wcast-align  -Wconversion -Wstrict-prototypes -Wmissing-declarations -Winline

# Link:
LD	= $(CXX)
LDOPTS	= -s

BUILDPREFIX = ../build/
OBJPREFIX = ../tmp/
SRCPREFIX = src/

NAME := $(BUILDPREFIX)prog


HDRS := datastruct.h \
		resources.h \
		strtonum.h \
		xfunctions.h

SRCS := datastruct.C \
		main.C \
		resources.C \
		strtonum.C \
		xfunctions.C
		
OBJS := $(patsubst %.C,$(OBJPREFIX)%.o, $(SRCS))
SRCS := $(patsubst %,$(SRCPREFIX)%, $(SRCS))
HDRS := $(patsubst %,$(SRCPREFIX)%, $(HDRS))

CALC_SCRIPT := plots/calculate
XPRM_prefix = XPRM_

all:	$(NAME)

$(NAME):	$(OBJS)
	$(LD) $(LDOPTS) $(OBJS) $(MATHLIB) -o $(NAME)

$(OBJS): $(OBJPREFIX)%.o: $(SRCPREFIX)%.C
	$(CXX) $(CXXOPTS)  -c $< -o $@

.PHONY: clean run

clean:
	rm -f $(NAME) $(OBJS) $(BUILDPREFIX)core

#run: $(total_cross_section_out) $(mass_out)
#
#$(total_cross_section_out) $(mass_out): $(CALC_SCRIPT) $(NAME)
#	NAME=$(NAME) XPRM_prefix=$(XPRM_prefix) \
#	$(XPRM_prefix)total_cross_section_out=$(total_cross_section_out) \
#	$(XPRM_prefix)mass_out=$(mass_out) \
#	$(CALC_SCRIPT)



