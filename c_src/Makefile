CURDIR := $(shell pwd)

MMDBDIR=$(CURDIR)/libmaxminddb
MMDBSRC=$(MMDBDIR)/src
MMDB_INCLUDE_DIR=$(MMDBDIR)/include
MMDB_STATIC_LIB=$(MMDBSRC)/.libs/libmaxminddb.a

SOURCES = geoip2erl.cc
C_SRC_OUTPUT=$(CURDIR)/../priv/geoip2erl

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)]).")
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)]).")

CXXFLAGS += -O3 -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I $(MMDB_INCLUDE_DIR)
LDLIBS   += -L $(ERL_INTERFACE_LIB_DIR) -lerl_interface -lei $(MMDB_STATIC_LIB)

all: $(C_SRC_OUTPUT)

$(C_SRC_OUTPUT): geoip2erl.cc $(MMDB_STATIC_LIB) ../priv
	$(CXX) -o $(C_SRC_OUTPUT) $(CXXFLAGS) $< $(LDLIBS)

$(MMDBDIR)/bootstrap:
	git submodule update --init

$(MMDBDIR)/Makefile: $(MMDBDIR)/bootstrap
	(cd $(MMDBDIR) && ./bootstrap)

$(MMDBSRC)/Makefile: $(MMDBDIR)/Makefile
	(cd $(MMDBDIR) && ./configure --enable-shared=false)

$(MMDB_STATIC_LIB): $(MMDBSRC)/Makefile
	make -C $(MMDBSRC)

clean:
	-(test -f $(MMDBDIR)/Makefile && make -C $(MMDBDIR) distclean)
	rm -f $(C_SRC_OUTPUT)
