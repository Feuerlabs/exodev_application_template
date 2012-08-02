# Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
#
# This Source Code Form is subject to the terms of the Mozilla Public
# License, v. 2.0. If a copy of the MPL was not distributed with this
# file, You can obtain one at http://mozilla.org/MPL/2.0/.


#
# After you have setup the cross compile toolchain (see exodev_build.config),
# run the following commands:
# 
#   make otp - Build the Erlang OTP for cross compiling port drivers
#
#   make compile - Download deps (rebar.config) and build them. 
#                  Build self's app
#
#   make release - Run rebar create-node. Ignore error about existing 
#                  reltool.config
#
#   make generate - Generate a release and install it under rel
#
# To build for the target architecture.
#
#   make clean - Clean out build machine libraries and binaries. 
#                Clean out release files.
#
#   make target-compile - Download deps build them for target. 
#                         Build self's app for target
#
#   make release - Run rebar create-node for the target.  Ignore error 
#                  about existing reltool.config
#
#   make target-generate - Generate a release and install it under rel.
#
#   cd rel; tar czf /tmp/rel.tgz [release] - Tar up the release to be 
#                                            transferred to target.
#                         
#
# NOTE: When cross-compiling, make sure that you don't have any
#       of the modules in ./deps also present in the parent directory
#       of this project. In that case, the parent directory's 
#       project will be used by make target-generate and you
#       will have .so files compiled for the build system 
#       in your rel/[release] directory.
#       
#
# NOTE: When running the app, either on the target system or on
#       the build box, make sure that you do it using the following
#       command: 
# 
#         LD_LIBRARY_PATH=./$(RELNAME)/lib/netlink-1/priv ./$(RELNAME)/bin/exodev_template start
#      
#       The netlink module is dynamically linked cross compiled libnl 
#       libraries stored in lib/netlink-1/priv. These cannot be found
#       by the erlang runtime unless LD_LIBRARY_PATH is setup correctly.
#       If you have a fix for this, please mail me at magnus@feuerlabs.com
#      
# Configuration file for the entire build system.
# Copy exodev_build.config.sample to exodev_build.config and edit
# according to instructions
#
EXODEV_BUILD_CFG_FILE="exodev_build.config"

#
# Name of release. Retrieved from exodev_build.config
#
RELNAME?=$(shell ./get_build_config_val $(EXODEV_BUILD_CFG_FILE) release_name 2>/tmp/otp_err0)

#
# Retrieve the various config values from the exodev_build.config file above.
# and setup our local build environment
# See the config file for details.
#

# Root directory of the cross compile tool chain is.
X_COMP_TOOLCHAIN_ROOT:=$(shell ./get_build_config_val $(EXODEV_BUILD_CFG_FILE) x_comp_toolchain_root 2>/tmp/otp_err1)

# Target architecture for the cross compile
X_COMP_TARGET_ARCH:=$(shell ./get_build_config_val $(EXODEV_BUILD_CFG_FILE) x_comp_target_arch 2>/tmp/otp_err2)

# Target architecture for the cross compile
X_COMP_BUILD_ARCH:=$(shell ./get_build_config_val $(EXODEV_BUILD_CFG_FILE) x_comp_build_arch 2>/tmp/otp_err3)

# URL where we can find OTP source tar balls.
X_COMP_OTP_DOWNLOAD_URL:=$(shell ./get_build_config_val $(EXODEV_BUILD_CFG_FILE) x_comp_otp_download_url 2>/tmp/otp_err4)

# Which file to download from the URL above.
X_COMP_OTP_DOWNLOAD_FILE:=$(shell ./get_build_config_val $(EXODEV_BUILD_CFG_FILE) x_comp_otp_download_file 2>/tmp/otp_err5)

# Linux kernel headers for the kernel that runs on target.
X_COMP_KERNEL_HEADERS:=$(shell ./get_build_config_val $(EXODEV_BUILD_CFG_FILE) x_comp_kernel_headers 2>/tmp/otp_err6)

# Which file to download from the URL above.
X_COMP_OTP_C_FLAGS:=$(shell ./get_build_config_val $(EXODEV_BUILD_CFG_FILE) x_comp_c_flags 2>/dev/null)

X_COMP_OTP_LD_FLAGS:=$(shell ./get_build_config_val $(EXODEV_BUILD_CFG_FILE) x_comp_ld_flags 2>/dev/null)

# Directory where the OTP source tar ball is unpacked into.
X_COMP_OTP_BUILD_ROOT:=$(shell basename "$(X_COMP_OTP_DOWNLOAD_FILE)" .tar.gz)

# Where is the built erlang OTP installed.
X_COMP_OTP_INSTALL_ROOT=$(PWD)/erl-$(X_COMP_TARGET_ARCH)

# Target indicating that the entire OTP has been installed.
X_COMP_OTP_INSTALLED=$(X_COMP_OTP_INSTALL_ROOT)/lib/erlang/bin/erl

# Target indicating that the target OTP has been built.
# We use the beam binary in the target arch subdirectory under the bin directory
X_COMP_OTP_X_BUILT=$(X_COMP_OTP_BUILD_ROOT)/bin/$(X_COMP_TARGET_ARCH)/beam

# Target indicating that the OTP has been built for the build host.
# We use the beam binary in the self's arch subdirectory under the bin directory
X_COMP_OTP_BUILT=$(X_COMP_OTP_BUILD_ROOT)/bin/$(X_COMP_BUILD_ARCH)/beam

# Target indicating that the OTP has been built for the build host.
# We use the readme file in the OTP root.
X_COMP_OTP_UNPACKED=$(X_COMP_OTP_BUILD_ROOT)/README.md


# URL where we can find OPENSSL source tar balls.
X_COMP_OPENSSL_DOWNLOAD_URL:=$(shell ./get_build_config_val $(EXODEV_BUILD_CFG_FILE) x_comp_openssl_download_url 2>/tmp/otp_err4)

# Which file to download from the URL above.
X_COMP_OPENSSL_DOWNLOAD_FILE:=$(shell ./get_build_config_val $(EXODEV_BUILD_CFG_FILE) x_comp_openssl_download_file 2>/tmp/otp_err5)

# Directory where the OPENSSL source tar ball is unpacked into.
X_COMP_OPENSSL_BUILD_ROOT:=$(shell basename "$(X_COMP_OPENSSL_DOWNLOAD_FILE)" .tar.gz)

# Where is the built erlang OPENSSL installed.
X_COMP_OPENSSL_INSTALL_ROOT=$(PWD)/openssl-$(X_COMP_TARGET_ARCH)

# Target indicating that the entire OPENSSL has been installed.
X_COMP_OPENSSL_INSTALLED=$(X_COMP_OPENSSL_INSTALL_ROOT)/lib/libcrypto.a

# Target indicating that that openssl has been configured
X_COMP_OPENSSL_CONFIGURED=$(X_COMP_OPENSSL_BUILD_ROOT)/Makefile

# Target indicating that the OPENSSL has been built for the build host.
# We use the beam binary in the self's arch subdirectory under the bin directory
X_COMP_OPENSSL_BUILT=$(X_COMP_OPENSSL_BUILD_ROOT)/libcrypto.a

# Target indicating that the OPENSSL has been built for the build host.
# We use the readme file in the OPENSSL root.
X_COMP_OPENSSL_UNPACKED=$(X_COMP_OPENSSL_BUILD_ROOT)/.unpacked


.PHONY: all compile clean release upgrade otp check_build_config check_x_compile

all: compile

compile:
	EXODEV_COMP=1 ./rebar get-deps
	EXODEV_COMP=1 ./rebar compile

recompile:
	EXODEV_COMP=1 ./rebar compile

target-compile: check_x_compile check_build_config otp
	EXODEV_COMP=1 ./rebar get-deps
	EXODEV_COMP=1 \
	X_COMP_TARGET_ARCH=$(X_COMP_TARGET_ARCH) \
	CC=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-gcc \
	CXX=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-g++ \
	LD=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-ld \
	RANLIB=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-ld \
	AR=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-ar \
	ERL_EI_LIBDIR=$(X_COMP_OTP_INSTALL_ROOT)/lib/erlang/lib/erl_interface-3.7.7/lib \
	CFLAGS="-I$(X_COMP_KERNEL_HEADERS) -I$(X_COMP_OTP_INSTALL_ROOT)/lib/erlang/lib/erl_interface-3.7.7/include -I$(X_COMP_OTP_INSTALL_ROOT)/lib/erlang/erts-5.9.1/include" \
	rebar compile

release: check_release_name
	cd rel; ../rebar create-node skip_deps=true nodeid=$(RELNAME)

target-generate:
	X_COMP_OTP_INSTALL_ROOT=$(X_COMP_OTP_INSTALL_ROOT)/lib/erlang ./rebar generate -f skip_deps=true

generate:
	./rebar generate -f skip_deps=true
#
# Rebar only copies deps/*/priv/*.so files, not files that end in .so.*.
# Since netlib has .so.200 and other file suffixes, we need to copy those
# out manually. Please remove this one once rebar does the right thing.
#
	-@cp -u ./deps/netlink/priv/* ./rel/$(RELNAME)/lib/netlink-1/priv/
#	./exorel current `./exorel last_build`

clean: relclean
	./rebar clean


relclean:
	rm -rf rel/files
	rm -rf rel/$(RELNAME)


check_release_name:
ifeq ($(RELNAME), )
	$(error $(shell cat /tmp/otp_err0))
endif


#
# OTP CROSS COMPILE STUFF
#
#
# Check that the config file $(EXODEV_BUILD_CFG_FILE) exists and that all
# required values could be read.
#
check_build_config:
ifneq ($(shell ./get_build_config_val $(EXODEV_BUILD_CFG_FILE) 2> /tmp/otp_err), ok)
	$(error $(shell cat /tmp/otp_err))
endif
ifeq ($(X_COMP_TOOLCHAIN_ROOT), )
	$(error $(shell cat /tmp/otp_err1))
endif

ifeq ($(X_COMP_KERNEL_HEADERS), )
	$(error $(shell cat /tmp/otp_err6))
endif

ifeq ($(X_COMP_TARGET_ARCH), )
	$(error $(shell cat /tmp/otp_err2))
endif

ifeq ($(X_COMP_BUILD_ARCH), )
	$(error $(shell cat /tmp/otp_err3))
endif

ifeq ($(X_COMP_OTP_DOWNLOAD_URL), )
	$(error $(shell cat /tmp/otp_err4))
endif

ifeq ($(X_COMP_OTP_DOWNLOAD_FILE), )
	$(error $(shell cat /tmp/otp_err5))
endif
	@rm -f /tmp/otp_err*


#
# Check that the cross compile toolkit is in place
#
check_x_compile: 
ifneq ($(shell /bin/ls $(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-gcc 2>/dev/null), \
			$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-gcc)
	$(error Could not locate gcc compiler at $(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-gcc.\
		Check $(EXODEV_BUILD_CFG_FILE) and its x_comp_toolchain_root entry)
endif

ifneq ($(shell /bin/ls $(X_COMP_KERNEL_HEADERS)/linux/errno.h 2>/dev/null), \
			$(X_COMP_KERNEL_HEADERS)/linux/errno.h)
	$(error Could not locate linux kernel header file $(X_COMP_KERNEL_HEADERS)/include/linux/errno.h.\
		 Check $(EXODEV_BUILD_CFG_FILE) and its x_comp_kernel_headers entry)
endif


otp: check_build_config $(X_COMP_OPENSSL_INSTALLED) $(X_COMP_OTP_INSTALLED)

# Install the cross compiled OTP system.
$(X_COMP_OTP_INSTALLED): $(X_COMP_OTP_X_BUILT)
	(cd $(X_COMP_OTP_BUILD_ROOT); make install)

# Do a cross compile build of the OTP
$(X_COMP_OTP_X_BUILT): $(X_COMP_OTP_BUILT)
	(cd $(X_COMP_OTP_BUILD_ROOT); \
		CC=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-gcc \
		CPP=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-cpp \
		CXX=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-g++ \
		LD=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-ld \
		AR=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-ar \
		RANLIB=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-ranlib \
		erl_xcomp_sysroot=$(X_COMP_OPENSSL_INSTALL_ROOT) \
		./configure --prefix=$(X_COMP_OTP_INSTALL_ROOT) \
				--with-ssl=$(X_COMP_OPENSSL_INSTALL_ROOT) \
				--without-termcap \
				--host=$(X_COMP_TARGET_ARCH) \
				--build=$(X_COMP_BUILD_ARCH));
	(cd $(X_COMP_OTP_BUILD_ROOT); \
		CC=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-gcc \
		CPP=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-cpp \
		CXX=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-g++ \
		LD=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-ld \
		AR=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-ar \
		RANLIB=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-ranlib \
		make)

# Do bootstrap build of the OTP in preparation for the cross compile stage
$(X_COMP_OTP_BUILT): $(X_COMP_OTP_UNPACKED)
	(cd $(X_COMP_OTP_BUILD_ROOT); ./configure --enable-bootstrap-only)
	(cd $(X_COMP_OTP_BUILD_ROOT); make)

# Unpack the downloaded OTP source tar ball
$(X_COMP_OTP_UNPACKED): $(X_COMP_OTP_DOWNLOAD_FILE)
	tar xf $(X_COMP_OTP_DOWNLOAD_FILE)
	touch $(X_COMP_OTP_UNPACKED)

# Target indicating that the OTP tar ball has been downloaded
$(X_COMP_OTP_DOWNLOAD_FILE):
# Donwload the OTP source tar ball
	wget $(X_COMP_OTP_DOWNLOAD_URL)/$(X_COMP_OTP_DOWNLOAD_FILE)

#
# Download and unpack openssl
#
openssl: check_build_config $(X_COMP_OPENSSL_INSTALLED)

# Install the cross compiled OPENSSL system.
$(X_COMP_OPENSSL_INSTALLED): $(X_COMP_OPENSSL_BUILT)
	(cd $(X_COMP_OPENSSL_BUILD_ROOT); make install)

# Do a cross compile build of the OPENSSL

$(X_COMP_OPENSSL_BUILT): $(X_COMP_OPENSSL_CONFIGURED)
	(cd $(X_COMP_OPENSSL_BUILD_ROOT); \
		CC=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-gcc \
		CPP=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-cpp \
		CXX=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-g++ \
		LD=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-ld \
		AR=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-ar \
		RANLIB=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-ranlib \
		make)

# Configure OpenSSL
$(X_COMP_OPENSSL_CONFIGURED): $(X_COMP_OPENSSL_UNPACKED)
	(cd $(X_COMP_OPENSSL_BUILD_ROOT); \
		CC=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-gcc \
		CPP=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-cpp \
		CXX=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-g++ \
		LD=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-ld \
		AR=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-ar \
		RANLIB=$(X_COMP_TOOLCHAIN_ROOT)/bin/$(X_COMP_TARGET_ARCH)-ranlib \
		./Configure dist --prefix=$(X_COMP_OPENSSL_INSTALL_ROOT) \
				shared \
				no-zlib)

# Unpack the downloaded OPENSSL source tar ball
$(X_COMP_OPENSSL_UNPACKED): $(X_COMP_OPENSSL_DOWNLOAD_FILE)
	tar xf $(X_COMP_OPENSSL_DOWNLOAD_FILE)
	touch $(X_COMP_OPENSSL_UNPACKED)

# Target indicating that the OPENSSL tar ball has been downloaded
$(X_COMP_OPENSSL_DOWNLOAD_FILE):
# Donwload the OPENSSL source tar ball
	wget $(X_COMP_OPENSSL_DOWNLOAD_URL)/$(X_COMP_OPENSSL_DOWNLOAD_FILE)
