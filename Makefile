default: main
.PHONY: default

PUREC_DIR := ../purec
export PATH := $(PUREC_DIR)/node_modules/.bin:$(PATH)
include $(PUREC_DIR)/mk/target.mk

$(eval $(call purs_mk_target,main,Main,src))
