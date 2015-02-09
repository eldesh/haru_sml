####################################################################################
#
# libHaru binding for SML/NJ
#
####################################################################################

export SMLNJ ?= sml
export NLFFIGEN ?= ml-nlffigen
export FFI_DIR ?= FFI
export LIBH_SML ?= libh.sml
export HPDF_INCLUDE_DIR ?= /usr/include
export HPDF_SHARED_LIB ?= /usr/lib/libhpdf.so

SMLNJ_DIR ?= smlnj
MLBUILD ?= ml-build
SMLNJ_SUFFIX := $(shell echo 'print(SMLofNJ.SysInfo.getHeapSuffix()^"\n");' | $(SMLNJ) | head -5 | tail -1)

all: smlnj


.PHONY: smlnj
smlnj:
	@echo "  [CM] hpdf.cm"
	@echo 'CM.make "smlnj/hpdf.cm";' | $(SMLNJ)

.PHONY: clean
clean:
	-make -C $(SMLNJ_DIR) clean
	-rm -rf .cm

