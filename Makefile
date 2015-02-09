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
ML_MAKEDEPEND ?= ml-makedepend
SMLNJ_SUFFIX := $(shell echo 'print(SMLofNJ.SysInfo.getHeapSuffix()^"\n");' | $(SMLNJ) | head -5 | tail -1)

all: smlnj


.PHONY: smlnj
smlnj:
	@echo "  [CM] hpdf.cm"
	@echo 'CM.make "smlnj/hpdf.cm";' | $(SMLNJ)


%.cm.d: %.cm
	@echo "  GEN [$@]"
	@touch $@
	@$(ML_MAKEDEPEND) -f $@ $< $(<:.cm=).$(SMLNJ_SUFFIX)


.PHONY: runsample
runsample: sample/arc_demo.$(SMLNJ_SUFFIX)
	$(SMLNJ) @SMLload=sample/arc_demo arc_demo


sample/arc_demo.$(SMLNJ_SUFFIX): smlnj sample/arc_demo.cm
	@$(MLBUILD) sample/arc_demo.cm ArcDemo.main $@


ifeq (,$(findstring $(MAKECMDGOALS),clean))
-include sample/arc_demo.cm.d
endif


.PHONY: clean
clean:
	-make -C $(SMLNJ_DIR) clean
	-rm -rf .cm
	-rm -rf sample/.cm
	-rm sample/arc_demo.cm.d
	-rm sample/arc_demo.$(SMLNJ_SUFFIX)

