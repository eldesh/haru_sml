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
ARCH ?= x86
OS ?= unix

SMLNJ_DIR ?= smlnj
MLBUILD ?= ml-build
ML_MAKEDEPEND ?= ml-makedepend
SMLNJ_SUFFIX := $(shell /bin/echo 'print(SMLofNJ.SysInfo.getHeapSuffix() ^ "\n");' | $(SMLNJ) | head -5 | tail -1)

SAMPLE_DIR := sample
SAMPLES := arc_demo text_demo
SAMPLE_BINS := $(addprefix $(SAMPLE_DIR)/,$(SAMPLES:=.$(SMLNJ_SUFFIX)))

all: smlnj


.PHONY: smlnj
smlnj:
	@echo "  CM [hpdf.cm]"
	@echo 'CM.make "$(SMLNJ_DIR)/hpdf.cm";' | $(SMLNJ)


%.cm.d: %.cm
	@echo "  GEN [$@]"
	@touch $@
	@$(ML_MAKEDEPEND) -f $@ $< -a $(ARCH) -o $(OS) $(<:.cm=).$(SMLNJ_SUFFIX)


.PHONY: runsample
runsample: $(SAMPLE_BINS)
	@for s in $?; do \
		echo "  DEMO [$${s%.$(SMLNJ_SUFFIX)}]"; \
		$(SMLNJ) @SMLload=$$s $${s%.$(SMLNJ_SUFFIX)}; \
	done


$(SAMPLE_BINS): %.$(SMLNJ_SUFFIX): %.cm smlnj
	@echo "  BUILD [$(notdir $*)]"
	@$(MLBUILD) $< Demo.main $@


ifeq (,$(findstring $(MAKECMDGOALS),clean))
-include $(SAMPLE_BINS:.$(SMLNJ_SUFFIX)=.cm.d)
endif


.PHONY: clean
clean:
	-make -C $(SMLNJ_DIR) clean
	-rm -rf .cm
	-rm -rf sample/.cm
	-rm $(SAMPLE_BINS)
	-rm $(SAMPLE_BINS:.$(SMLNJ_SUFFIX)=.cm.d)
	-rm $(SAMPLE_BINS:.$(SMLNJ_SUFFIX)=.pdf)

