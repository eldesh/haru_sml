####################################################################################
#
# libHaru/FFI for SML/NJ
#
####################################################################################

SMLNJ ?= sml
NLFFIGEN ?= ml-nlffigen
FFI_DIR ?= FFI
LIBH_SML ?= libh.sml
HPDF_INCLUDE_DIR ?= /usr/include
HPDF_SHARED_LIB ?= /usr/lib/libhpdf.so

## suppress errors
CPP_FLAGS := '-D__builtin_va_list=int'

HPDF_API_CM := $(FFI_DIR)/libharu.cm

all: $(HPDF_API_CM)


$(LIBH_SML): ../libh.sml.in
	@echo "  GEN [$(notdir $@)]"
	@sed -e "s|@SHARED_LIB@|$(HPDF_SHARED_LIB)|" $< > $@


$(HPDF_API_CM): $(LIBH_SML)
	@echo "  FFI [$(notdir $@)]"
	@$(NLFFIGEN) -allSU -cppopt $(CPP_FLAGS) -I$(HPDF_INCLUDE_DIR) \
	   	-dir $(FFI_DIR)/ -include ../$(LIBH_SML) \
		-cmfile $(notdir $@) -match '.*hpdf.*\.h' \
		-enum-constructors $(HPDF_INCLUDE_DIR)/hpdf.h


.PHONY: clean
clean:
	$(RM) -r .cm
	$(RM) -r $(FFI_DIR)
	$(RM) $(LIBH_SML)

