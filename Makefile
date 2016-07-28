OUTPUT_DIR ?= doc

TITLE := Core Erlang 1.0.3

SRC_DIR  := Language/CoreErlang
LHS_SRCS := $(notdir $(wildcard $(SRC_DIR)/*.lhs))
PDFS     := $(LHS_SRCS:.lhs=.pdf)

PANDOC_FLAGS = -f latex+lhs -s -V title='$(TITLE)'
ifeq ($(DEBUG),1)
	PANDOC_FLAGS += --verbose
endif

all: pdfs

pdfs: $(addprefix $(OUTPUT_DIR)/, $(PDFS))
$(OUTPUT_DIR)/%.pdf: export TITLE = $(subst /,.,$(<:.lhs=))
$(OUTPUT_DIR)/%.pdf: $(SRC_DIR)/%.lhs
	pandoc $(PANDOC_FLAGS) -o $@ $<
