# Define variables
SRC_DIR := src
BUILD_DIR := build
SOURCE := $(SRC_DIR)/48-pixel-sprite-with-pf0.asm
OUTPUT := $(BUILD_DIR)/48-pixel-sprite-with-pf0
DASM := dasm

# Define DASM flags
DASM_FLAGS := -f3 -o$(OUTPUT).bin -I$(SRC_DIR) -l$(OUTPUT).lst -s$(OUTPUT).sym

# Default target
all: $(OUTPUT)

# Build target
$(OUTPUT): $(SOURCE)
	mkdir -p $(BUILD_DIR)
	$(DASM) $(SOURCE) $(DASM_FLAGS)

# Clean target
clean:
	rm -rf $(BUILD_DIR)/*.bin
	rm -rf $(BUILD_DIR)/*.sym
	rm -rf $(BUILD_DIR)/*.lst

# Phony targets
.PHONY: all clean