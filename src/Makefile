include ../vsn.mk

# ----------------------------------------------------
# VARIABLES
# ----------------------------------------------------

APP_NAME = meadow

MODULES= \
	emd_cfg \
	emd_log \
	emd_erlang \
	emd_lib \
	emd_json

INCLUDES= \
	../include


TARGET_FILES = $(APP_TARGET) $(EMAKE_TARGET)

include ../../meadow/priv/Makefile.vars

# ----------------------------------------------------
# BUILD TARGETS
# ----------------------------------------------------
all: $(TARGET_FILES) 
	erl -make

clean:
	rm -f $(APP_TARGET) $(EMAKE_TARGET) ../ebin/*.beam


doc: edoc


include ../../meadow/priv/Makefile.rules

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

