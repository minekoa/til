TARGET ?= a.out

SRCS := ./src/main.cpp
OBJS := $(addsuffix .o,$(basename $(SRCS)))
DEPS := $(OBJS:.o=.d)

INC_DIRS := $(shell find $(SRC_DIRS) -type d)
INC_FLAGS := $(addprefix -I,$(INC_DIRS))

CPPFLAGS ?= $(INC_FLAGS) -MMD -MP -std=c++1z -Wall

CC = g++

$(info SRCS= $(SRCS))
$(info OBJS= $(OBJS))

$(TARGET): $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) -o $@ $(LOADLIBES) $(LDLIBS)

.PHONY: all clean

all: $(TARGET)

clean:
	$(RM) $(TARGET) $(OBJS) $(DEPS)

run: $(TARGET)
	/usr/bin/time -f "%M(KB) e=%E s=%S u=%U" ./a.out 7000000
	ls -lh




-include $(DEPS)


