# Toolchain path
CC = arm-none-eabi-gcc
AS = arm-none-eabi-gcc
LD = arm-none-eabi-gcc

# Path to your CMSIS 5.9.0 directory
CMSIS_PATH = ./cmsis

# Compiler Flags
# -mcpu=cortex-m4: Targets the M4 core
# -mthumb: Generates Thumb-2 instructions
# -mfloat-abi=hard -mfpu=fpv4-sp-d16: Enables the FPU
CFLAGS = -mcpu=cortex-m4 -mthumb -mfloat-abi=hard -mfpu=fpv4-sp-d16 -O0 -g3
CFLAGS += -DARMCM4_FP  # Matches the macro in the document
CFLAGS += -I$(CMSIS_PATH)/CMSIS/Core/Include
CFLAGS += -I$(CMSIS_PATH)/Device/ARM/ARMCM4/Include

# Linker Flags
# --specs=rdimon.specs -lc -lrdimon: Enables Semihosting for printf
LDFLAGS = -T$(CMSIS_PATH)/Device/ARM/ARMCM4/Source/GCC/gcc_arm.ld
LDFLAGS += --specs=rdimon.specs -lc -lrdimon 

# Source Files
SRCS = main.c \
       $(CMSIS_PATH)/Device/ARM/ARMCM4/Source/system_ARMCM4.c \
       $(CMSIS_PATH)/Device/ARM/ARMCM4/Source/GCC/startup_ARMCM4.S

OBJS = $(SRCS:.c=.o)
OBJS := $(OBJS:.S=.o)

all: image.elf

image.elf: $(OBJS)
	$(LD) $(CFLAGS) $(OBJS) $(LDFLAGS) -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

%.o: %.S
	$(AS) $(CFLAGS) -c $< -o $@

clean:
	rm -f *.o image.elf
