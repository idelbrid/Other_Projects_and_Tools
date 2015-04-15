################################################################################
# Automatically-generated file. Do not edit!
################################################################################

# Add inputs and outputs from these tool invocations to the build variables 
C_SRCS += \
../CBparse_main.c \
../char_classes.c \
../html.c \
../keywords.c \
../parser.c \
../reader.c \
../scanner.c 

OBJS += \
./CBparse_main.o \
./char_classes.o \
./html.o \
./keywords.o \
./parser.o \
./reader.o \
./scanner.o 

C_DEPS += \
./CBparse_main.d \
./char_classes.d \
./html.d \
./keywords.d \
./parser.d \
./reader.d \
./scanner.d 


# Each subdirectory must supply rules for building sources it contributes
%.o: ../%.c
	@echo 'Building file: $<'
	@echo 'Invoking: Cross GCC Compiler'
	gcc -O0 -g3 -Wall -c -fmessage-length=0 -MMD -MP -MF"$(@:%.o=%.d)" -MT"$(@:%.o=%.d)" -o "$@" "$<"
	@echo 'Finished building: $<'
	@echo ' '


