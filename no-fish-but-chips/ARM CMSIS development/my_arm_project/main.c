#include <stdio.h>
#include "ARMCM4_FP.h" // Device header for Cortex-M4 with FPU

// Overriding the default handler defined in the startup file
void Interrupt0_Handler(void) {
    printf("Interrupt 0 triggered successfully!\n");
}

int main(void) {
    // SystemCoreClockUpdate() is a CMSIS standard function to 
    // sync the SystemCoreClock variable with actual hardware state.
    SystemCoreClockUpdate();

    printf("Starting CMSIS Demo on macOS...\n");

    /* Initialize Interrupt0_IRQn */
    NVIC_SetPriority(Interrupt0_IRQn, 1);
    NVIC_EnableIRQ(Interrupt0_IRQn);

    /* Manually set the interrupt to pending state */
    printf("Setting Interrupt 0 to pending...\n");
    NVIC_SetPendingIRQ(Interrupt0_IRQn);

    while(1) {
        __WFI(); // Wait For Interrupt (power-saving)
    }

    return 0;
}
