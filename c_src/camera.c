
/******************************************************************************
* PRU_memAccess_DDR_PRUsharedRAM.c
*
* The PRU reads three values from external DDR memory and stores these values 
* in shared PRU RAM using the programmable constant table entries.  The example 
* initially loads 3 values into the external DDR RAM.  The PRU configures its 
* Constant Table Programmable Pointer Register 0 and 1 (CTPPR_0, 1) to point 
* to appropriate locations in the DDR memory and the PRU shared RAM.  The 
* values are then read from the DDR memory and stored into the PRU shared RAM 
* using the values in the 28th and 31st entries of the constant table.
*
******************************************************************************/


/******************************************************************************
* Include Files                                                               *
******************************************************************************/

// Standard header files
#include <stdio.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>

// Driver header file
#include "prussdrv.h"
#include <pruss_intc_mapping.h>

#include "pru_camera_bin.h"

/******************************************************************************
* Explicit External Declarations                                              *
******************************************************************************/

/******************************************************************************
* Local Macro Declarations                                                    *
******************************************************************************/

#define PRU_NUM 	 1

#define PRUSS0_SHARED_DATARAM    4

/******************************************************************************
* Local Typedef Declarations                                                  *
******************************************************************************/


/******************************************************************************
* Local Function Declarations                                                 *
******************************************************************************/

static int LOCAL_exampleInit ( );
static unsigned short LOCAL_examplePassed ();

/******************************************************************************
* Local Variable Definitions                                                  *
******************************************************************************/


/******************************************************************************
* Intertupt Service Routines                                                  *
******************************************************************************/


/******************************************************************************
* Global Variable Definitions                                                 *
******************************************************************************/

static void *ddrMem, *sharedMem;

static unsigned int *sharedMem_int;
static unsigned int *ddrMem_int;

/******************************************************************************
* Global Function Definitions                                                 *
******************************************************************************/

int main (void)
{
    unsigned int ret;
    tpruss_intc_initdata pruss_intc_initdata = PRUSS_INTC_INITDATA;
    
    printf("\nINFO: Starting %s example.\n", "PRU_memAccess_DDR_PRUsharedRAM");
    /* Initialize the PRU */
    prussdrv_init ();		
    
    /* Open PRU Interrupt */
    ret = prussdrv_open(PRU_EVTOUT_1);
    if (ret)
    {
        printf("prussdrv_open open failed\n");
        return (ret);
    }
    
    /* Get the interrupt initialized */
    prussdrv_pruintc_init(&pruss_intc_initdata);

    /* Initialize example */
    printf("\tINFO: Initializing example.\n");
    LOCAL_exampleInit();
    
    /* Execute example on PRU */
    printf("\tINFO: Executing example.\n");
    prussdrv_pru_disable(PRU_NUM);
    prussdrv_pru_write_memory(PRUSS0_PRU1_IRAM, 0, PRUcode, sizeof(PRUcode));
    prussdrv_pru_enable(PRU_NUM);

    /* Wait until PRU0 has finished execution */
    printf("\tINFO: Waiting for HALT command.\n");
    prussdrv_pru_wait_event (PRU_EVTOUT_1);
    printf("\tINFO: PRU completed transfer.\n");
    prussdrv_pru_clear_event (PRU1_ARM_INTERRUPT);

    /* Check if example passed */
    if ( LOCAL_examplePassed() )
    {
        printf("Example executed succesfully.\n");
    }
    else
    {
        printf("Example failed.\n");
    }
    
    /* Disable PRU and close memory mapping*/
    prussdrv_pru_disable(PRU_NUM); 
    prussdrv_exit ();

    return(0);
}

/*****************************************************************************
* Local Function Definitions                                                 *
*****************************************************************************/

#define DDR_MIN_SIZE (1024*1024)

static int LOCAL_exampleInit (  )
{
    prussdrv_map_extmem(&ddrMem);

    if (prussdrv_extmem_size() < DDR_MIN_SIZE) {
        printf("extmem_size not large enough. Check that uio_pruss.extram_pool_sz=0x%x or larger.\n", DDR_MIN_SIZE);
        return -1;
    }

    ddrMem_int = (unsigned int*) ddrMem;
    memset(ddrMem, 0, DDR_MIN_SIZE);

    /* Allocate Shared PRU memory. */
    prussdrv_map_prumem(PRUSS0_SHARED_DATARAM, &sharedMem);
    sharedMem_int = (unsigned int*) sharedMem;

    // Tell the PRU where the DDR buffer lives
    sharedMem_int[0] = prussdrv_get_phys_addr(ddrMem);

    return(0);
}

static unsigned short LOCAL_examplePassed()
{
    unsigned int pixel_count = ddrMem_int[0];
    unsigned int line_count = ddrMem_int[1];

    printf("pixel_count=%d, line_count=%d\n", pixel_count, line_count);
    if (pixel_count != 360960 || line_count != 480) {
        return 0;
    }

    FILE *fp = fopen("/tmp/output.raw", "wb");
    fwrite(&ddrMem_int[3], 1, pixel_count, fp);
    fclose(fp);

    return 1;
}
