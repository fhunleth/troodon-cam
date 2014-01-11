#include <stdio.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <err.h>

// Driver header file
#include "prussdrv.h"
#include <pruss_intc_mapping.h>

#include "pru_camera_bin.h"

#define PRU_NUM 	 1
#define PRUSS0_SHARED_DATARAM    4

static int LOCAL_exampleInit ( );
static unsigned short LOCAL_examplePassed ();

static void *ddrMem, *sharedMem;
static unsigned int *sharedMem_int;
static unsigned int *ddrMem_int;

int main()
{
    fprintf(stderr, "Going to take a snapshot from the troodon-cam...\n");

    /* Initialize the PRU */
    prussdrv_init();

    /* Open PRU Interrupt */
    unsigned int ret = prussdrv_open(PRU_EVTOUT_1);
    if (ret)
	errx(EXIT_FAILURE, "prussdrv_open: %d", ret);

    /* Get the interrupt initialized */
    tpruss_intc_initdata pruss_intc_initdata = PRUSS_INTC_INITDATA;
    prussdrv_pruintc_init(&pruss_intc_initdata);

    /* Initialize example */
    fprintf(stderr, "\tINFO: Initializing example.\n");

    LOCAL_exampleInit();

    /* Execute example on PRU */
    fprintf(stderr, "\tINFO: Executing example.\n");
    prussdrv_pru_disable(PRU_NUM);
    prussdrv_pru_write_memory(PRUSS0_PRU1_IRAM, 0, PRUcode, sizeof(PRUcode));
    prussdrv_pru_enable(PRU_NUM);

    /* Wait until PRU0 has finished execution */
    fprintf(stderr, "\tINFO: Waiting for HALT command.\n");
    prussdrv_pru_wait_event (PRU_EVTOUT_1);
    fprintf(stderr, "\tINFO: PRU completed transfer.\n");
    prussdrv_pru_clear_event (PRU1_ARM_INTERRUPT);

    /* Check if example passed */
    if ( LOCAL_examplePassed() )
        fprintf(stderr, "Example executed successfully.\n");
    else
        fprintf(stderr, "Example failed.\n");

    /* Disable PRU and close memory mapping*/
    prussdrv_pru_disable(PRU_NUM);
    prussdrv_exit ();

    return(0);
}

/*****************************************************************************
* Local Function Definitions                                                 *
*****************************************************************************/

#define DDR_MIN_SIZE (1024*1024)

static int LOCAL_exampleInit()
{
    prussdrv_map_extmem(&ddrMem);

    if (prussdrv_extmem_size() < DDR_MIN_SIZE)
        errx(EXIT_FAILURE, "extmem_size not large enough. Check that uio_pruss.extram_pool_sz=0x%x or larger.", DDR_MIN_SIZE);

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

    fprintf(stderr, "pixel_count=%d, line_count=%d\n", pixel_count, line_count);
    if (pixel_count != 360960 || line_count != 480)
        return 0;

    FILE *fp = fopen("/tmp/output.pgm", "wb");
    fprintf(fp, "P5\n%d %d 255\n", pixel_count / line_count, line_count);
    fwrite(&ddrMem_int[3], 1, pixel_count, fp);
    fclose(fp);

    return 1;
}
