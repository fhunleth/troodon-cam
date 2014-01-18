#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <err.h>

// Driver header file
#include "prussdrv.h"
#include <pruss_intc_mapping.h>

#include "pru_camera_bin.h"

#define DEBUG
#ifdef DEBUG
#define DEBUG_PRINTF(FORMAT, ...) fprintf(stderr, FORMAT, ## __VA_ARGS__)
#else
#define DEBUG_PRINTF(FORMAT, ...)
#endif

#define PRU_NUM 	 1
#define PRUSS0_SHARED_DATARAM    4

// See analogous data structures in pru_camera.p
struct pru_camera_config
{
    uint32_t ddr;
    uint32_t frame_size;
};

struct pru_camera_frame_header
{
    uint32_t id;
    uint32_t frame_start;
    uint32_t frame_end;
};

#define CAMERA_MAX_LINES   480
#define CAMERA_MAX_COLUMNS 752
#define CAMERA_FRAME_SIZE  (CAMERA_MAX_COLUMNS * CAMERA_MAX_LINES)

// We'd like space for the frame header and two buffers
#define DDR_MIN_SIZE (sizeof(struct pru_camera_frame_header) + 2 * CAMERA_FRAME_SIZE)

struct camera_state
{
    // DDR memory addresses from prussdrv
    // This is the large memory area where frames are stored
    void *ddr;
    unsigned int ddr_phys;
    struct pru_camera_frame_header *frame_header;

    // PRU shared memory
    // This is the small shared memory where configuration is
    // passed to the PRU program.
    void *shared;
    struct pru_camera_config *config;
};

static void camera_init(struct camera_state *state)
{
    // Initialize the PRU
    prussdrv_init();

    // Open PRU Interrupt
    unsigned int ret = prussdrv_open(PRU_EVTOUT_1);
    if (ret)
	errx(EXIT_FAILURE, "prussdrv_open: %d", ret);

    // Get the interrupt initialized
    tpruss_intc_initdata pruss_intc_initdata = PRUSS_INTC_INITDATA;
    prussdrv_pruintc_init(&pruss_intc_initdata);

    prussdrv_map_extmem(&state->ddr);
    if (prussdrv_extmem_size() < DDR_MIN_SIZE)
        errx(EXIT_FAILURE, "extmem_size not large enough. Check that uio_pruss.extram_pool_sz=0x%x or larger.", DDR_MIN_SIZE);
    state->frame_header = (struct pru_camera_frame_header *) state->ddr;
    state->ddr_phys = prussdrv_get_phys_addr(state->ddr);
    memset(state->ddr, 0, DDR_MIN_SIZE);

    // Allocate Shared PRU memory.
    prussdrv_map_prumem(PRUSS0_SHARED_DATARAM, &state->shared);
    state->config = (struct pru_camera_config *) state->shared;
}

static void camera_start(struct camera_state *state)
{
    // Fill out the PRU program's configuration
    state->config->ddr = state->ddr_phys;
    state->config->frame_size = CAMERA_FRAME_SIZE;

    DEBUG_PRINTF("Loading and running PRU code\n");
    prussdrv_exec_code(PRU_NUM, PRUcode, sizeof(PRUcode), 0);
}

static void camera_close(struct camera_state *state)
{
    memset(state, 0, sizeof(*state));

    // Disable PRU and close memory mapping
    prussdrv_pru_disable(PRU_NUM);
    prussdrv_exit ();
}

int main()
{
    DEBUG_PRINTF("Initializing troodon-cam...\n");

    struct camera_state state;
    camera_init(&state);

    DEBUG_PRINTF("Starting troodon-cam...\n");
    camera_start(&state);

    int i;
    for (i = 0; i < 10; i++) {
	int event_count;
	prussdrv_pru_wait_event(PRU_EVTOUT_1, &event_count);
	prussdrv_pru_clear_event (PRU1_ARM_INTERRUPT);

	struct pru_camera_frame_header header = *state.frame_header;

	DEBUG_PRINTF("Got %d events. Header=%d, %08x, %08x (%d)\n",
		     event_count,
		     header.id,
		     header.frame_start,
		     header.frame_end,
	             header.frame_end - header.frame_start);
    }

    camera_close(&state);

    return 0;
}

#if 0
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
#endif
