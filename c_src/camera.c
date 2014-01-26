#include <stdio.h>
#include <stdlib.h>
#include <poll.h>
#include <errno.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <err.h>
#include <jpeglib.h>
#include <time.h>
#include <arpa/inet.h>

// Driver header file
#include "prussdrv.h"
#include <pruss_intc_mapping.h>

#include "pru_camera_bin.h"
#include "erlcmd.h"

//#define DEBUG
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
    uint32_t subsample;
};

struct pru_camera_frame_header
{
    uint32_t id;
    uint32_t frame_start;
    uint32_t frame_end;
};

#define CAMERA_SUBSAMPLE   1

#if CAMERA_SUBSAMPLE == 0
#define CAMERA_MAX_LINES   480
#define CAMERA_MAX_COLUMNS 752
#else
#define CAMERA_MAX_LINES   240
#define CAMERA_MAX_COLUMNS 376
#endif

#define CAMERA_FRAME_SIZE  (CAMERA_MAX_COLUMNS * CAMERA_MAX_LINES)

// We'd like space for the frame header and two buffers
#define DDR_MIN_SIZE (sizeof(struct pru_camera_frame_header) + 2 * CAMERA_FRAME_SIZE)
#define JPEG_BUFFER_SIZE  (CAMERA_FRAME_SIZE / 2)

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

    // JPEG encoder
    struct jpeg_compress_struct cinfo;
    struct jpeg_error_mgr jerr;
    unsigned char *jpeg_out;
    unsigned long jpeg_outsize;

    // Exit
    int done;
};

static void camera_init(struct camera_state *state)
{
    memset(state, 0, sizeof(*state));

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

    // Initialize libjpeg
    state->cinfo.err = jpeg_std_error(&state->jerr);
    jpeg_create_compress(&state->cinfo);

    state->cinfo.image_width = CAMERA_MAX_COLUMNS;
    state->cinfo.image_height = CAMERA_MAX_LINES;
    state->cinfo.input_components = 1;
    state->cinfo.in_color_space = JCS_GRAYSCALE;
    jpeg_set_defaults(&state->cinfo);
    jpeg_set_quality(&state->cinfo, 50, TRUE);

    state->jpeg_outsize = JPEG_BUFFER_SIZE;
    state->jpeg_out = malloc(JPEG_BUFFER_SIZE);
    jpeg_mem_dest(&state->cinfo, &state->jpeg_out, &state->jpeg_outsize);
}

static void camera_close(struct camera_state *state)
{
    // Free up JPEG encoder memory
    free(state->jpeg_out);

    // Disable PRU and close memory mapping
    prussdrv_pru_disable(PRU_NUM);
    prussdrv_exit ();
}

static void camera_start(struct camera_state *state)
{
    // Fill out the PRU program's configuration
    state->config->ddr = state->ddr_phys;
    state->config->frame_size = CAMERA_FRAME_SIZE;
    state->config->subsample = CAMERA_SUBSAMPLE;

    DEBUG_PRINTF("Loading and running PRU code\n");
    prussdrv_exec_code(PRU_NUM, PRUcode, sizeof(PRUcode), 0);
}

static void camera_process(struct camera_state *state)
{
    int event_count;

    // Clear the event
    prussdrv_pru_wait_event(PRU_EVTOUT_1, &event_count);
    prussdrv_pru_clear_event(PRU1_ARM_INTERRUPT, PRU_EVTOUT_1);

    // Copy the frame header
    struct pru_camera_frame_header header = *state->frame_header;

#if 0
    struct timespec tp;
    clock_gettime(CLOCK_MONOTONIC, &tp);
    DEBUG_PRINTF("%d.%09d: Header=%d, %08x, %08x (%d)\n",
		 (int) tp.tv_sec,
		 (int) tp.tv_nsec,
		 header.id,
		 header.frame_start,
		 header.frame_end,
		 header.frame_end - header.frame_start);
#endif

    if (header.frame_end - header.frame_start != CAMERA_FRAME_SIZE) {
	DEBUG_PRINTF("Received bad frame: %d bytes\n", header.frame_end - header.frame_start);
	return;
    }

    // Initialize/reinitialize the output buffer
    state->jpeg_outsize = JPEG_BUFFER_SIZE;
    jpeg_mem_dest(&state->cinfo, &state->jpeg_out, &state->jpeg_outsize);

    jpeg_start_compress(&state->cinfo, TRUE);

    // Figure out the address of the start of the frame.
    JSAMPROW row_pointer[1];
    row_pointer[0] = (unsigned char *) state->ddr + (header.frame_start - state->ddr_phys);
    while (state->cinfo.next_scanline < CAMERA_MAX_LINES) {
	jpeg_write_scanlines(&state->cinfo, row_pointer, 1);
	row_pointer[0] += CAMERA_MAX_COLUMNS;
    }
    jpeg_finish_compress(&state->cinfo);

    uint16_t be_len = htons(state->jpeg_outsize);
    fwrite(&be_len, 1, 2, stdout);
    fwrite(state->jpeg_out, 1, state->jpeg_outsize, stdout);
#if 0
    struct timespec tp2;
    clock_gettime(CLOCK_MONOTONIC, &tp2);
    DEBUG_PRINTF("%d.%09d: JPEG size is %d: 0.%09d s \n",
		 (int) tp2.tv_sec,
		 (int) tp2.tv_nsec,
		 (int) state->jpeg_outsize,
		 (int) (tp2.tv_nsec - tp.tv_nsec));
#endif
}

static void camera_handle_request(ETERM *emsg, void *cookie)
{
    struct camera_state *state = (struct camera_state *) cookie;

    ETERM *emsg_type = erl_element(1, emsg);
    if (emsg_type == NULL)
	errx(EXIT_FAILURE, "erl_element(emsg_type)");

    if (strcmp(ERL_ATOM_PTR(emsg_type), "exit") == 0) {
	state->done = 1;
    } else {
	errx(EXIT_FAILURE, "unexpected request %s", ERL_ATOM_PTR(emsg_type));
    }

    erl_free_term(emsg_type);
}

int main()
{
    struct camera_state camera;
    camera_init(&camera);

    struct erlcmd handler;
    erlcmd_init(&handler, camera_handle_request, &camera);

    camera_start(&camera);
    int pru_fd = prussdrv_pru_event_fd(PRU_EVTOUT_1);

    while (!camera.done) {
	struct pollfd fdset[2];

	fdset[0].fd = pru_fd;
	fdset[0].events = POLLIN;
	fdset[0].revents = 0;

	fdset[1].fd = STDIN_FILENO;
	fdset[1].events = POLLIN;
	fdset[1].revents = 0;

	int rc = poll(fdset, 2, -1);
	if (rc < 0) {
	    /* Retry if EINTR */
	    if (errno == EINTR)
		continue;

	    err(EXIT_FAILURE, "poll");
	}

	if (fdset[0].revents & (POLLIN | POLLHUP))
	    camera_process(&camera);

	if (fdset[1].revents & (POLLIN | POLLHUP))
	    erlcmd_process(&handler);
    }

    camera_close(&camera);

    return 0;
}
