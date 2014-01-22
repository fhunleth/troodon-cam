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

    struct timespec tp;
    clock_gettime(CLOCK_MONOTONIC, &tp);
    DEBUG_PRINTF("%d.%09d: Header=%d, %08x, %08x (%d)\n",
		 (int) tp.tv_sec,
		 (int) tp.tv_nsec,
		 header.id,
		 header.frame_start,
		 header.frame_end,
		 header.frame_end - header.frame_start);

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
    struct timespec tp2;
    clock_gettime(CLOCK_MONOTONIC, &tp2);
    DEBUG_PRINTF("%d.%09d: JPEG size is %d: 0.%09d s \n",
		 (int) tp2.tv_sec,
		 (int) tp2.tv_nsec,
		 (int) state->jpeg_outsize,
		 (int) (tp2.tv_nsec - tp.tv_nsec));
}

/*
 * Erlang request/response processing
 */
#define BUF_SIZE 1024
struct erlcmd
{
    unsigned char buffer[BUF_SIZE];
    ssize_t index;
};

//void erlcmd_send(ETERM *response);

/**
 * Initialize an Erlang command handler.
 *
 * @param handler the structure to initialize
 */
void erlcmd_init(struct erlcmd *handler)
{
    memset(handler, 0, sizeof(*handler));
}

#if 0
/**
 * @brief Synchronously send a response back to Erlang
 *
 * @param response what to send back
 */
void erlcmd_send(ETERM *response)
{
    unsigned char buf[1024];

    if (erl_encode(response, buf + sizeof(uint16_t)) == 0)
	errx(EXIT_FAILURE, "erl_encode");

    ssize_t len = erl_term_len(response);
    uint16_t be_len = htons(len);
    memcpy(buf, &be_len, sizeof(be_len));

    len += sizeof(uint16_t);
    ssize_t wrote = 0;
    do {
	ssize_t amount_written = write(STDOUT_FILENO, buf + wrote, len - wrote);
	if (amount_written < 0) {
	    if (errno == EINTR)
		continue;

	    err(EXIT_FAILURE, "write");
	}

	wrote += amount_written;
    } while (wrote < len);
}
#endif

/**
 * @brief call to process any new requests from Erlang
 */
void erlcmd_process(struct erlcmd *handler, struct camera_state *camera)
{
    ssize_t amount_read = read(STDIN_FILENO, handler->buffer, sizeof(handler->buffer) - handler->index);
    if (amount_read < 0) {
	/* EINTR is ok to get, since we were interrupted by a signal. */
	if (errno == EINTR)
	    return;

	/* Everything else is unexpected. */
	err(EXIT_FAILURE, "read");
    } else if (amount_read == 0) {
	/* EOF. Erlang process was terminated. This happens after a
	   release or if there was an error. */
	exit(EXIT_SUCCESS);
    }

    handler->index += amount_read;
    for (;;) {
	ssize_t bytes_processed = 0; // TODO erlcmd_dispatch(handler, can);
	if (bytes_processed == 0) {
	    /* Only have part of the command to process. */
	    break;
	} else if (handler->index > bytes_processed) {
	    /* Processed the command and there's more data. */
	    memmove(handler->buffer, &handler->buffer[bytes_processed], handler->index - bytes_processed);
	    handler->index -= bytes_processed;
	} else {
	    /* Processed the whole buffer. */
	    handler->index = 0;
	    break;
	}
    }
}

int main()
{
    struct camera_state camera;
    struct erlcmd handler;

    erlcmd_init(&handler);
    camera_init(&camera);

    camera_start(&camera);
    int pru_fd = prussdrv_pru_event_fd(PRU_EVTOUT_1);

    for (;;) {
	struct pollfd fdset[2];

	fdset[0].fd = pru_fd;
	fdset[0].events = POLLIN;
	fdset[0].revents = 0;

	fdset[1].fd = STDIN_FILENO;
	fdset[1].events = POLLIN;
	fdset[1].revents = 0;

	int rc = poll(fdset, 1, -1);
	if (rc < 0) {
	    /* Retry if EINTR */
	    if (errno == EINTR)
		continue;

	    err(EXIT_FAILURE, "poll");
	}

	if (fdset[0].revents & (POLLIN | POLLHUP))
	    camera_process(&camera);

	if (fdset[1].revents & (POLLIN | POLLHUP))
	    erlcmd_process(&handler, &camera);
    }

#if 0
    // See how long camera_process takes if the PRU isn't running.
    usleep(100000);
    DEBUG_PRINTF("PRU off\n");
    prussdrv_pru_disable(PRU_NUM);
    camera_process(&camera);
#endif

    camera_close(&camera);

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
