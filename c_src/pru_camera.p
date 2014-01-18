
.origin 0
.entrypoint start

#include "pru_camera.hp"

.struct Configuration
    .u32 ddr            // Where to store frames in memory. This
                        // should be big enough to hold 2 frames
    .u32 frame_size     // The max size of a frame
.ends

// This program writes the following header to the beginning
// of the DDR memory as specified in the Configuration. After
// a frame is captured, this header is updated. The host should
// capture the contents of this header before the next frame
// is done being captured or else it will miss a frame.
.struct OutputHeader
    .u32 id           // Frame ID (incremented each frame)
    .u32 frame_start  // Pointer to the start of the frame in DRAM
    .u32 frame_end    // Pointer to the end of the frame in DRAM
.ends

start:

    // Register assignments:
    // r0, r1  Temporaries
    // r2 - r3   Configuration
    // r4 - r6   Output header for the frame in progress
.assign Configuration, r2, *, config
.assign OutputHeader, r4, *, header

    // Enable OCP master port
    lbco    r0, CONST_PRUCFG, 4, 4
    clr     r0, r0, 4         // Clear SYSCFG[STANDBY_INIT] to enable OCP master port
    sbco    r0, CONST_PRUCFG, 4, 4

    // Setup CONST_PRUSHAREDDRAM to point to the base of the PRU shared memory region
    mov     r0, 0x00000100
    mov     r1, PRU1_CTRL_CTPPR_0
    st32    r0, r1

    // Copy the configuration data from PRU shared memory
     lbco    &config, CONST_PRUSHAREDRAM, 0, SIZE(config)

    // Enable 16 bit parallel capture mode
    mov     r0, 0x01
    sbco    r0, CONST_PRUCFG, 0x0c, 4

    // Setup first frame header
    zero    &header, SIZE(header)

main_loop:
    // Restart the pixel and line counters
    add     header.frame_start, config.ddr, SIZE(header)
    qbbc    capture_frame, header.id, 0

    // Odd numbered frames use the 2nd buffer
    add     header.frame_start, header.frame_start, config.frame_size

capture_frame:
    mov     header.frame_end, header.frame_start

    // Wait for vsync
vsync_loop:
    // Wait for pixel clock
    wbs     r31, 16
    qbbs    vsync_loop, r31, 11 // branch if fv is set

    // Wait for frame
wait_for_start_loop:
    wbs     r31, 16
    qbbc    wait_for_start_loop, r31, 11

wait_for_start_line:
    wbs     r31, 16
    qbbc    frame_done, r31, 11 // if frame_valid goes to 0, then done
    qbbc    wait_for_start_line, r31, 10

    // Trim the pixel to 8 bits for now (really want all 10 bits)
    lsr     r0, r31, 2
    and     r0, r0, 255

    // Store the first pixel of the line to DRAM
    sbbo    r0.b0, header.frame_end, 0, 1
    add     header.frame_end, header.frame_end, 1

read_line:
    wbs     r31, 16
    qbbc    frame_done, r31, 11 // if frame_valid goes to 0, then done
    qbbc    wait_for_start_line, r31, 10 // if line_valid goes to 0, then wait for the next line

    // Trim the pixel to 8 bits for now (really want all 10 bits)
    lsr     r0, r31, 2
    and     r0, r0, 255

    // Store the pixel to DRAM
    // NOTE: unroll if storing 32 bits at a time is needed, but be careful to
    //       get alignment right
    sbbo    r0.b0, header.frame_end, 0, 1
    add     header.frame_end, header.frame_end, 1

    qba     read_line

frame_done:
    // Write the completed header to DRAM
    sbbo    &header, config.ddr, 0, SIZE(header)

    // Notify the host
    mov     r31.b0, PRU1_ARM_INTERRUPT+16

    // Now work on the next frame
    add     header.id, header.id, 1
    jmp     main_loop
