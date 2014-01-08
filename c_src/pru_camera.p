
.origin 0
.entrypoint start

#include "pru_camera.hp"

start:

.struct Configuration
    .u32 ddr
.ends

.assign Configuration, r2, *, config

    // Enable OCP master port
    lbco      r0, CONST_PRUCFG, 4, 4
    clr     r0, r0, 4         // Clear SYSCFG[STANDBY_INIT] to enable OCP master port
    sbco      r0, CONST_PRUCFG, 4, 4

    // Setup CONST_PRUSHAREDDRAM to point to the base of the PRU shared memory region
    mov     r0, 0x00000100
    mov       r1, PRU1_CTRL_CTPPR_0
    st32      r0, r1

    // Copy the configuration data from PRU shared memory
    lbco    &config, CONST_PRUSHAREDRAM, 0, 4

    // Enable 16 bit parallel capture mode
    mov     r0, 0x01
    sbco    r0, CONST_PRUCFG, 0x0c, 4

.enter Scope
.struct Stats
    .u32 pixels
    .u32 lines
    .u32 frameptr
.ends
.assign Stats, r3, *, stats

    zero &stats, SIZE(stats)
    add stats.frameptr, config.ddr, SIZE(stats)

    // Wait for vsync
vsync_loop:
    // Wait for pixel clock
    wbs  r31, 16
    qbbs vsync_loop, r31, 11 // branch if fv is set

    // Wait for frame
wait_for_start_loop:
    wbs r31, 16
    qbbc wait_for_start_loop, r31, 11

wait_for_start_line:
    wbs r31, 16
    qbbc done, r31, 11 // if fv goes to 0, then done
    qbbc wait_for_start_line, r31, 10

    add stats.lines, stats.lines, 1 // start of a line
    add stats.pixels, stats.pixels, 1 // got a pixel

    // trim the pixel to 8 bits for now
    lsr r0, r31, 2
    and r0, r0, 255

    sbbo r0.b0, stats.frameptr, 0, 1
    add stats.frameptr, stats.frameptr, 1

read_line:
    wbs r31, 16
    qbbc done, r31, 11 // if fv goes to 0, then done
    qbbc wait_for_start_line, r31, 10 // if lv goes to 0, then wait for the next line

    add stats.pixels, stats.pixels, 1 // got a pixel

    // trim the pixel to 8 bits for now
    lsr r0, r31, 2
    and r0, r0, 255

    sbbo r0.b0, stats.frameptr, 0, 1
    add stats.frameptr, stats.frameptr, 1

    qba read_line

done:
    sbbo &stats, config.ddr, 0, SIZE(stats)

.leave Scope

    // Send notification to Host for program completion
    mov       r31.b0, PRU1_ARM_INTERRUPT+16

    // Halt the processor
    halt


