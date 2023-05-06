#include "config.h"

#include "block.h"
#include "util.h"

Block blocks[] = {
    /* { "~/.config/suckless/dwmblocks-async/scripts/kernel.sh", 0, 1 }, */
    { "~/.config/suckless/dwmblocks-async/scripts/clock.sh", 1, 2 },
    {"~/.config/suckless/dwmblocks-async/scripts/battery.sh", 5, 3 },
    /* {"sb-disk",    1800, 3 }, */
    /* {"sb-memory",  10,   4 }, */
    /* {"sb-loadavg", 5,    5 }, */
    /* {"sb-mic",     0,    6 }, */
    /* {"sb-record",  0,    7 }, */
    /* {"sb-volume",  0,    8 }, */
    /* {"sb-battery", 5,    9 }, */
    /* {"sb-date",    1,    10}, */
};

const unsigned short blockCount = LEN(blocks);
