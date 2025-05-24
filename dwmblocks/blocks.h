//Modify this file to change what commands output to your statusbar, and recompile using the make command.

#define SCRIPTS_PATH "$HOME/.config/dwmblocks/scripts/"

static const Block blocks[] = {
	/*Icon*/	/*Command*/		/*Update Interval*/	/*Update Signal*/
    { " ", SCRIPTS_PATH "kernel.sh",  0,   0 },
	{ "",  SCRIPTS_PATH "memory.sh",  30,  0 },
	{ "",  SCRIPTS_PATH "battery.sh", 120, 0 },
    { "",  SCRIPTS_PATH "date.sh",    0,   0 }, 
    { "",  SCRIPTS_PATH "clock.sh",   1,   0 },
    { "",  "echo ''", 0, 0}, /* To show a separator between the last script and the systray */
};

//sets delimiter between status commands. NULL character ('\0') means no delimiter.
static char delim[] = " | ";
static unsigned int delimLen = 5;
