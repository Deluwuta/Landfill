//Modify this file to change what commands output to your statusbar, and recompile using the make command.

#define SCRIPTS_PATH "$HOME/.config/dwmblocks/scripts/"

static const Block blocks[] = {
	/*Icon*/	/*Command*/		/*Update Interval*/	/*Update Signal*/
	{"Mem:", "free -h | awk '/^Mem/ { print $3\"/\"$2 }' | sed s/i//g",	30,		0},

	{"", SCRIPTS_PATH "date.sh", 1, 0},
};

//sets delimiter between status commands. NULL character ('\0') means no delimiter.
static char delim[] = " | ";
static unsigned int delimLen = 5;
