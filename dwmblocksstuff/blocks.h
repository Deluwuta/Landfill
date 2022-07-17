//Modify this file to change what commands output to your statusbar, and recompile using the make command.
static const Block blocks[] = {
	/*Icon*/	/*Command*/		/*Update Interval*/	/*Update Signal*/
    {" 📢🔊🚨✨", "", 0, 9},
    {" 🐧 ", "~/.config/suckless/dwmblocks/scripts/kernel.sh", 0, 1},
    {"🔔 ", "~/.config/suckless/dwmblocks/scripts/pacups.sh", 300, 2},
    {"💾 Disk:", "~/.config/suckless/dwmblocks/scripts/diskSpace.sh", 900, 3},
	{"🚀 ", "~/.config/suckless/dwmblocks/scripts/memory.sh", 5, 4},
	{"⏳", "date '+%d/%m/%Y %k:%M%p '",	30,	0},
};

//sets delimeter between status commands. NULL character ('\0') means no delimeter.
static char delim[] = " | ";
static unsigned int delimLen = 5;
