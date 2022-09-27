/* See LICENSE file for copyright and license details. */

// PACMAN GHOST ICON \uf6e2

/* appearance */

static const unsigned int colorfultitle = 1;
static const unsigned int colorfultag = 1;

static const unsigned int ulinepad	    = 1;	/* horizontal padding between the underline and tag */
static const unsigned int ulinestroke	= 2;	/* thickness / height of the underline */
static const unsigned int ulinevoffset	= 0;	/* how far above the bottom of the bar the line should appear */
static const int ulineall 		        = 0;	/* 1 to show underline on all tags, 0 for just the active ones */

static const unsigned int borderpx  = 2;        /* border pixel of windows */
static const unsigned int gappx     = 8;        /* gaps between windows */
static const unsigned int snap      = 32;       /* snap pixel */
static const int showbar            = 1;        /* 0 means no bar */
static const int topbar             = 1;        /* 0 means bottom bar */
static const char *fonts[]          = { "Hack:size=14:antialias=true:autohint=true",
                                        "Noto Color Emoji:size=12:antialias=true:autohint=true",
										"Material Design Icons Desktop:size=15",
                                        "Font Awesome 6 Free Solid:size=14:antialias=true:autohint=true",
										"Font Awesome 6 Regular:size=14:antialias=true:autohint=true",
										"Font Awesome 6 Brands:size=14:antialias=true:autohint=true"
                                      };
static const char dmenufont[]       = "Hack:size=11";
static const char col_gray1[]       = "#222222";
static const char col_gray2[]       = "#444444";
static const char col_gray3[]       = "#bbbbbb";
static const char col_gray4[]       = "#eeeeee";
//static const char col_cyan[]        = "#00425E"; //"#005577"
//static const char *colors[][3]      = {
//	/*               fg         bg         border   */
//	[SchemeNorm] = { col_gray3, col_gray1, col_gray2 },
//	[SchemeSel]  = { col_gray4, col_cyan,  col_cyan  },
//	[SchemeTag1] = { "#580100", "#000000", col_cyan },
//};
//
static const char col_bg[]               = "#1a1b26";
static const char col_dark[]             = "#16161E";
static const char col_dark_1[]           = "#232433";
static const char col_dark_2[]           = "#2a2b3d";
static const char col_dark_3[]           = "#3b3d57";
static const char col_fg[]               = "#a9b1d6";
static const char col_black[]            = "#32344a";
static const char col_br_black[]         = "#444b6a";
static const char col_white[]            = "#787c99";
static const char col_br_white[]         = "#acb0d0";
static const char col_red[]              = "#f7768e";
static const char col_br_red[]           = "#ff7a93";
static const char col_green[]            = "#9ece6a";
static const char col_br_green[]         = "#b9f27c";
static const char col_yellow[]           = "#e0af68";
static const char col_br_yellow[]        = "#ff9e64";
static const char col_blue[]             = "#7aa2f7";
static const char col_br_blue[]          = "#7da6ff";
static const char col_magenta[]          = "#ad8ee6";
static const char col_br_magenta[]       = "#bb9af7";
static const char col_cyan[]             = "#449dab";
static const char col_br_cyan[]          = "#0db9d7";

static const char col_darkGrey[]   = "#201e23";
static const char col_pastelYell[] = "#d0be48";
static const char col_spark[]      = "#fefc0c";
static const char col_redBlood[]   = "#580100";
static const char col_redBrighter[]= "#A60300";

static const char col_lightGreen[] =   "#00e62a";
static const char col_lightYell[] =    "#ebe801";
static const char col_orange[] =       "#ea9402";
static const char col_lightRed[] =     "#ea0f01";
static const char col_lightPinkish[] = "#d601ea";
static const char col_lightPurple[] =  "#7400ea";
static const char col_darkBlue[] =     "#010bea";
static const char col_lightBlue[] =    "#00d3eb";

static const char *colors[][3]           = {
	/*                     fg              bg              border   */
	[SchemeNorm]       = { col_fg,             col_darkGrey,       col_darkGrey },         /* \x0b */
	[SchemeSel]        = { col_blue,           col_darkGrey,       col_spark },     /* \x0c */
	[SchemeTag]        = { col_white,          col_darkGrey,       "#FFFFFF" },
	[SchemeTag1]       = { col_pastelYell,     col_darkGrey,       col_black },
	[SchemeTag2]       = { col_lightGreen,     col_darkGrey,       col_black },
	[SchemeTag3]       = { col_lightPurple,    col_darkGrey,       col_black },
	[SchemeTag4]       = { col_lightBlue,      col_darkGrey,       col_black },
	[SchemeTag5]       = { col_pastelYell,     col_darkGrey,       col_black },
	[SchemeTag6]       = { col_pastelYell,     col_darkGrey,       col_black },
	[SchemeTag7]       = { col_pastelYell,     col_darkGrey,       col_black },
	[SchemeTag8]       = { col_pastelYell,     col_darkGrey,       col_black },
	[SchemeTag9]       = { col_pastelYell,     col_darkGrey,       col_black },
	[SchemeLayout]     = { col_lightPinkish,   col_darkGrey,       col_black },
	[SchemeTitle]      = { col_fg,             col_darkGrey,       col_black },
	[SchemeTitleFloat] = { col_br_blue,        col_darkGrey,       col_black },
	[SchemeTitle1]     = { col_redBrighter,     col_darkGrey,       col_black },
	[SchemeTitle2]     = { col_redBrighter,     col_darkGrey,       col_black },
	[SchemeTitle3]     = { col_redBrighter,     col_darkGrey,       col_black },
	[SchemeTitle4]     = { col_redBrighter,     col_darkGrey,       col_black },
	[SchemeTitle5]     = { col_redBrighter,     col_darkGrey,       col_black },
	[SchemeTitle6]     = { col_redBrighter,     col_darkGrey,       col_black },
	[SchemeTitle7]     = { col_redBrighter,     col_darkGrey,       col_black },
	[SchemeTitle8]     = { col_redBrighter,     col_darkGrey,       col_black },
	[SchemeTitle9]     = { col_redBrighter,     col_darkGrey,       col_black },
};

/* tagging */
static const char *tags[] = { "󰮯", "⦁", "\uf111", "⦁", "\uf111", "⦁", "\uf111", "⦁", "\uf111" };
static const char *tagsalt[] = { "α", "β", "γ", "Δ", "Θ", "λ", "χ", "ψ", "Ω" };
static const int momentaryalttags = 0; // 1 means alttags will show only when key is held down

static const int tagschemes[] = { SchemeTag1, SchemeTag2, SchemeTag3,
                                  SchemeTag4, SchemeTag5, SchemeTag6,
                                  SchemeTag7, SchemeTag8, SchemeTag9 };
static const int titleschemes[] = { SchemeTitle1, SchemeTitle2, SchemeTitle3,
                                    SchemeTitle4, SchemeTitle5, SchemeTitle6,
                                    SchemeTitle7, SchemeTitle8, SchemeTitle9 };

static const Rule rules[] = {
	/* xprop(1):
	 *	WM_CLASS(STRING) = instance, class
	 *	WM_NAME(STRING) = title
	 */
	/* class      instance    title       tags mask     isfloating   monitor */
	{ "Gimp",     NULL,       NULL,       0,            1,           -1 },
	{ "Firefox",  NULL,       NULL,       1 << 8,       0,           -1 },
};

/* layout(s) */
static const float mfact     = 0.50; /* factor of master area size [0.05..0.95] */
static const int nmaster     = 1;    /* number of clients in master area */
static const int resizehints = 0;    /* 1 means respect size hints in tiled resizals */
static const int lockfullscreen = 1; /* 1 will force focus on the fullscreen window */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
    { "|M|",      centeredmaster },
	{ "\uf6e2",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
    //{ ">M>",      centeredfloatingmaster },
    { NULL, NULL }, // cyclelayouts dependency
};

/* key definitions */
#define MODKEY Mod4Mask

#define TAGKEYS(KEY,TAG)												\
	{MODKEY, KEY,								view,           {.ui = 1 << TAG} },	\
	{MODKEY|ControlMask, KEY,					toggleview,     {.ui = 1 << TAG} }, \
	{MODKEY|ShiftMask, KEY,						tag,            {.ui = 1 << TAG} }, \
	{MODKEY|ControlMask|ShiftMask, KEY,			toggletag,      {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { "dmenu_run", "-m", dmenumon, "-fn", dmenufont, "-nb", col_dark, "-nf", col_gray3, "-sb", col_redBlood, "-sf", col_gray4,
                                  "-p", "Dwm: ", NULL };
static const char *termcmd[]  = { "st", NULL };

static Key keys[] = {
	/* Keys        function        argument */
	{MODKEY, XK_p,							spawn,          {.v = dmenucmd } },
//	{MODKEY, XK_e, 			spawn,          {.v = termcmd } },

    /* My own keybindings */
    {MODKEY, XK_Return,      spawn,       SHCMD("alacritty -e fish")},
    {MODKEY|ShiftMask, XK_e, spawn, SHCMD("emacsclient -c -a 'emacs'")},
    {MODKEY, XK_b, spawn, SHCMD("librewolf")},
    {MODKEY, XK_f, spawn, SHCMD("thunar")},

	// Your blade sir... It will KEEL
	{MODKEY|ShiftMask, XK_c,					killclient,     {0} },

	/*** (Layout modification commands) Keychords SUPER + z followed by key ***/
	{MODKEY|ControlMask, XK_b, togglebar, {0} }, // Hide/Show bar
	{MODKEY|ControlMask, XK_t, setlayout, {.v = &layouts[0]}}, // Tiled
	{MODKEY|ControlMask, XK_f, setlayout, {.v = &layouts[1]}}, // Floating
	{MODKEY|ControlMask, XK_m, setlayout, {.v = &layouts[2]}}, // Monocle
    {MODKEY|ControlMask, XK_u, setlayout, {.v = &layouts[3]}}, // CenteredMaster
    {MODKEY|ControlMask, XK_o, setlayout, {.v = &layouts[4]}}, // CenteredFloating
	{MODKEY|ControlMask, XK_n, togglealttag, {0} },

	/*** (Reload commands) Keychords SUPER + r followed by key ***/
	{MODKEY|ControlMask, XK_r, spawn, SHCMD("$HOME/.config/dmenuscripts/resetdwmblocks.sh")},

	/*** (Dmenu scripts commands) Keychords SUPER + d followed by key ***/

	/*** Volumen, Monitor backlight and keyboard backlight ***/
	{0, XF86XK_AudioRaiseVolume, spawn, SHCMD("pamixer -ui 5 ; pkill -RTMIN+6 dwmblocks")},
	{0, XF86XK_AudioLowerVolume, spawn, SHCMD("pamixer -ud 5 ; pkill -RTMIN+6 dwmblocks")},

	{0, XF86XK_MonBrightnessUp, spawn, SHCMD("brightnessctl s 5%+ ; pkill -RTMIN+8 dwmblocks")},
	{0, XF86XK_MonBrightnessDown, spawn, SHCMD("brightnessctl s 5%- ; pkill -RTMIN+8 dwmblocks")},

	{0, XF86XK_KbdBrightnessUp, spawn, SHCMD("~/kbdbacklight.sh up")},
	{0, XF86XK_KbdBrightnessDown, spawn, SHCMD("~/kbdbacklight.sh down")},

	/*** Layout manipulation ***/
	{MODKEY, XK_j,							focusstack,     {.i = +1 } },
	{MODKEY, XK_k,							focusstack,     {.i = -1 } },
	//{MODKEY, XK_i,							incnmaster,     {.i = +1 } },
	//{MODKEY, XK_d,							incnmaster,     {.i = -1 } },
	{MODKEY, XK_h,							setmfact,       {.f = -0.05} },
	{MODKEY, XK_l,							setmfact,       {.f = +0.05} },
	{MODKEY|ShiftMask, XK_Return,			zoom,           {0} }, // Makes the focused window the master
	{MODKEY, XK_Tab,							view,           {0} }, // Rotate between the last 2 used workspaces
	{MODKEY, XK_space,						cyclelayout,    {.i = +1} }, // Rotates between layouts
	{MODKEY|ShiftMask, XK_space,				cyclelayout,    {.i = -1} },

	//{1, {{MODKEY|ShiftMask, XK_space}},				togglefloating, {0} },
	{MODKEY, XK_0,							view,           {.ui = ~0 } },
	{MODKEY|ShiftMask, XK_0,					tag,            {.ui = ~0 } },

	// Switching between monitors (Not used in my case)
	//{MODKEY, XK_comma,						focusmon,       {.i = -1 } },
	//{MODKEY, XK_period,					focusmon,       {.i = +1 } },
	//{MODKEY|ShiftMask, XK_comma,			tagmon,         {.i = -1 } },
	//{MODKEY|ShiftMask, XK_period,			tagmon,         {.i = +1 } },

	// Workspaces
	TAGKEYS(                        XK_1,                      0)
	TAGKEYS(                        XK_2,                      1)
	TAGKEYS(                        XK_3,                      2)
	TAGKEYS(                        XK_4,                      3)
	TAGKEYS(                        XK_5,                      4)
	TAGKEYS(                        XK_6,                      5)
	TAGKEYS(                        XK_7,                      6)
	TAGKEYS(                        XK_8,                      7)
	TAGKEYS(                        XK_9,                      8)

	// Quit and reload, in that order '-'
	{MODKEY|ShiftMask, XK_q,      quit,           {0} },
	{MODKEY|ShiftMask, XK_r,      quit,           {1} },
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static Button buttons[] = {
	/* click                event mask      button          function        argument */
	{ ClkLtSymbol,          0,              Button1,        setlayout,      {0} },
	{ ClkLtSymbol,          0,              Button3,        setlayout,      {.v = &layouts[2]} },
	{ ClkWinTitle,          0,              Button2,        zoom,           {0} },
	{ ClkStatusText,        0,              Button2,        spawn,          {.v = termcmd } },
	{ ClkClientWin,         MODKEY,         Button1,        movemouse,      {0} },
	{ ClkClientWin,         MODKEY,         Button2,        togglefloating, {0} },
	{ ClkClientWin,         MODKEY,         Button3,        resizemouse,    {0} },
	{ ClkTagBar,            0,              Button1,        view,           {0} },
	{ ClkTagBar,            0,              Button3,        toggleview,     {0} },
	{ ClkTagBar,            MODKEY,         Button1,        tag,            {0} },
	{ ClkTagBar,            MODKEY,         Button3,        toggletag,      {0} },
};
