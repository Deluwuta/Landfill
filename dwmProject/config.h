/*
                         
    /\                   
   /  \   __   __  _   _ 
  / /\ \ / / _ \ \| | | |
 / /__\ \ |_/ \_| | |_| |
/________\___^___/| ._,_|
                  | |    
                  |_|    

Delta's Δωμ config file. 
*/

// Autostarting
static const char *const autostart[] = {
	// "alacritty", NULL,
	"sh", "-c", "xwallpaper --zoom $HOME/Pictures/oshinoko.png", NULL,
	"sh", "-c", "xrandr -s 1920x1080", NULL,
	"sh", "-c", "killall -q dwmblocks; while pgrep -u $UID -x dwmblocks >/dev/null; do sleep 1; done; dwmblocks &", NULL,
	NULL /* terminate */
};

/* appearance */
static const unsigned int borderpx = 3;  /* border pixel of windows */
static const unsigned int gappx    = 12; /* Gaps between windows */
static const unsigned int snap     = 32; /* snap pixel */
static const int showbar           = 1;  /* 0 means no bar */
static const int topbar            = 1;  /* 0 means bottom bar */
static const char *fonts[]         = { "FiraCode Nerd Font:size=10" };
static const char dmenufont[]      = "FiraCode Nerd Font:size=10";

/* static const char col_gray1[] = "#222222"; */
static const char col_gray2[] = "#444444";
static const char col_gray3[] = "#bbbbbb";
static const char col_gray4[] = "#eeeeee";
static const char col_cyan[]  = "#005577";

// Oxocarbon color scheme
static const char col_black1[] = "#161616";
static const char col_black2[] = "#262626";
static const char col_black3[] = "#393939";

static const char col_white1[] = "#dde1e6";
static const char col_white2[] = "#f2f4f8";
static const char col_white3[] = "#ffffff";

static const char col_gray1[] = "#525252";

static const char col_pink1[] = "#ee5396";
static const char col_pink2[] = "#ff7eb6";


static const char *colors[][3]      = {
	/*               fg         bg         border   */
	[SchemeNorm] = { col_white1, col_black2, col_black1 },
	[SchemeSel]  = { col_white3, col_black3,  col_pink2  },
};

/* tagging */
static const char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

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
static const float mfact        = 0.50; /* factor of master area size [0.05..0.95] */
static const int nmaster        = 1;    /* number of clients in master area */
static const int resizehints    = 1;    /* 1 means respect size hints in tiled resizals */
static const int lockfullscreen = 1;    /* 1 will force focus on the fullscreen window */

static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },    /* first entry is default */
	{ "><>",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
};

/* key definitions */
#define MODKEY Mod4Mask
#define ALTKEY Mod1Mask

#define TAGKEYS(KEY,TAG) \
	{ MODKEY,                       KEY, view,       {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask,           KEY, toggleview, {.ui = 1 << TAG} }, \
	{ MODKEY|ShiftMask,             KEY, tag,        {.ui = 1 << TAG} }, \
	{ MODKEY|ControlMask|ShiftMask, KEY, toggletag,  {.ui = 1 << TAG} },

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
static char dmenumon[2] = "0"; /* component of dmenucmd, manipulated in spawn() */
static const char *dmenucmd[] = { 
	"dmenu_run", 
	"-m", dmenumon, 
	"-fn", dmenufont, 
	"-nb", col_gray1, 
	"-nf", col_gray3, 
	"-sb", col_cyan, 
	"-sf", col_gray4, 
	NULL };

static const char *termcmd[]  = { "alacritty", NULL };

static const Key keys[] = {
	/* modifier                     key        function        argument */
	// Yeee
	{ MODKEY, XK_p	   , spawn, {.v = dmenucmd } },
	{ MODKEY, XK_Return, spawn, {.v = termcmd } },

	// Quiting and restarting
	{ MODKEY|ControlMask, XK_q, quit, {0} },
	{ MODKEY|ControlMask, XK_r, quit, {1} },

	// Killin
	{ MODKEY|ShiftMask, XK_c, killclient, {0} },

	// Basic stuff
	{ MODKEY, XK_b, spawn, SHCMD("brave")},

	// Force to tile | Hide bar
	{ MODKEY|ShiftMask  , XK_t, togglefloating, {0} },
	{ MODKEY|ControlMask, XK_h, togglebar, {0} },

	// Windows swapin
	{ MODKEY|ShiftMask, XK_space, zoom, {0} },

	// Window movement and size manip
	{ MODKEY, XK_j, focusstack, {.i = +1 } },
	{ MODKEY, XK_k, focusstack, {.i = -1 } },
	{ MODKEY, XK_h, setmfact, {.f = -0.05} },
	{ MODKEY, XK_l, setmfact, {.f = +0.05} },

	{ MODKEY, XK_Tab  , view, {0} },
	{ MODKEY, XK_t    , setlayout, {.v = &layouts[0]} },
	{ MODKEY, XK_f    , setlayout, {.v = &layouts[1]} },
	{ MODKEY, XK_m    , setlayout, {.v = &layouts[2]} },
	{ MODKEY, XK_space, setlayout, {0} },

	{ MODKEY, XK_0, view, {.ui = ~0 } },
	{ MODKEY|ShiftMask, XK_0, tag, {.ui = ~0 } },


	//{ MODKEY, XK_i, incnmaster, {.i = +1 } },
	//{ MODKEY, XK_d, incnmaster, {.i = -1 } },

	// Multimonitor keys
	//{ MODKEY,                       XK_comma,  focusmon,       {.i = -1 } },
	//{ MODKEY,                       XK_period, focusmon,       {.i = +1 } },
	//{ MODKEY|ShiftMask,             XK_comma,  tagmon,         {.i = -1 } },
	//{ MODKEY|ShiftMask,             XK_period, tagmon,         {.i = +1 } },

	TAGKEYS(XK_1, 0)
	TAGKEYS(XK_2, 1)
	TAGKEYS(XK_3, 2)
	TAGKEYS(XK_4, 3)
	TAGKEYS(XK_5, 4)
	TAGKEYS(XK_6, 5)
	TAGKEYS(XK_7, 6)
	TAGKEYS(XK_8, 7)
	TAGKEYS(XK_9, 8)
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle, ClkClientWin, or ClkRootWin */
static const Button buttons[] = {
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
