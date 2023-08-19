------------------------------
-- Defaultn't awesome theme --
------------------------------

local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()
local theme_assets = require("beautiful.theme_assets")

local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local theme = {}

-- * Ui Fonts * --
theme.font_name = "FantasqueSansM Nerd Font "
-- theme.font_name = "Hack Nerd Font "
-- theme.font_name = "JetBrainsNF Nerd Font "
theme.font = theme.font_name .. "Medium 10"
-- theme.taglist_font  = "FantasqueSansM Nerd Font Propo 12"

-- Icon Fonts (In progress)


-- * Colors * --

-- Ayu Dark Color Scheme

-- Catppuccin-macchiato Color Scheme

-- * Yoru Color Scheme * --
--- Special
theme.darker_black  = "#060809"
theme.black         = "#0c0e0f"
theme.lighter_black = "#121415"
theme.one_bg        = "#161819"
theme.one_bg2       = "#1f2122"
theme.one_bg3       = "#27292a"
theme.grey          = "#343637"
theme.grey_fg       = "#3e4041"
theme.grey_fg2      = "#484a4b"
theme.light_grey    = "#505253"
theme.white         = "#edeff0"

theme.transparent = "#00000000"

--- Black
theme.color0 = "#232526"
theme.color8 = "#2c2e2f"

--- Red
theme.color1 = "#df5b61"
theme.color9 = "#e8646a"

--- Green
theme.color2 = "#78b892"
theme.color10 = "#81c19b"

--- Yellow
theme.color3 = "#de8f78"
theme.color11 = "#e79881"

--- Blue
theme.color4 = "#6791c9"
theme.color12 = "#709ad2"

--- Magenta
theme.color5 = "#bc83e3"
theme.color13 = "#c58cec"

--- Cyan
theme.color6 = "#67afc1"
theme.color14 = "#70b8ca"

--- White
theme.color7 = "#e4e6e7"
theme.color15 = "#f2f4f5"

--- * Extras * ---
-- E-Blue
theme.extra_blue1 = "#45D0FE"

-- E-Pink
theme.extra_pink1 = "#f5c2e7"

------------------------------------

-- * General * --
--- Background colors
theme.bg_normal     = theme.one_bg
theme.bg_focus      = theme.color7
theme.bg_urgent     = theme.one_bg
theme.bg_minimize   = theme.black

--- Foreground colors
theme.fg_normal     = theme.white
theme.fg_focus      = theme.color4
theme.fg_urgent     = theme.color1
theme.fg_minimize   = theme.color0

-- ** Borders ** -- 
theme.gap_single_client = false

theme.border_width  = 3
theme.useless_gap   = dpi(4)

theme.border_normal = theme.lighter_black
theme.border_focus  = theme.extra_pink1
theme.border_marked = theme.color9

-- theme.border_radius = 12

-- ** Events ** --
theme.leave_event = theme.transparent
theme.enter_event = "#ffffff" .. "10"
theme.pres_event = "#ffffff" .. "15"
theme.release_event = "#ffffff" .. "10"

-- ** Taglist ** --
theme.taglist_bg_focus = "#4d4d4d"
theme.taglist_fg_focus = "#89dceb"
theme.taglist_fg_occupied = "#cba6f7"

theme.menu_submenu_icon = themes_path.."default/submenu.png"
theme.menu_height = dpi(15)
theme.menu_width  = dpi(100)

-- ** Titlebars ** -- 
theme.titlebar_enabled = false
theme.titlebar_bg = theme.black
theme.titlebar_fg = theme.white

--- Close Button
theme.titlebar_close_button_normal = themes_path.."default/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = themes_path.."default/titlebar/close_focus.png"

--- Minimize Buttom
theme.titlebar_minimize_button_normal = themes_path.."default/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus  = themes_path.."default/titlebar/minimize_focus.png"

--- Maximized Active Button
theme.titlebar_maximized_button_normal_active = themes_path.."default/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = themes_path.."default/titlebar/maximized_focus_active.png"

--- Maximized Inactive Button
theme.titlebar_maximized_button_normal_inactive = themes_path.."default/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = themes_path.."default/titlebar/maximized_focus_inactive.png"
--
-- ** Wibar ** --
theme.wibar_bg = theme.one_bg
theme.bg_systray = theme.wibar_bg

-- ** Layouts Icons ** --
theme.layout_floating  = themes_path.."default/layouts/floatingw.png"
theme.layout_magnifier = themes_path.."default/layouts/magnifierw.png"
theme.layout_max = themes_path.."default/layouts/maxw.png"
theme.layout_fullscreen = themes_path.."default/layouts/fullscreenw.png"
theme.layout_tilebottom = themes_path.."default/layouts/tilebottomw.png"
theme.layout_tile = themes_path.."default/layouts/tilew.png"
theme.layout_tiletop = themes_path.."default/layouts/tiletopw.png"

-- theme.wallpaper = themes_path.."default/background.png"
theme.wallapaper = "/home/delta/Pictures/archwall1.jpg"

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
    theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
