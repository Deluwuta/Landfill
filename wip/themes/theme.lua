-- This shit ain't the default theme lol
local beautiful = require("beautiful")
local theme_assets = require("beautiful.theme_assets")
-- local rnotification = require("ruled.notification")
local dpi = beautiful.xresources.apply_dpi

local gears = require("gears")
local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()

local user = require("utils.user_variables")
local colors = require("themes." .. user.theme)

local theme = {}

theme.extra_colors = {
    catppuccin_surface0 = "#313244",
    catppuccin_flamingo = "#f2cdcd",

    pinkish_white = "#faedff",
}

theme.font_name = "Hack "
theme.font = theme.font_name .. "Medium 10"

theme.bg = colors.bg_dark
theme.fg = colors.fg_normal

theme.bg_normal = theme.bg
theme.fg_normal = theme.fg

theme.bg_focus = colors.bg_normal
theme.fg_focus = theme.fg

theme.bg_urgent = "#543a48"
theme.fg_urgent = theme.fg

theme.bg_minimize = colors.bg_dim
theme.fg_minimize = theme.fg

theme.bg_systray = theme.bg_normal

-- Window border
theme.border_width = dpi(2)

theme.border_normal = theme.extra_colors.catppuccin_surface0 -- Surface0 from catppuccin mocha
theme.border_focus = "#94e2d5"
theme.border_marked = colors.red

-- theme.gap_single_client = false
theme.useless_gap = dpi(2)

-- Wallpaper
theme.wallpaper = themes_path.."default/background.png"

-- There are other variable sets
-- overriding the default one when
-- defined, the sets are:
-- taglist_[bg|fg]_[focus|urgent|occupied|empty|volatile]
-- tasklist_[bg|fg]_[focus|urgent]
-- titlebar_[bg|fg]_[normal|focus]
-- tooltip_[font|opacity|fg_color|bg_color|border_width|border_color]
-- prompt_[fg|bg|fg_cursor|bg_cursor|font]
-- hotkeys_[bg|fg|border_width|border_color|shape|opacity|modifiers_fg|label_bg|label_fg|group_margin|font|description_font]
-- Example:
--theme.taglist_bg_focus = "#ff0000"

-- Generate taglist squares (the thing that appears when a window is opened):
local taglist_square_size = dpi(0)
theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
    taglist_square_size, theme.fg_normal
)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
    taglist_square_size, theme.fg_normal
)

-- Taglist (This one IS useful)
-- theme.taglist_font = theme.font_name .. "bold 10"
--
-- theme.taglist_bg_emtpy = theme.bg
-- theme.taglist_fg_emtpy = theme.fg
--
-- theme.taglist_bg_occupied = theme.bg
-- theme.taglist_fg_occupied = theme.fg
--
-- theme.taglist_bg_focus = "#48584e"
-- theme.taglist_fg_focus = theme.fg
--
-- theme.taglist_spacing = 4
-- theme.taglist_shape_border_width = 1
-- theme.taglist_shape_border_radius = 20
-- theme.taglist_shape_border_color = "#00000040"

-- Tasklist config (I won't use one (For now lol))
theme.tasklist_disable_task_name = false

theme.tasklist_bg_minimize = theme.bg_minimize
theme.tasklist_bg_focus = theme.bg_focus

theme.tasklist_shape_border_width = 0

-- Variables set for theming notifications:
-- notification_font
-- notification_[bg|fg]
-- notification_[width|height|margin]
-- notification_[border_color|border_width|shape|opacity]

-- theme.notification_font = theme.font
--
-- theme.notification_bg = colors.bg_dim
-- theme.notification_fg = theme.fg
--
-- theme.notification_position = 'top_right'
-- theme.notification_margin = dpi(10)
--
-- theme.notification_border_width = 6
-- theme.notification_border_color = colors.bg_dark
theme.notification_shape = gears.shape.rounded_rect
--
-- theme.notification_max_width = dpi(350)
--
-- theme.notification_spacing = dpi(15)
-- theme.notification_icon_resize_strategy = 'center'
-- theme.notification_icon_size = dpi(300)

-- Variables set for theming the menu:
-- menu_[bg|fg]_[normal|focus]
-- menu_[border_color|border_width]
theme.menu_submenu_icon = themes_path.."default/submenu.png"
theme.menu_height = dpi(15)
theme.menu_width  = dpi(100)

-- You can add as many variables as
-- you wish and access them by using
-- beautiful.variable in your rc.lua
--theme.bg_widget = "#cc0000"

-- Define the image to load
theme.titlebar_close_button_normal = themes_path.."default/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = themes_path.."default/titlebar/close_focus.png"

theme.titlebar_minimize_button_normal = themes_path.."default/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus  = themes_path.."default/titlebar/minimize_focus.png"

theme.titlebar_ontop_button_normal_inactive = themes_path.."default/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = themes_path.."default/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = themes_path.."default/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = themes_path.."default/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = themes_path.."default/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = themes_path.."default/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = themes_path.."default/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = themes_path.."default/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = themes_path.."default/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = themes_path.."default/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = themes_path.."default/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = themes_path.."default/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = themes_path.."default/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = themes_path.."default/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = themes_path.."default/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = themes_path.."default/titlebar/maximized_focus_active.png"

-- You can use your own layout icons like this:
theme.layout_fairh = themes_path.."default/layouts/fairhw.png"
theme.layout_fairv = themes_path.."default/layouts/fairvw.png"
theme.layout_floating  = themes_path.."default/layouts/floatingw.png"
theme.layout_magnifier = themes_path.."default/layouts/magnifierw.png"
theme.layout_max = themes_path.."default/layouts/maxw.png"
theme.layout_fullscreen = themes_path.."default/layouts/fullscreenw.png"
theme.layout_tilebottom = themes_path.."default/layouts/tilebottomw.png"
theme.layout_tileleft   = themes_path.."default/layouts/tileleftw.png"
theme.layout_tile = themes_path.."default/layouts/tilew.png"
theme.layout_tiletop = themes_path.."default/layouts/tiletopw.png"
theme.layout_spiral  = themes_path.."default/layouts/spiralw.png"
theme.layout_dwindle = themes_path.."default/layouts/dwindlew.png"
theme.layout_cornernw = themes_path.."default/layouts/cornernww.png"
theme.layout_cornerne = themes_path.."default/layouts/cornernew.png"
theme.layout_cornersw = themes_path.."default/layouts/cornersww.png"
theme.layout_cornerse = themes_path.."default/layouts/cornersew.png"

-- Generate Awesome icon:
theme.awesome_icon = theme_assets.awesome_icon(
    theme.menu_height, theme.bg_focus, theme.fg_focus
)

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

-- Set different colors for urgent notifications.
-- rnotification.connect_signal('request::rules', function()
--     rnotification.append_rule {
--         rule       = { urgency = 'critical' },
--         properties = { bg = theme.bg_urgent, fg = theme.fg_urgent }
--     }
-- end)

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
