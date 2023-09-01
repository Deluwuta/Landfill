local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

-- {{{ Wallpaper
-- It doesn't zoom to fill the screen, so I won't use it for now
--screen.connect_signal("request::wallpaper", function(s)
--    awful.wallpaper {
--        screen = s,
--        widget = {
--            {
--                image     = beautiful.wallpaper,
--                resize = true,
--                upscale   = true,
--                downscale = true,
--                widget    = wibox.widget.imagebox,
--            },
--            valign = "center",
--            halign = "center",
--            tiled  = false,
--            widget = wibox.container.tile,
--        }
--    }
--end)
-- }}}

require(... .. ".wibar.wibar")
require(... .. ".titlebar.titlebar")
