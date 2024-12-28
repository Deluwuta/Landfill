-- Touched
local gears = require("gears")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi

local boton = wibox.widget({
    {
        {
            widget = wibox.widget.imagebox,
            image = os.getenv("HOME") .. "/Pictures/nixos-icon.svg",
            resize = true,
            opacity = 1,
        },
        left = dpi(1),
        right = dpi(1),
        top = dpi(1),
        bottom = dpi(1),
        widget = wibox.container.margin
    },
    bg = beautiful.bg_normal,
    shape = gears.shape.rounded_rect,
    widget = wibox.container.background,
})

-- To be used as a guide
-- boton:connect_signal("mouse::enter", function(c) c:set_bg("#000000") end)
-- boton:connect_signal("mouse::leave", function(c) c:set_bg("#4e4e4e") end)
-- boton:connect_signal("button::press", function(c) c:set_bg("#ffffff") end)
-- boton:connect_signal("button::release", function(c, _, _, button)
-- 	c:set_bg("#000000")
-- 	if button == 1 then awful.spawn("alacritty") end
-- end)

return boton
