local wibox = require("wibox")
local gears = require("gears")
local gfs = gears.filesystem

local beautiful = require("beautiful")
local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local get_container = function (w, colorfg)
  local basurero =
  {
    {
      {
        {
          {
            widget = w,
          },
          left = dpi(10),
          right = dpi(10),
          top = dpi(1),
          bottom = dpi(1),
          widget = wibox.container.margin,
        },
        fg = colorfg,
        widget = wibox.container.background,
      },
      margins = 2,
      widget = wibox.container.margin,
    },
    spacing = 5,
    layout = wibox.layout.fixed.horizontal,
  }
  return basurero
end

return get_container
