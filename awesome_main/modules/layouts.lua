local awful = require("awful")

-- Available layouts
awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.floating,
    awful.layout.suit.max,
}

-- Overwrite default layouts
return awful.layout.layouts
