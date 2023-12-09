local wibox = require("wibox")
local awful = require("awful")
local beautiful = require("beautiful")

-- Crear widgets
local widget_left = wibox.widget.textbox("Left Widget")
local widget_middle = wibox.widget.textbox("Middle Widget")
local widget_right = wibox.widget.textbox("Right Widget")

-- Configurar el diseño de los widgets
local layout_left = wibox.layout.align.horizontal()
layout_left:set_left(widget_left)

local layout_middle = wibox.layout.align.horizontal()
layout_middle:set_middle(widget_middle)

local layout_right = wibox.layout.align.horizontal()
layout_right:set_right(widget_right)

-- Configurar la wibox con layout.ratio.horizontal
local mywibox = awful.wibar({ position = "top", screen = 1 })
mywibox:setup {
    layout = wibox.layout.ratio.horizontal,
    {
        layout = wibox.layout.ratio.horizontal,
        {
            layout = wibox.layout.flex.horizontal,
            layout_left,
        },
        ratio = 0.1,
    },
    {
        layout = wibox.layout.ratio.horizontal,
        {
            layout = wibox.layout.flex.horizontal,
            layout_middle,
        },
        ratio = 0.8,
    },
    {
        layout = wibox.layout.ratio.horizontal,
        {
            layout = wibox.layout.flex.horizontal,
            layout_right,
        },
        ratio = 0.1,
    },
}

-- Configurar colores
mywibox.bg = beautiful.bg_normal
widget_left.bg = beautiful.bg_normal
widget_middle.bg = beautiful.bg_normal
widget_right.bg = beautiful.bg_normal
