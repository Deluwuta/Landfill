local wibox = require("wibox")
local watch = require("awful.widget.watch")
local gears = require("gears")
local awful = require("awful")
local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi

local wifi_name = wibox.widget.textbox()
local wifi_icon = wibox.widget.textbox()

wifi_name.font = beautiful.widget_text
wifi_icon.font = beautiful.widget_icon

local wifi_widget = wibox.widget ({
    {
        wifi_icon,
        fg = "#ffffff",
        widget = wibox.container.background
    },
    {
        wifi_name,
        fg = "#fefefe",
        widget = wibox.container.background
    },
    spacing = dpi(4),
    layout = wibox.layout.fixed.horizontal,
})

-- local wifi_tooltip = awful.tooltip({
--     objects = { wifi_widget },
--     mode = "outside",
--     align = "right",
--     delay_show = 1,
--     border_width = dpi(1),
--     bg = beautiful.bg_tooltip,
--     fg = beautiful.fg_wifi,
--     border_color = beautiful.black,
--     shape = gears.shape.rounded_rect,
--     font = beautiful.widget_text,
--     margins = dpi(8),
-- })

local function update_widget(ssid)
    wifi_icon.text = "YE"
    wifi_name.text = ssid
end

watch([[ bash -c 
    "nmcli dev status | grep 'connected' | awk '{ print $2 }' | head -n1"
]], 20, function (_, stdout)
    update_widget(stdout)
    collectgarbage("collect")
end)

return wifi_widget
