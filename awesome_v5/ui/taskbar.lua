local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")

local empty_widget = wibox.widget.textbox("")
local separator = wibox.widget.textbox(" | ")
local espacer = wibox.widget.textbox(" ")

local tasklist_buttons = gears.table.join(
    awful.button({ }, 1, function (c)
        if c == client.focus then
            c.minimized = true
        else
            c:emit_signal(
                "request::activate",
                "tasklist",
                {raise = true})
        end
    end),
    awful.button({ }, 3, function()
        awful.menu.client_list({ theme = { width = 250 }})
    end),
    awful.button({ }, 4, function () awful.client.focus.byidx(1) end),
    awful.button({ }, 5, function () awful.client.focus.byidx(-1) end)
)

awful.screen.connect_for_each_screen(function(s)

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = tasklist_buttons,
        style = {
            font = beautiful.font,
            disable_icon = false,
            align ="center",

            fg_normal = beautiful.fg_normal,
            bg_normal = beautiful.bg_normal,

            fg_focus = beautiful.darkest_dark,
            bg_focus = beautiful.white2,
        },
        layout = {
            spacing = 20,
            spacing_widget = {
                {
                    forced_width = 5,
                    shape = gears.shape.circle,
                    widget = wibox.widget.separator,
                },
                valign = "center",
                halign = "center",
                widget = wibox.container.place,
            },
            layout = wibox.layout.flex.horizontal
        },
    }

    -- Create the wibox
    s.mywibox = awful.wibar({
        screen = s,
        position = "top",
        height = 18
    })

    -- Add widgets to the wibox
    s.mywibox:setup({
        layout = wibox.layout.ratio.horizontal,
        { -- Left widgets
            layout = wibox.layout.align.horizontal,
            wibox.widget.textbox(" Izquierda"),
            empty_widget,
            separator,
        },
        { -- Middle widgets
            layout = wibox.layout.flex.horizontal,
            -- espacer,
            s.mytasklist,
            -- espacer,
        },
        { -- Right widgets
            layout = wibox.layout.align.horizontal,
            separator,
            empty_widget,
            wibox.widget.textbox("Derecha "),
        },
    })
    s.mywibox.widget:adjust_ratio(2, 0.1, 0.8, 0.1)
end)
