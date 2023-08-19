local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")

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
        style = {
            font = "Hack Nerd Font 10",
            disable_task_name = false,

            fg_normal = "#cdd6f4",
            bg_normal = "#1e1e2e",

            fg_focus = "#bac2de",
            bg_focus = "#313244",

        },
        buttons = tasklist_buttons
    }

    -- Create the wibox
    s.mywibox = awful.wibar({
        screen = s,
        position = "bottom",
        height = 18
    })

    -- Add widgets to the wibox
    s.mywibox:setup {
        layout = wibox.layout.flex.horizontal,
        s.mytasklist,
    }
end)
