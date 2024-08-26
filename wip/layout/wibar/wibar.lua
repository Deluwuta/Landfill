local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")

local dpi = beautiful.xresources.apply_dpi

local battery = require("layout.wibar.widgets.batteryarc")
-- local network = require("layout.wibar.widgets.network")
local fancy_button = require("layout.wibar.widgets.fancy_button")

local spacer = wibox.widget.textbox(" ")
local separator = wibox.widget({
    {
        id = "separator",
        text = "|",
        font = beautiful.font,
        widget = wibox.widget.textbox,
    },
    bg = beautiful.bg_normal,
    fg = beautiful.fg_normal,
    widget = wibox.container.background,
})

-- Main widgets

-- Keyboard map indicator and switcher
local mykeyboardlayout = awful.widget.keyboardlayout()

-- Create a textclock widget
local clock = wibox.widget({
    {
        id = "clock",
        format = "<b>" .. "%a, %d/%m ~ %H:%M" .. "</b>",
        refresh = 10,
        widget = wibox.widget.textclock(),
    },
    bg = beautiful.bg_normal,
    fg = beautiful.fg_normal,
    widget = wibox.container.background,
})

-- THA BAR
-- Cannot use this rn
-- local thebar = awful.wibar({
--     position = "bottom",
--     margins = { top = 0, bottom = 6, left = 3, right = 3 },
--     height = 24,
--     opacity = 1,
--
--     bg = beautiful.bg_normal,
--     fg = beautiful.fg_normal,
--
--     screen = s
-- })

screen.connect_signal("request::desktop_decoration", function(s)
    -- Each screen has its own tag table.
    awful.tag({ "1", "2", "3", "4", "5", "6", "7", "8", "9" }, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()

    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox {
        screen  = s,
        buttons = {
            awful.button({ }, 1, function () awful.layout.inc( 1) end),
            awful.button({ }, 3, function () awful.layout.inc(-1) end),
            awful.button({ }, 4, function () awful.layout.inc(-1) end),
            awful.button({ }, 5, function () awful.layout.inc( 1) end),
        }
    }

    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen  = s,
        filter  = awful.widget.taglist.filter.all,
        buttons = {
            awful.button({ }, 1, function(t) t:view_only() end),
            awful.button({ mod }, 1, function(t)
                                            if client.focus then
                                                client.focus:move_to_tag(t)
                                            end
                                        end),
            awful.button({ }, 3, awful.tag.viewtoggle),
            awful.button({ mod }, 3, function(t)
                                            if client.focus then
                                                client.focus:toggle_tag(t)
                                            end
                                        end),
            awful.button({ }, 4, function(t) awful.tag.viewprev(t.screen) end),
            awful.button({ }, 5, function(t) awful.tag.viewnext(t.screen) end),
        }
    }

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist {
        screen  = s,
        filter  = awful.widget.tasklist.filter.currenttags,
        buttons = {
            awful.button({ }, 1, function (c)
                c:activate { context = "tasklist", action = "toggle_minimization" }
            end),
            awful.button({ }, 3, function() awful.menu.client_list { theme = { width = 250 } } end),
            awful.button({ }, 4, function() awful.client.focus.byidx(-1) end),
            awful.button({ }, 5, function() awful.client.focus.byidx( 1) end),
        }
    }

    -- Create the wibox
    s.mywibox = awful.wibar {
        position = "bottom",
        margins = { top = 0, bottom = 6, left = 3, right = 3 },
        height = 28,
        opacity = 1,

        bg = beautiful.bg_normal,
        fg = beautiful.fg_normal,

        screen = s,

        widget = {
            -- La pila como la entiendo es que los widgets se ponen uno encima de otro
            -- Si se mete otro widget junto al clock, estos se superpondrán
            layout = wibox.layout.stack,
            expand = "none",
            {
                layout = wibox.layout.align.horizontal,
                {
                    -- Left widgets
                    layout = wibox.layout.fixed.horizontal,
                    spacer,
                    fancy_button,
                    spacer,
                    s.mytaglist,
                    separator,
                    s.mypromptbox,
                    s.mylayoutbox,
                    separator,
                },
                nil,
                {
                    -- Right widgets
                    layout = wibox.layout.fixed.horizontal,
                    separator,
                    -- network,
                    -- separator,
                    mykeyboardlayout,
                    separator,
                    spacer,
                    battery({
                        show_current_level = true,
                        size = 26,
                        arc_thickness = 3,
                        timeout = 60,
                        notification_position = 'bottom_right',
                    }),
                    spacer,
                    wibox.widget.systray(),
                },
            },

            {
                -- Middle widgets
                clock,

                valign = "center",
                halign = "center",
                layout = wibox.container.place,
            },
        }
    }
end)
