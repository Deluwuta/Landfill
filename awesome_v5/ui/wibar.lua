local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")

local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

-- {{{ Wibar
local separator = wibox.widget {
    {
        id = "separator",
        text = " | ",
        font = beautiful.font_name .. "bold 11",
        widget = wibox.widget.textbox,
    },
    bg = beautiful.bg_normal,
    fg = beautiful.bg_white2,
    widget = wibox.container.background,
}

-- Keyboard map indicator and switcher
local keyboardlayout = awful.widget.keyboardlayout()

-- Create a textclock widget
local textclock = wibox.widget({
  {
    id = "clock",
    format = "<b>" .. "%a, %d/%m ~ %H:%M" .. "</b>",
    refresh = 5,
    widget = wibox.widget.textclock(),
  },
  bg = beautiful.bg_normal,
  fg = beautiful.light_blue,
  widget = wibox.container.background,
})

-- local battery = require("ui.widgets.battery")

screen.connect_signal("request::desktop_decoration", function(s)
    -- Each screen has its own tag table.
    awful.tag({ "α", "β", "γ", "Δ", "Ε", "ζ", "λ", "φ", "Ω" }, s, awful.layout.layouts[1])

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
        screen   = s,
        widget   = {
            layout = wibox.layout.align.horizontal,
            { -- Left widgets
                layout = wibox.layout.fixed.horizontal,
                s.mytaglist,
                separator,
                s.mypromptbox,
                s.mylayoutbox,
                separator,
            },
            { -- Middle widgets
                layout = wibox.layout.fixed.horizontal,
                -- s.mytasklist,
            },
            { -- Right widgets
                layout = wibox.layout.fixed.horizontal,
                separator,
                -- require("ui.widgets.net"),
                require("ui.widgets.netless")("enp0s3"),
                separator,
                keyboardlayout,
                separator,
                -- battery({show_current_level = true}),
                separator,
                textclock,
                separator,
                wibox.widget.systray(),
            },
        }
    }
end)

-- }}}
