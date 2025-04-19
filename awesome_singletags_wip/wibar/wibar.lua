local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")

local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi

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

-- Keyboard map indicator and switcher
local mykeyboardlayout = awful.widget.keyboardlayout()

-- Create a textclock widget
local mytextclock = wibox.widget({
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

screen.connect_signal("request::desktop_decoration", function(s)
    -- Each screen has its own tag table.
    -- α, β, γ, Δ, ε, ζ, η, θ, λ, μ, π, ρ, Σ, τ, φ, Ψ, Ω.
    -- awful.tag({ "α", "β", "γ", "Δ", "λ", "μ", "π", "φ", "Ω" }, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()

    -- Create an imagebox widget which will contain an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox {
        screen = s,
        buttons = {
            awful.button({ }, 1, function () awful.layout.inc( 1) end),
            awful.button({ }, 3, function () awful.layout.inc(-1) end),
            awful.button({ }, 4, function () awful.layout.inc(-1) end),
            awful.button({ }, 5, function () awful.layout.inc( 1) end),
        }
    }

    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist {
        screen = s,
        filter = awful.widget.taglist.filter.all,
        buttons = {
            awful.button({ }, 1, function(t) t:view_only() end),
            awful.button({ modkey }, 1,
                function(t)
                    if client.focus then
                        client.focus:move_to_tag(t)
                    end
                end
            ),
            awful.button({ }, 3, awful.tag.viewtoggle),
            awful.button({ modkey }, 3,
                function(t)
                    if client.focus then
                        client.focus:toggle_tag(t)
                    end
                end
            ),
            awful.button({ }, 4, function(t) awful.tag.viewprev(t.screen) end),
            awful.button({ }, 5, function(t) awful.tag.viewnext(t.screen) end),
        }
    }

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist {
        screen = s,
        filter = awful.widget.tasklist.filter.currenttags,
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
        position = "top",
        height = 28,
        screen = s,
        widget = {
            layout = wibox.layout.align.horizontal,
            { -- Left widgets
                layout = wibox.layout.fixed.horizontal,

                s.mytaglist,
                spacer,
                s.mylayoutbox,
                s.mypromptbox,
            },
            {
                layout = wibox.layout.fixed.horizontal,
                -- s.mytasklist, -- Middle widget
            },
            { -- Right widgets
                layout = wibox.layout.fixed.horizontal,
                wibox.widget.systray(),
                separator,
                mykeyboardlayout,
                separator,
                spacer,
                mytextclock,
                spacer,
            },
        }
    }
end)
