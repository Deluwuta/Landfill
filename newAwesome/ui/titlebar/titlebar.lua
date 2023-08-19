-- Imports
local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")

local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

-- Titlebar behaviour
-- To not repeat code. 
-- True if floting, otherwise false
local auxFunc = function (c)
    if c.floating or c.first_tag.layout.name == "floating" then
        awful.titlebar.show(c)
    else
        awful.titlebar.hide(c)
    end
end

client.connect_signal("manage", auxFunc)

-- This function hides or shows titlebars when changing from floating to tile and viceversa on the fly
tag.connect_signal("property::layout", function(t)
    local clients = t:clients()
    for k,c in pairs(clients) do
       auxFunc(c)
    end
end)

-- Double click to maximize
function double_click_event_handler(double_click_event)
    if double_click_timer then
        double_click_timer:stop()
        double_click_timer = nil
        return true
    end

    double_click_timer = gears.timer.start_new(0.20, function()
        double_click_timer = nil
        return false
    end)
end

-- Functionality
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = gears.table.join(
        awful.button({ }, 1, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
	    -- Double click momento
	    if double_click_event_handler() then
	       c.maximized = not c.maximized
	       c:raise()
	    else
	       awful.mouse.client.move(c)
	    end
        end),
        awful.button({ }, 3, function()
            c:emit_signal("request::activate", "titlebar", {raise = true})
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c, { position = "top", size = dpi(16), font = "FantasqueSansM Nerd Font Propo 12", bg = "#363a4f", fg = "#cdd6f4"}) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.maximizedbutton(c),
	    awful.titlebar.widget.minimizebutton(c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)
