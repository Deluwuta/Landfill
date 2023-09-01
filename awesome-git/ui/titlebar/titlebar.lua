local awful = require("awful")
local wibox = require("wibox")
local gears = require("gears")
local beautiful = require("beautiful")

local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local hide_titlebar = function (c)
  if c.floating or c.first_tag.layout.name == "floating" then
    awful.titlebar.show(c)
  else
    awful.titlebar.hide(c)
  end
end

client.connect_signal("manage", hide_titlebar)

-- Hides or shows titlebars on the fly
tag.connect_signal("property::layout", function (t)
  local clients = t:clients()
  for k,c in pairs(clients) do
    hide_titlebar(c)
  end
end)

client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = {
        awful.button({ }, 1, function()
            c:activate { context = "titlebar", action = "mouse_move"  }
        end),

        awful.button({ }, 2, function ()
          c:emit_signal("request::activate", "titlebar", { raise = true })
          c:kill()
          -- c:activate { context = "titlebar", action = "client_kill" }
        end),

        awful.button({ }, 3, function()
            c:activate { context = "titlebar", action = "mouse_resize"}
        end),
    }

    awful.titlebar(c).widget = {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                halign = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.minimizebutton(c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)
