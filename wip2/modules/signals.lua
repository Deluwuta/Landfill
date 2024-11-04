local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")

-- naughty.connect_signal("request::display", function(n)
--     naughty.layout.box { notification = n }
-- end)

-- Rounded borders
client.connect_signal("request::manage", function(c)
	c.shape = function(cr, w, h)
		gears.shape.rounded_rect(cr, w, h, 0)
	end
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    c:activate {
        context = "mouse_enter",
        raise = false
    }
end)

-- Dinamically show the titlebar when the client floats
-- client.connect_signal("property::floating", function(c)
--   if c.floating and not c.requests_no_titlebar then
--     awful.titlebar.show(c)
--   else
--     awful.titlebar.hide(c)
--   end
-- end)
--
-- awful.tag.attached_connect_signal(nil, "property::layout", function (t)
--   local float = t.layout.name == "floating"
--   for _,c in pairs(t:clients()) do
--     c.floating = float
--   end
-- end)
