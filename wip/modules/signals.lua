local gears = require("gears")
local naughty = require("naughty")

naughty.connect_signal("request::display", function(n)
    naughty.layout.box { notification = n }
end)

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
