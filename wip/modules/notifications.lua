local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")
local ruled = require("ruled")
local wibox = require("wibox")

local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi

local user = require("utils.user_variables")
local color = require("themes." .. user.theme)

naughty.bg = color.mid_light

-----------------------
--Defaults-------------
-----------------------
naughty.config.defaults.ontop = true
naughty.config.defaults.screen = awful.screen.focused()
naughty.config.defaults.border_width = 0
naughty.config.defaults.position = "top_right"
naughty.config.defaults.title = "Notification"
naughty.config.defaults.margin = dpi(16)

-----------------------
-- Rules---------------
-----------------------
ruled.notification.connect_signal('request::rules', function()
	-- Critical
	ruled.notification.append_rule {
		rule       = { urgency = 'critical' },
		properties = {
			bg = color.bg_dim,
			fg = color.fg_normal,
			timeout = 0,
		}
	}

	-- Normal
	ruled.notification.append_rule {
		rule       = { urgency = 'normal' },
		properties = {
			bg = beautiful.bg_dim,
			fg = color.fg_normal,
			timeout = 5,
		}
	}

	-- Low
	ruled.notification.append_rule {
		rule       = { urgency = 'low' },
		properties = {
			bg = beautiful.bg_dim,
			fg = color.fg_normal,
			timeout = 5,
		}
	}
end)

-- ruled.notification.connect_signal('request::rules', function()
--     -- All notifications will match this rule.
--     ruled.notification.append_rule {
--         rule       = { },
--         properties = {
--             screen           = awful.screen.preferred,
--             implicit_timeout = 3,
--         }
--     }
-- end)
