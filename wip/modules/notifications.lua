local awful = require("awful")
local gears = require("gears")
local naughty = require("naughty")
local ruled = require("ruled")
local wibox = require("wibox")

local beautiful = require("beautiful")
local dpi = beautiful.xresources.apply_dpi

local user = require("utils.user_variables")
local color = require("themes." .. user.theme)

-- Naughty defaults table
-- timeout, text, screen, ontop, margin, border_width, position
naughty.config.defaults.ontop = true
naughty.config.defaults.margin = dpi(16)
naughty.config.defaults.border_width = dpi(2)
naughty.config.defaults.position = "top_right"

-- Properties template
local template_properties = function (_timeout, _border_color)
    return 
        {
            bg = color.bg_dim,
            fg = color.fg_normal,
            timeout = _timeout,
            border_color = _border_color,
            shape = gears.shape.rounded_rect,
            opacity = 1,
        }
end

-- Rules---------------
ruled.notification.connect_signal('request::rules', function()
	-- Critical
	ruled.notification.append_rule {
		rule       = { urgency = 'critical' },
		properties = {
            -- bg = color.bg_dim,
            fg = color.fg_normal,
            timeout = 0,
            border_color = color.red,
            opacity = 1,
		}
	}

	-- Normal
	ruled.notification.append_rule {
		rule       = { urgency = 'normal' },
		properties = {
            -- bg = color.bg_dim,
            fg = color.fg_normal,
            timeout = 3,
            border_color = beautiful.extra_colors.pinkish_white,
            opacity = 1,
		}
	}

	-- Low
	ruled.notification.append_rule {
		rule       = { urgency = 'low' },
		properties = {
            -- bg = color.bg_dim,
            fg = color.fg_normal,
            timeout = 3,
            border_color = beautiful.extra_colors.pinkish_white,
            opacity = 1,
		}
	}
end)

naughty.connect_signal("request::display", function (n)
    naughty.layout.box {
        notification = n,
        type = "notification",
        bg = color.bg_dim,
        shape = gears.shape.rounded_rect,
        -- widget_template = {
        --         naughty.widget.title,
        --         naughty.widget.message,
        -- },
        widget_template = {
            {
                {
                    {
                        {
                            naughty.widget.title,
                            font = beautiful.font,
                            -- forced_height = dpi(20),
                            layout = wibox.layout.align.horizontal,
                        },
                        -- left = dpi(8),
                        -- top = dpi(8),
                        widget = wibox.container.margin,
                    },
                    {
                        {
                            resize_strategy = 'center',
                            widget = naughty.widget.icon or "/home/delta/.config/awesome/utils/icons/volume-up.png",
                        },
                        {
                            {
                                naughty.widget.message,
                                spacing = 4,
                                layout  = wibox.layout.align.horizontal,
                            },
                            -- left = dpi(8),
                            -- bottom = dpi(8),
                            -- right = dpi(8),
                            widget = wibox.container.margin,
                        },
                        layout = wibox.layout.align.horizontal,
                    },
                    layout = wibox.layout.align.vertical,
                },
                margins = dpi(12),
                widget  = wibox.container.margin,
            },
            id = "background_role",
            bg = color.fg_normal,
            widget = naughty.container.background,
        },
    }
end)
