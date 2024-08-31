local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")
local dpi =  beautiful.xresources.apply_dpi

-- Colors idk
local image = wibox.widget {
	image = os.getenv("HOME") .. '/Pictures/nixos-icon.svg',
	widget = wibox.widget.imagebox,
	resize = true,
	forced_height = dpi(20),
	forced_width = dpi(20),
	halign = 'center'
}

local separator = wibox.widget.textbox("   ")
separator.forced_height = dpi(50)
separator.forced_width = dpi(50)

-- Slider
local volume_slider = wibox.widget({
    widget = wibox.widget.slider,
    bar_shape = function (cr, width, height)
        gears.shape.rounded_rect(cr, width, height, 15)
    end,
    bar_height = dpi(10),
    bar_color = "#232a2e",
    bar_active_color  = "#d3c6aa",
    handle_shape = nil,
    handle_width = 0,
    -- handle_color = "#00FFFF",
    -- handle_border_width = 1,
    -- handle_border_color = "#00FFFF",
    minimum = 0,
    maximumm = 100,
    value = 69,
})

-- Signals
volume_slider:connect_signal("property::value", function (slider)
    local volume_level = math.floor(slider.value)
    awful.spawn("amixer set Master " .. volume_level .. "%")
end)

local volume_text = wibox.widget ({
    markup = '<span color="' .. "#d3c6aa" .. '" font="Ubuntu Nerd Font bold 12">' .. 'Volume' .. '</span>'
    ,
    widget = wibox.widget.textbox,
    fg = "#FFFFFF",
})

local volume_percentage = wibox.widget {
	markup = '<span color="' ..
		"#d3c6aa" .. '" font="Ubuntu Nerd Font bold 12">' .. volume_slider.value .. '</span>'
	,
	widget = wibox.widget.textbox,
	fg = "#FFFFFF",
}

local update_volume_slider = function()
	awful.spawn.easy_async("amixer sget Master", function(stdout)
		local volume = tonumber(string.match(stdout, "(%d?%d?%d)%%"))
		volume_slider.value = volume or 0
		volume_percentage.markup = '<span color="' ..
			"#d3c6aa" .. '" font="Ubuntu Nerd Font bold 12">' .. volume .. '</span>'
	end)
end

local volume_slider_timer = gears.timer({
	timeout = 1,
	call_now = true,
	autostart = true,
	callback = update_volume_slider,
})

-- Main popup
local osd_box = awful.popup {
    screen = s,
    widget = wibox.container.background,
    ontop = true,
    bg = "#00000000",
    visible = false,
    placement = function(c)
        awful.placement.bottom(c,
            { margins = {
                top = dpi(0),
                bottom = dpi(100),
                left = 0,
                right = dpi(0),
            }})
    end,
    opacity = 1
}

osd_box:setup {
    {
        {
            {
                image,
                widget = wibox.container.margin,
                -- top = dpi(15),
                bottom = dpi(0),
                right = dpi(6),
                left = dpi(6),
            },
            {
                volume_slider,
                layout = wibox.layout.align.horizontal,
				-- bottom = dpi(15),
				-- top = dpi(0),
				-- left = dpi(25),
				-- right = dpi(25),
				forced_height = dpi(10),
				forced_width = dpi(160),
				widget = wibox.container.margin,
            },
            {
                {
                    -- volume_text,
                    -- nil,
                    volume_percentage,
                    layout = wibox.layout.align.horizontal
                },
                widget = wibox.container.margin,
				left = dpi(6),
				right = dpi(6),
				-- top = dpi(0),
				-- bottom = dpi(15)
            },
            widget = wibox.container.place,
			halign = 'center',
			layout = wibox.layout.fixed.horizontal
        },
        -- separator,
        widget = wibox.container.margin,
        top = dpi(8),
        bottom = dpi(8),
    },
    widget = wibox.container.background,
	bg = "#3d484d",
	shape = function(cr, width, height)
		gears.shape.rounded_rect(cr, width, height, 30)
	end,
}

return osd_box
