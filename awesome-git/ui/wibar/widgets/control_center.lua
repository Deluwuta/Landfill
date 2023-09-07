local gears = require("gears")
local awful = require("awful")
local wibox = require("wibox")
local beautiful = require("beautiful")

---------------------------------------------------
--Control Center Widget
----------------------------------------------------

-- Segun veo estos widgets al final con un popup con cosas que estan visibles o no dependiendo de una "signal". 
-- Para crear este widget hay que ver el "objeto" que lo activa (a lo que asociamos la signal) y el disenio del popio widget asi como su funcionalidad

-- Button widget
local button_control = awful.widget.button({
  image = "/home/delta/Pictures/nixos-icon.svg",
  widget = wibox.widget.imagebox,
})

-- Sliders
local volume_slider = wibox.widget({
  bar_shape = gears.shape.rounded_rect,
  bar_height = 10,
  bar_color = beautiful.white,
  handle_shape = gears.shape.circle,
  handle_color = beautiful.color6,
  handle_width = 30,
  handle_border_width = 1,
  handle_border_color = beautiful.color7,
  minimum = 0,
  maximum = 100,
  value = 20,
  widget = wibox.widget.slider,
})

local brightness_slider = wibox.widget({
	bar_shape = gears.shape.rounded_rect,
	bar_height = 10,
	bar_color = "#b7b2f1",
	handle_shape = gears.shape.circle,
	handle_color = "#ffffff",
	handle_width = 30,
	handle_border_width = 1,
	handle_border_color = "#aaaaaa",
	minimum = 5,
	maximum = 100,
	value = 30,
	widget = wibox.widget.slider,
})

local vol_popup = awful.popup({
  widget = {
    widget = wibox.widget.imagebox,
    image = "/home/delta/Pictures/icons8-volume-100.png",
    resize = true,
    opacity = 1,
  },
	bg = "#00000099",
	border_color = "#00000099",
	border_width = 10,
	ontop = true,
  placement = awful.placement.centered,
	shape = gears.shape.rounded_rect,
	visible = false,
	forced_width = 200,
	forced_height = 200,
})

local popup2 = awful.popup({
	widget = {
		{
			{
				{
					widget = wibox.widget.imagebox,
					image = "/home/delta/Pictures/hellap_crop.png",
					resize = true,
					opacity = 1,
				},
				{
					volume_slider,
					widget = wibox.container.margin,
					margins = 10,
					forced_width = 350,
					forced_height = 60,
				},
				layout = wibox.layout.fixed.horizontal,
				forced_width = 400,
				forced_height = 60,
			},
			{
				{
					id = "brightness",
					widget = wibox.widget.imagebox,
					image = os.getenv("HOME") .. "/.icons/papirus-icon-theme-20230301/Papirus/brightness2.png",
					resize = true,
					opacity = 1,
				},
				{
					brightness_slider,
					widget = wibox.container.margin,
					margins = 10,
					forced_width = 350,
					forced_height = 60,
				},
				layout = wibox.layout.fixed.horizontal,
				forced_width = 400,
				forced_height = 60,
			},
			layout = wibox.layout.fixed.vertical,
		},
		margins = 10,
		widget = wibox.container.margin,
	},
	bg = "#00000099",
	border_color = "#00000099",
	border_width = 10,
	ontop = true,
	placement = function(c)
		local screen_geometry = awful.screen.focused().geometry
		return awful.placement.top_right(c, { margins = { right = 10, top = 35 } })
	end,
	shape = gears.shape.rounded_rect,
	visible = false,
	forced_width = 400,
	forced_height = 200,
})

button_control:connect_signal("button::release", function ()
  popup2.visible = not popup2.visible
end)

return vol_popup
