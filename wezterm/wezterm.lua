-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

-- This is where you actually apply your config choices

-- UI modifications
config.enable_tab_bar = false
config.default_cursor_style = "BlinkingUnderline"

-- Theme
config.color_scheme = 'Ashes (dark) (terminal.sexy)'
config.font = wezterm.font 'JetBrains Mono'
config.font_size = 16.0

-- Others 
config.audible_bell = "Disabled"
config.warn_about_missing_glyphs = false

-- Wayland
config.enable_wayland = true

-- and finally, return the configuration to wezterm
return config
