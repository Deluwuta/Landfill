-- Catppuccin theme
require("catppuccin").setup({
  flavour = "mocha", -- latte, frappe, macchiato, mocha
  background = { -- :h background
    light = "latte",
    dark = "mocha",
  },
  transparent_background = true,
  show_end_of_buffer = false, -- show the '~' characters after the end of buffers
    term_colors = false,
    dim_inactive = {
        enabled = false,
        shade = "dark",
        percentage = 0.15,
    },
    no_italic = false, -- Force no italic
    no_bold = false, -- Force no bold
    styles = {
        comments = { "italic" },
        conditionals = { "italic" },
        loops = {},
        functions = {},
        keywords = {},
        strings = {},
        variables = {},
        numbers = {},
        booleans = {},
        properties = {},
        types = {},
        operators = {},
    },
    color_overrides = {},
    custom_highlights = {},
    integrations = {
        cmp = true,
        gitsigns = true,
        nvimtree = true,
        telescope = true,
        notify = false,
        mini = false,
        -- For more plugins integrations please scroll down (https://github.com/catppuccin/nvim#integrations)
    },
})

require("tokyonight").setup({
  style = "storm", -- storm | moon | night | day
  light_style = "day",
  transparent = true, -- Background
  terminal_colors = true,
  styles = {
    -- Style to be applied to different syntax groups
    comments = { italic = true },
    keywords = { bold = true },
    functions = {},
    variables = {},
    -- Background styles
    sidebars = "dark",
    floats = "dark",
  },
  sidebars = { "qf", "help" },
  day_brightness = 0.3, -- Only for *Day* theme. 0 - 1. Dull - Vibrant
  hide_inactive_statusline = true, -- 
  dim_inactive = true, -- Dims inactive windows
  lualine_bold = true,
})

-- Colorscheme
vim.cmd [[ colorscheme tokyonight ]]
