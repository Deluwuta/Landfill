require("nvim-highlight-colors").setup({
    render = "background", -- background | foreground | first_column
    enable_named_colors = true,
    enable_tailwind = false,
})

-- Lualine
require("lualine").setup({
    options = {
        icons_enabled = true,
    },
    sections = {
        lualine_a = {
            {
                'filename',
                path = 1,
            }
        }
    }
})
