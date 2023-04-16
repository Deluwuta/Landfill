--require("alpha").setup{}

require("nvim-highlight-colors").setup {
    render = "background", -- background | foreground | first_column
    enable_named_colors = true,
    enable_tailwind = false,
}

-- LSP charging animation
require("fidget").setup {}

require("lualine").setup {
    options = {
        icons_enabled = true,
        theme = "rose-pine",
    },
    sections = {
        lualine_a = {
            {
                'filename',
                path = 1
            }
        }
    }
}

-- Highlight current line number :^)
vim.cmd.set("cursorline")
vim.cmd [[ highlight CursorLineNr guifg=gold gui=bold ]]

-- To keep cursor shape when exiting Neovim (block | ver25 = I-Beam | hor20 = underline)
vim.cmd [[
  augroup change_coursor
    au!
    au ExitPre * :set guicursor=a:hor25
  augroup END
]]
