require("core.plugin_config.catppuccin")
require("core.plugin_config.lualine")
require("core.plugin_config.nvim-tree")
require("core.plugin_config.telescope")
require("core.plugin_config.treesitter")

-- Highlight current line number :^)
vim.cmd[[ highlight CursorLineNr guifg=#17FFFF gui=bold ]]

-- To keep cursor shape when exiting Neovim (block | ver25 = I-Beam | hor20 = underline)
vim.cmd [[ 
  augroup change_coursor
    au!
    au ExitPre * :set guicursor=a:hor25
  augroup END
]]
