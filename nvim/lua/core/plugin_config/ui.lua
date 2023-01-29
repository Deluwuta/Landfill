--require("alpha").setup{}

require("bufferline").setup{}

--local colorOpts = {
--  filetypes = {
--    "*",
--  },
--  user_default_options = {
--    RGB = true, -- #RGB hex codes
--    RRGGBB = true, -- #RRGGBB hex codes
--    names = false, -- "Name" codes like Blue
--    RRGGBBAA = false, -- #RRGGBBAA hex codes
--    rgb_fn = false, -- CSS rgb() and rgba() functions
--    hsl_fn = false, -- CSS hsl() and hsla() functions
--    css = false, -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
--    css_fn = false, -- Enable all CSS *functions*: rgb_fn, hsl_fn
--    mode = "background", -- Set the display mode.
--  },
--}
--require("colorizer").setup(colorOpts)
---- execute colorizer as soon as possible
--vim.defer_fn(function()
--  require("colorizer").attach_to_buffer(0)
--end, 0)

require("nvim-highlight-colors").setup {
  render = "background", -- background | foreground | first_column
  enable_named_colors = true,
  enable_tailwind = false,
}

-- LSP charging animation
require("fidget").setup{}

require("indent_blankline").setup{
  indentLine_enabled = 1,
  filetype_exclude = {
      "",
      "NvimTree",
      "Octo",
      "TelescopePrompt",
      "Trouble",
      "alpha",
      "git",
      "help",
      "markdown",
      "undotree",
  },
  buftype_exclude = { "terminal", "nofile" },
  char = "│",
  char_list_blankline = { "|", "┊", "┆", "¦" },
  space_char_blankline = " ",

  show_trailing_blankline_indent = false,
  show_first_indent_level = false,
  show_current_context = true,
  show_current_context_start = true,
  --context_patterns = {
  --    "class",
  --    "function",
  --    "method",
  --    "block",
  --    "list_literal",
  --    "selector",
  --    "^if",
  --    "^table",
  --    "if_statement",
  --    "while",
  --    "for",
  --    "type",
  --    "var",
  --    "import",
  --},
}

require("lualine").setup {
  options = {
    icons_enabled = true,
    theme = "tokyonight",
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

-- Not really needed
--require("notify").setup{}
--
--require("noice").setup{
--  hacks = { cmp_popup_row_offset = 1 },
--  views = {
--    mini = {
--      position = { row = "90%", col = "100%" },
--    },
--    cmdline_popup = {
--      position = { row = "30%", col = "50%" },
--      size = { width = "40%", height = "auto" },
--    },
--  },
--  cmdline = {
--    enabled = false,
--  },
--  messages = {
--    enabled = false,
--  },
--  lsp = {
--    override = {
--      ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
--      ["vim.lsp.util.stylize_markdown"] = true,
--      ["cmp.entry.get_documentation"] = true,
--    },
--  },
--  presets = {
--    bottom_search = false,
--    command_palette = true,
--    long_message_to_split = true,
--    inc_rename = false,
--    lsp_doc_border = false,
--  },
--  requires = {
--    "rcarriga/nvim-notify"
--  },
--}

-- Highlight current line number :^)
vim.cmd[[ highlight CursorLineNr guifg=#17FFFF gui=bold ]]

-- To keep cursor shape when exiting Neovim (block | ver25 = I-Beam | hor20 = underline)
vim.cmd [[ 
  augroup change_coursor
    au!
    au ExitPre * :set guicursor=a:hor25
  augroup END
]]
