require("bufferline").setup{}

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

-- require("colorizer").setup()
require("nvim-highlight-colors").setup {
  render = "background", -- background | foreground | first_column
  enable_named_colors = true,
  enable_tailwind = false,
}

-- LSP charging animation
require("fidget").setup{}

require("indent_blankline").setup{
  char = "│",
  char_list_blankline = { "|", "┊", "┆", "¦" },
  space_char_blankline = " ",
  show_first_indent_level = true,
  show_trailing_blankline_indent = false,
  filetype_exclude = {
      "",
      "NvimTree",
      "Octo",
      "TelescopePrompt",
      "Trouble",
      "dashboard",
      "git",
      "help",
      "markdown",
      "undotree",
  },
  buftype_exclude = { "terminal", "nofile" },
  show_current_context = false,
  show_current_context_start = false,
  context_patterns = {
      "class",
      "function",
      "method",
      "block",
      "list_literal",
      "selector",
      "^if",
      "^table",
      "if_statement",
      "while",
      "for",
      "type",
      "var",
      "import",
  },
}

require("lualine").setup {
  options = {
    icons_enabled = true,
    theme = "nightfly",
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

-- Colorscheme
vim.cmd [[ colorscheme catppuccin ]]

-- Highlight current line number :^)
vim.cmd[[ highlight CursorLineNr guifg=#17FFFF gui=bold ]]

-- To keep cursor shape when exiting Neovim (block | ver25 = I-Beam | hor20 = underline)
vim.cmd [[ 
  augroup change_coursor
    au!
    au ExitPre * :set guicursor=a:hor25
  augroup END
]]
