--require("dired").setup{
--  path_separator = "/",
--  show_banner = false,
--  show_hidden = true,
--  show_dot_dirs = true,
--  show_colors = true,
--}

require("impatient")

-- require("nvim-tree").setup{}
require("neo-tree").setup{
  window = { width = 28 },
  filesystem = { follow_current_file = true },
}

require("telescope").setup {
  defaults = {
    initial_mode = "normal",
  },
  extensions = {
    file_browser = {
      theme = "ivy",
      hidden = true,
      hijack_netrw = true,
    },
  },
}
require("telescope").load_extension("file_browser")

require("toggleterm").setup{
  auto_scroll = true,
  close_on_exit = true,
  start_in_insert = true,
  direction = "float",
  float_opts = {
    border = "curved",
    winblend = 3,
  },
  size = function (term)
    if term.direction == "horizontal" then
      return 15
    elseif term.direction == "vertical" then
      return vim.o.columns * 0.4
    end
  end,
}
