-- require("impatient")

-- Autopairs setup
require("nvim-autopairs").setup({
    enable_check_bracket_line = true,
    ignored_next_char = "[%w%.]",
})

-- Comment line setup
require("nvim_comment").setup({
    comment_empty = false,
    comment_empty_trim_whitespaces = false,
})
vim.keymap.set("n", "<leader>l", ":CommentToggle<CR>")
