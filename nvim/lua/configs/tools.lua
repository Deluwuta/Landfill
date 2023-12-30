require("impatient") -- GIMME THA SPEED

-- Autopairs
require("nvim-autopairs").setup({
    enable_check_bracket_line = true,
    ignored_next_char = "[%w%.]",
})

-- Autocomment
require("nvim_comment").setup({
    comment_empty = false,
    comment_empty_trim_whitespaces = false,
})
