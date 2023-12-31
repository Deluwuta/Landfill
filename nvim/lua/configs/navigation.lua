-- Harpoon
local mark = require("harpoon.mark")
local ui = require("harpoon.ui")

vim.keymap.set("n", "<leader>a", mark.add_file)
vim.keymap.set("n", "<leader>he", ui.toggle_quick_menu)

vim.keymap.set("n", "<leader>hf", function() ui.nav_file(1) end)
vim.keymap.set("n", "<leader>hs", function() ui.nav_file(2) end)
vim.keymap.set("n", "<leader>h3", function() ui.nav_file(3) end)
vim.keymap.set("n", "<leader>h4", function() ui.nav_file(4) end)

-- Telescope :D
require("telescope").setup {
    defaults = {
        vimgrep_arguments = {
            "rg",
            "--color=never",
            "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
            "--smart-case",
            "--trim" -- add this value
        },
        path_display = "absolute"
    },

    pickers = {
        find_files = {
            theme = "ivy",
        },
    },

    extensions = {
        file_browser = {
            theme = "ivy",
            hidden = true,
            prompt_path = true,
            dir_icon = "",
            dir_icon_hl = "Default",
        },
    },
}
require("telescope").load_extension("file_browser")

-- Telescope keys
-- Finding files
vim.keymap.set("n", "<leader>ff", require("telescope.builtin").find_files, {})
vim.keymap.set("n", "<leader>fr", require("telescope.builtin").oldfiles, {})
vim.keymap.set("n", "<leader>." , ":Telescope file_browser hidden=true<CR>")
vim.keymap.set("n", "<leader>fw", require("telescope.builtin").live_grep, {})

-- Buffers
vim.keymap.set("n", "<leader>bi", require("telescope.builtin").buffers)
vim.keymap.set("n", "<leader>bk", ":bd <CR>") -- *Kill current buffer
vim.keymap.set("n", "<leader>bn", ":bnext <CR>")
vim.keymap.set("n", "<leader>bp", ":bprev <CR>")

-- Choose colorscheme
vim.keymap.set("n", "<leader>ht", ":Telescope colorscheme<CR>")
