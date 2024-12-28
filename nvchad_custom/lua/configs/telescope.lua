dofile(vim.g.base46_cache .. "telescope")

local M = {}

M.opts = {
    defaults = {
        -- path_display = "absolute",
        vimgrep_arguments = {
            "rg",
            "-L",
            "--color=never",
            -- "--no-heading",
            "--with-filename",
            "--line-number",
            "--column",
            "--smart-case",
            -- "--trim",
        },
        selection_caret = " ",
        entry_prefix = " ",
        sorting_strategy = "ascending",
        layout_config = {
            horizontal = {
                prompt_position = "bottom",
                preview_width = 0.55,
            },
            width = 0.87,
            height = 0.80,
        },

        mappings = {
            -- n = { ["q"] = require("telescope.actions").close },
        },
    },

    pickers = {
        find_files = {
            -- theme = "ivy",
            initial_mode = "insert",
            hidden = true,
        },
        buffers = {
            initial_mode = "normal",
        },
    },

    extensions = {
        file_browser = {
            -- Open the filebrowser in the current buffer path
            path = vim.fn.expand('%:p'),
            select_buffer = true,

            hidden = true,
            grouped = true,
            previewer = true,

            theme = "ivy",
            prompt_path = true,
            initial_mode = "insert",

            dir_icon = "",
            dir_icon_hl = "Default",

            layout_config = {
                horizontal = { prompt_position = "top" },
                height = 0.40,
            },
        },
    },
    extensions_list = { "themes", "terms" },
}

return M.opts
