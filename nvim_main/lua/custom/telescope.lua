dofile(vim.g.base46_cache .. "telescope")

return {
    defaults = {
        prompt_prefix = "   ",
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
            n = { ["q"] = require("telescope.actions").close },
        },
    },

    extensions_list = { "themes", "terms" },
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
}
