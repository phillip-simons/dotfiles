return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { -- add a new dependency to telescope that is our new plugin
      "xvzc/chezmoi.nvim",
    },
    -- the first parameter is the plugin specification
    -- the second is the table of options as set up in Lazy with the `opts` key
    config = function(plugin, opts)
      -- run the core AstroNvim configuration function with the options table
      require "astronvim.plugins.configs.telescope"(plugin, opts)

      -- require telescope and load extensions as necessary
      require("telescope").load_extension "chezmoi"
      vim.keymap.set(
        "n",
        "<leader>f.",
        require("telescope").extensions.chezmoi.find_files,
        { desc = "Find Chezmoi Files" }
      )
    end,
  },
}
