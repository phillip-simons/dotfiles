return {
  "nvim-neorg/neorg",
  lazy = false, -- Disable lazy loading as some `lazy.nvim` distributions set `lazy = true` by default
  version = "*", -- Pin Neorg to the latest stable release
  opts = {
    load = {
      ["core.defaults"] = {}, -- Loads default behaviour
      ["core.concealer"] = {}, -- Adds pretty icons to your documents
      ["core.keybinds"] = {}, -- Adds default keybindings
      ["core.completion"] = {
        config = {
          engine = "nvim-cmp",
        },
      },                  -- Enables support for completion plugins
      ["core.journal"] = {}, -- Enables support for the journal module
      ["core.dirman"] = { -- Manages Neorg workspaces
        config = {
          workspaces = {
            notes = "~/projects/notes",
          },
        },
      },
    },
  },
}
