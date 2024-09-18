return {
  {
    "AstroNvim/astrocore",
    ---@type AstroCoreOpts
    opts = {
      mappings = {
        -- first key is the mode
        n = {
          -- second key is the lefthand side of the map
          -- mappings seen under group name "Buffer"
        },
        v = {
          -- Move selection down
          ["J"] = { ":m '>+1<CR>gv=gv", desc = "Move selection down" },
          -- Move selection up
          ["K"] = { ":m '<-2<CR>gv=gv", desc = "Move selection up" },
          -- Keep selection when indenting
          ["<"] = { "<gv", desc = "Un-indent" },
          [">"] = { ">gv", desc = "Indent" },
        },
        -- tables with just a `desc` key will be registered with which-key if it's installed
        -- this is useful for naming menus
        -- quick save
        -- ["<C-s>"] = { ":w!<cr>", desc = "Save File" },  -- change description but the same command
        t = {
          -- setting a mapping to false will disable it
          -- ["<esc>"] = false,
        },
      },
    },
  },
}
