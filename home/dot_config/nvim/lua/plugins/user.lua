-- You can also add or configure plugins by creating files in this `plugins/` folder
-- Here are some examples:

---@type LazySpec
return {
  "andweeb/presence.nvim",
  {
    "NTBBloodbath/doom-one.nvim",
    config = function() vim.cmd "colorscheme doom-one" end,
  },
}
