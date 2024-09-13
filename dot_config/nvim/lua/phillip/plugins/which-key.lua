 return {
  "folke/which-key.nvim",
  event = "VeryLazy",
  init = function()
    vim.o.timeout = true
    vim.o.timeoutlen = 500
  end,
  opts_extend = { "spec", "disable.ft", "disable.bt" },
  opts = {
    icons = {
      group = vim.g.icons_enabled ~= false and "" or "+",
      rules = false,
      separator = "-",
    },
  },
}
