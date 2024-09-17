return {
  "https://git.sr.ht/~swaits/scratch.nvim",
  lazy = true,
  keys = {
    { "<leader>bx", "<cmd>Scratch<cr>", desc = "Scratch Buffer", mode = "n" },
    { "<leader>bS", "<cmd>ScratchSplit<cr>", desc = "Scratch Buffer (split)", mode = "n" },
  },
  cmd = {
    "Scratch",
    "ScratchSplit",
  },
  opts = {},
}
