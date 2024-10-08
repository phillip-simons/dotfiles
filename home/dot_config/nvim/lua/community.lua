-- AstroCommunity: import any community modules here
-- We import this file in `lazy_setup.lua` before the `plugins/` folder.
-- This guarantees that the specs are processed before any user plugins.

---@type LazySpec
return {
  "AstroNvim/astrocommunity",
  { import = "astrocommunity.color.headlines-nvim" },
  { import = "astrocommunity.completion.cmp-tmux" },
  { import = "astrocommunity.completion.copilot-lua-cmp" },
  { import = "astrocommunity.editing-support.copilotchat-nvim" },
  { import = "astrocommunity.editing-support.cloak-nvim" },
  { import = "astrocommunity.editing-support.neogen" },
  { import = "astrocommunity.editing-support.suda-vim" },
  { import = "astrocommunity.fuzzy-finder.telescope-zoxide" },
  { import = "astrocommunity.icon.mini-icons" },
  { import = "astrocommunity.motion.nvim-surround" },
  { import = "astrocommunity.note-taking.neorg" },
  { import = "astrocommunity.pack.ansible" },
  { import = "astrocommunity.pack.bash" },
  { import = "astrocommunity.pack.chezmoi" },
  { import = "astrocommunity.pack.docker" },
  { import = "astrocommunity.pack.hyprlang" },
  { import = "astrocommunity.pack.lua" },
  { import = "astrocommunity.pack.python" },
  { import = "astrocommunity.pack.rust" },
  { import = "astrocommunity.recipes.neovide" },
  { import = "astrocommunity.recipes.vscode" },
  { import = "astrocommunity.scrolling.neoscroll-nvim" },
  { import = "astrocommunity.terminal-integration.vim-tmux-navigator" },
  { import = "astrocommunity.terminal-integration.vim-tpipeline" },
  { import = "astrocommunity.utility.telescope-lazy-nvim" },
  -- import/override with your plugins folder
}
