-- AstroCommunity: import any community modules here
-- We import this file in `lazy_setup.lua` before the `plugins/` folder.
-- This guarantees that the specs are processed before any user plugins.

---@type LazySpec
return {
	"AstroNvim/astrocommunity",
	{ import = "astrocommunity.pack.lua" },
	{ import = "astrocommunity.pack.rust" },
	{ import = "astrocommunity.pack.bash" },
	{ import = "astrocommunity.pack.ansible" },
	{ import = "astrocommunity.pack.docker" },
	{ import = "astrocommunity.utility.telescope-lazy-nvim" },
	{ import = "astrocommunity.pack.chezmoi" },
	{ import = "astrocommunity.pack.python" },
	{ import = "astrocommunity.completion.cmp-tmux" },
	{ import = "astrocommunity.completion.copilot-lua-cmp" },
	{ import = "astrocommunity.terminal-integration.vim-tmux-navigator" },
	{ import = "astrocommunity.terminal-integration.vim-tpipeline" },
	{ import = "astrocommunity.color.headlines-nvim" },
	{ import = "astrocommunity.editing-support.copilotchat-nvim" },
	{ import = "astrocommunity.editing-support.cloak-nvim" },
	{ import = "astrocommunity.note-taking.neorg" },
	{ import = "astrocommunity.recipes.neovide" },
	{ import = "astrocommunity.scrolling.neoscroll-nvim" },
	{ import = "astrocommunity.motion.nvim-surround" },
	{ import = "astrocommunity.split-and-window.minimap-vim" },
	-- import/override with your plugins folder
}
