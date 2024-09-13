vim.g.mapleader = " "

local keymap = vim.keymap -- for conciseness

keymap.set("i", "jk", "<ESC>", { desc = "Exit insert mode with jk" })
keymap.set("i", "fd", "<ESC>", { desc = "Exit insert mode with fd" })

keymap.set("n", "<leader>nh", ":nohl<CR>", { desc = "Clear search highlights" })
keymap.set("n", "<leader><Enter>", "o<Esc>", { desc = "Insert line" })

-- window management
keymap.set("n", "<leader>sv", "<C-w>v", { desc = "Split window vertically" }) -- split window vertically
keymap.set("n", "<leader>sh", "<C-w>s", { desc = "Split window horizontally" }) -- split window horizontally
keymap.set("n", "<leader>se", "<C-w>=", { desc = "Make splits equal size" }) -- make split windows equal width & height
keymap.set("n", "<leader>sx", "<cmd>close<CR>", { desc = "Close current split" }) -- close current split window

-- tab management
keymap.set("n", "<leader>to", "<cmd>tabnew<CR>", { desc = "Open new tab" }) -- open new tab
keymap.set("n", "<leader>tx", "<cmd>tabclose<CR>", { desc = "Close current tab" }) -- close current tab
keymap.set("n", "<leader>tn", "<cmd>tabn<CR>", { desc = "Go to next tab" }) --  go to next tab
keymap.set("n", "<leader>tp", "<cmd>tabp<CR>", { desc = "Go to previous tab" }) --  go to previous tab
keymap.set("n", "<leader>tf", "<cmd>tabnew %<CR>", { desc = "Open current buffer in new tab" }) --  move current buffer to new tab

-- buffer management
keymap.set("n", "<leader>bn", "<cmd>bn<cr>", { desc = "Next Buffer" })
keymap.set("n", "<leader>bp", "<cmd>bp<cr>", { desc = "Previous Buffer" })
keymap.set("n", "<leader>bd", function()
	vim.cmd("bnext")
	vim.cmd("bdelete #")
end, { desc = "Switch to next buffer and delete the previous one" })

-- prefix labels
keymap.set("n", "<leader>f", "", { desc = "Find" })
keymap.set("n", "<leader>g", "", { desc = "Git" })
keymap.set("n", "<leader>e", "", { desc = "File Tree" })
keymap.set("n", "<leader>t", "", { desc = "Tabs" })
keymap.set("n", "<leader>x", "", { desc = "Diagnostics" })
keymap.set("n", "<leader>s", "", { desc = "Splits" })
keymap.set("n", "<leader>g", "", { desc = "LSP" })
keymap.set("n", "<leader>b", "", { desc = "Buffers" })
