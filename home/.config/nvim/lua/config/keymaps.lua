-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local keymap = vim.keymap -- for conciseness

-- Save files
keymap.set("n", "<C-s>", ":wa<CR>", { noremap = true })
keymap.set("i", "<C-s>", "<Esc>:wa<CR>a", { noremap = true })
keymap.set("n", "<leader>bs", ":w<CR>", { desc = "Save file" })
keymap.set("n", "<leader>fs", ":w<CR>", { desc = "Save buffer" })
keymap.set("n", "<leader>ps", ":wa<CR>", { desc = "Save all project files" })

-- window management
keymap.set("n", "<leader>ww", "<C-w>w", { desc = "Switch Windows" })
keymap.set("n", "<leader>wv", "<C-w>v", { desc = "Split Vertically" })
keymap.set("n", "<leader>wd", "<C-w>q", { desc = "Close Window" })
keymap.set("n", "<leader>wq", "<C-w>q", { desc = "Close Window" })
keymap.set("n", "<leader>wo", "<C-w>o", { desc = "Close Others" })
keymap.set("n", "<leader>wh", "<C-w>h", { desc = "Switch windows left" })
keymap.set("n", "<leader>wj", "<C-w>j", { desc = "Switch windows down" })
keymap.set("n", "<leader>wk", "<C-w>k", { desc = "Switch windows up" })
keymap.set("n", "<leader>wl", "<C-w>l", { desc = "Switch windows right" })

-- File browser
keymap.set("n", "<leader>.", ":Oil --float<CR>")
keymap.set("n", "<leader>pp", ":Telescope projections<CR>", { desc = "Find Projects" })

-- developer
keymap.set("n", "<leader>d", ":vsplit | lua vim.lsp.buf.definition()<CR>")
keymap.set("n", "<leader>s", ":belowright split | lua vim.lsp.buf.definition()<CR>")
