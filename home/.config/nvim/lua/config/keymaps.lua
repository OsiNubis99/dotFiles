-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

local keymap = vim.keymap -- for conciseness

-- Save files
keymap.set("n", "<D-s>", ":w<CR>", { noremap = true, silent = true })
keymap.set("i", "<D-s>", "<Esc>:w<CR>a", { noremap = true, silent = true })
keymap.set("n", "s", ":w<CR>", { noremap = true, silent = true })
keymap.set("n", "<leader>fs", ":w<CR>", { desc = "Save buffer" })
keymap.set("n", "<leader>ps", ":wa<CR>", { desc = "Save all open buffers" })

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
keymap.set("n", "<leader>.", ":Oil --float<CR>", { desc = "Open File Browser" })
keymap.set("n", "<leader><space>", LazyVim.pick("files", { root = false }), { desc = "Find Files (cwd)" })
keymap.set("n", "<leader>/", LazyVim.pick("live_grep", { root = false }), { desc = "Grep (cwd)" })
keymap.set("n", "<leader>pp", ":Telescope projections<CR>", { desc = "Find Projects" })

-- developer
keymap.set("n", "<leader>d", ":vsplit | lua vim.lsp.buf.definition()<CR>")
keymap.set({ "n", "v" }, "E", "$")
keymap.set({ "n", "v" }, "B", "^")
