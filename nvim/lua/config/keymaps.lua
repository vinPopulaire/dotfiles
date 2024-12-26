local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }

-- remap leader key
keymap("n", "<Space>", "", opts)

vim.o.clipboard = "unnamedplus"
vim.o.number = true

-- removes highlighting after escaping vim search
keymap("n", "<Esc>", "<Esc>:noh<CR>", opts)

keymap({"n", "v"}, "<leader>op", ":Neotree toggle<CR>")

keymap({"n", "v"}, "<leader>gg", ":Neogit kind=replace<CR>")

keymap({"n", "v"}, "<leader>vv", ":vsplit<CR>")
keymap({"n", "v"}, "<leader>ss", ":split<CR>")

keymap({"n", "v"}, "<leader>fs", ":w<CR>")

keymap("n", "<leader>ff", require('fzf-lua').files, { desc = "Fzf Files" })

keymap("n", "j", "gj")
keymap("n", "k", "gk")

keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)
