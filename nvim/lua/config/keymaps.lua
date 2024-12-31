local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }

local fzf = require('fzf-lua')

-- remap leader key
keymap("n", "<Space>", "", opts)

-- removes highlighting after escaping vim search
keymap("n", "<Esc>", "<Esc>:noh<CR>", opts)

keymap({"n", "v"}, "<leader>op", ":Neotree toggle<CR>", opts)

keymap({"n", "v"}, "<leader>gg", ":Neogit<CR>", opts)

keymap({"n", "v"}, "<leader>vv", function()
  vim.cmd('vsplit')
  vim.cmd('wincmd l')
  fzf.git_files()
end, opts)
keymap({"n", "v"}, "<leader>ss", function()
  vim.cmd('split')
  vim.cmd('wincmd j')
  fzf.git_files()
end, opts)

keymap({"n", "v"}, "<leader>fs", ":w<CR>")

keymap("n", "<leader>ff", fzf.files, { noremap = true, silent = true, desc = "fzf browse open files" })
keymap("n", "<leader><leader>", fzf.git_files, {desc = "fzf browse open files"})
keymap("n", "<leader>bb", fzf.buffers, {desc = "fzf browse open buffers"})

keymap("n", "<leader>/", function()
  fzf.live_grep({
    search = "",
    cwd = vim.fn.systemlist('git rev-parse --show-toplevel')[1],
    git_files = true,
    rg_opts = '--ignore-case --regexp',
  })
end, { noremap = true, silent = true, desc = "Live search for word in project" })

keymap("n", "<leader>T", function()
  fzf.live_grep_glob({
    search = "",
    cwd = vim.fn.systemlist('git rev-parse --show-toplevel')[1],
    git_files = true,
  })
end)

keymap("n", "j", "gj")
keymap("n", "k", "gk")

keymap({"n", "v"}, "B", "^")
keymap({"n", "v"}, "E", "$")

keymap("n", "<C-h>", "<C-w>h", opts)
keymap("n", "<C-j>", "<C-w>j", opts)
keymap("n", "<C-k>", "<C-w>k", opts)
keymap("n", "<C-l>", "<C-w>l", opts)

keymap("n", "<leader>bq", function()
  vim.cmd('bp')
  vim.cmd('bd #')
end, opts)

keymap("n", "<leader>wq", ":close<CR>", opts)
