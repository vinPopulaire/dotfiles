vim.opt.clipboard = "unnamedplus"
vim.opt.number = true

vim.opt.background = "dark"
vim.cmd([[colorscheme gruvbox]])

vim.opt.shiftwidth = 4
vim.wo.wrap = false

vim.opt.cursorline = true
vim.opt.colorcolumn = "120"                                                                           

vim.api.nvim_create_autocmd('TextYankPost', {
  desc = 'Highlight when yanking text',
  group = vim.api.nvim_create_augroup('kickstart-highlight-yank', { clear = true }),
  callback = function()
    vim.highlight.on_yank()
  end,
})
