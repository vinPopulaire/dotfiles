require("config.lazy")

if vim.g.vscode then
  require("code.keymaps")
else
  require("config.keymaps")
  require("config.setup")
end

