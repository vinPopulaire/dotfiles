require("config.lazy")

if vim.g.vscode then
  -- VSCode Neovim
  require("code.keymaps")

  require('neogit').setup({
	  disable_commit_confirmation = true, -- Optional, disable commit confirmation dialog
	  kind = "split", -- Use split instead of floating windows
	  disable_hint = true, -- Disable hint messages to reduce clutter
	  integrations = {
		  diffview = false, -- Disable if using external tools like Diffview to isolate issues
	  },
  })
else
  require("config.keymaps")
end

