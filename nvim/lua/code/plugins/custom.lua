return {
	{ 'tpope/vim-repeat', },
	{ 'tpope/vim-surround', },
	{
		"NeogitOrg/neogit",
		dependencies = {
			"nvim-lua/plenary.nvim",         -- required
			"sindrets/diffview.nvim",        -- optional - Diff integration
			"ibhagwan/fzf-lua",              -- optional
		},
		config = function()
			require("neogit").setup({
				disable_commit_confirmation = true, -- Disable confirmation prompts for commits
				kind = "split",                     -- Use a split instead of floating windows
				integrations = {
					diffview = true,                -- Enable diffview integration if installed
				}
			})
		end
	}
}
