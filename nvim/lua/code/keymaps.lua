local keymap = vim.keymap.set
local opts = { noremap = true, silent = true }

-- remap leader key
keymap("n", "<Space>", "", opts)

vim.o.clipboard = "unnamedplus"

-- yank to system clipboard
keymap({"n", "v"}, "<leader>y", '"+y', opts)

-- paste from system clipboard
keymap({"n", "v"}, "<leader>p", '"+p', opts)

-- better indent handling
keymap("v", "<", "<gv", opts)
keymap("v", ">", ">gv", opts)

-- move text up and down
keymap("v", "J", ":m .+1<CR>==", opts)
keymap("v", "K", ":m .-2<CR>==", opts)
keymap("x", "J", ":move '>+1<CR>gv-gv", opts)
keymap("x", "K", ":move '<-2<CR>gv-gv", opts)

-- paste preserves primal yanked piece
keymap("v", "p", '"_dP', opts)

-- removes highlighting after escaping vim search
keymap("n", "<Esc>", "<Esc>:noh<CR>", opts)

-- call vscode commands from neovim

-- general keymaps
keymap({"n", "v"}, "<leader>a", "<cmd>lua require('vscode').action('editor.action.quickFix')<CR>")

keymap({"n", "v"}, "<leader>bb", "<cmd>lua require('vscode').action('editor.debug.action.toggleBreakpoint')<CR>")
keymap({"n", "v"}, "<leader>bq", "<cmd>lua require('vscode').action('workbench.action.closeActiveEditor')<CR>")

keymap({"n", "v"}, "<leader>cd", "<cmd>lua require('vscode').action('editor.action.revealDefinition')<CR>")
keymap({"n", "v"}, "<leader>cD", "<cmd>lua require('vscode').action('editor.action.goToReferences')<CR>")
keymap({"n", "v"}, "<leader>cn", "<cmd>lua require('vscode').action('notifications.clearAll')<CR>")
keymap({"n", "v"}, "<leader>cp", "<cmd>lua require('vscode').action('workbench.action.showCommands')<CR>")

keymap({"n", "v"}, "<leader>d", "<cmd>lua require('vscode').action('editor.action.showHover')<CR>")

keymap({"n", "v"}, "<leader>fd", "<cmd>lua require('vscode').action('editor.action.formatDocument')<CR>")
keymap({"n", "v"}, "<leader>ff", "<cmd>lua require('vscode').action('workbench.action.quickOpen')<CR>")

keymap({"n", "v"}, "<leader>fk", "<cmd>lua require('vscode').action('workbench.action.openGlobalKeybindings')<CR>")
keymap({"n", "v"}, "<leader>fs", "<cmd>lua require('vscode').action('workbench.action.files.save')<CR>")

keymap({"n", "v"}, "<leader>gb", "<cmd>lua require('vscode').action('magit.blame-file')<CR>")
keymap({"n", "v"}, "<leader>gc", "<cmd>lua require('vscode').action('gitlens.copyRemoteFileUrlToClipboard')<CR>")
keymap({"n", "v"}, "<leader>gL", "<cmd>lua require('vscode').action('magit.log-file')<CR>")
keymap({"n", "v"}, "<leader>gg", "<cmd>lua require('vscode').action('magit.status')<CR>")
keymap({"n", "v"}, "<leader>goo", "<cmd>lua require('vscode').action('gitlens.openFileOnRemote')<CR>")


keymap({"n", "v"}, "<leader>ha", "<cmd>lua require('vscode').action('vscode-harpoon.addEditor')<CR>")
keymap({"n", "v"}, "<leader>ho", "<cmd>lua require('vscode').action('vscode-harpoon.editorQuickPick')<CR>")
keymap({"n", "v"}, "<leader>he", "<cmd>lua require('vscode').action('vscode-harpoon.editEditors')<CR>")
keymap({"n", "v"}, "<leader>h1", "<cmd>lua require('vscode').action('vscode-harpoon.gotoEditor1')<CR>")
keymap({"n", "v"}, "<leader>h2", "<cmd>lua require('vscode').action('vscode-harpoon.gotoEditor2')<CR>")
keymap({"n", "v"}, "<leader>h3", "<cmd>lua require('vscode').action('vscode-harpoon.gotoEditor3')<CR>")
keymap({"n", "v"}, "<leader>h4", "<cmd>lua require('vscode').action('vscode-harpoon.gotoEditor4')<CR>")
keymap({"n", "v"}, "<leader>h5", "<cmd>lua require('vscode').action('vscode-harpoon.gotoEditor5')<CR>")
keymap({"n", "v"}, "<leader>h6", "<cmd>lua require('vscode').action('vscode-harpoon.gotoEditor6')<CR>")
keymap({"n", "v"}, "<leader>h7", "<cmd>lua require('vscode').action('vscode-harpoon.gotoEditor7')<CR>")
keymap({"n", "v"}, "<leader>h8", "<cmd>lua require('vscode').action('vscode-harpoon.gotoEditor8')<CR>")
keymap({"n", "v"}, "<leader>h9", "<cmd>lua require('vscode').action('vscode-harpoon.gotoEditor9')<CR>")

keymap({"n", "v"}, "<leader>ot", "<cmd>lua require('vscode').action('workbench.action.terminal.toggleTerminal')<CR>")
keymap({"n", "v"}, "<leader>op", "<cmd>lua require('vscode').action('workbench.view.explorer')<CR>")
keymap({"n", "v"}, "<leader>od", "<cmd>lua require('vscode').action('workbench.debug.action.focusRepl')<CR>")
keymap({"n", "v"}, "<leader>os", "<cmd>lua require('vscode').action('workbench.view.testing.focus')<CR>")

keymap({"n", "v"}, "<leader>pp", "<cmd>lua require('vscode').action('projectManager.listProjects')<CR>")
keymap({"n", "v"}, "<leader>pa", "<cmd>lua require('vscode').action('projectManager.saveProject')<CR>")
keymap({"n", "v"}, "<leader>po", "<cmd>lua require('vscode').action('projectManager.listProjectsNewWindow')<CR>")
keymap({"n", "v"}, "<leader>pe", "<cmd>lua require('vscode').action('projectManager.editProjects')<CR>")

keymap({"n", "v"}, "<leader>qp", "<cmd>lua require('vscode').action('workbench.action.closePanel')<CR>")
keymap({"n", "v"}, "<leader>qq", "<cmd>lua require('vscode').action('workbench.action.closeSidebar')<CR>")

keymap({"n", "v"}, "<leader>sp", "<cmd>lua require('vscode').action('workbench.actions.view.problems')<CR>")

keymap({"n", "v"}, "<leader>ts", "<cmd>lua require('vscode').action('testing.runAtCursor')<CR>")
keymap({"n", "v"}, "<leader>tS", "<cmd>lua require('vscode').action('testing.debugAtCursor')<CR>")
keymap({"n", "v"}, "<leader>tv", "<cmd>lua require('vscode').action('testing.runCurrentFile')<CR>")
keymap({"n", "v"}, "<leader>ta", "<cmd>lua require('vscode').action('testing.runAll')<CR>")

keymap({"n", "v"}, "<leader>wq", "<cmd>lua require('vscode').action('workbench.action.closeUnmodifiedEditors')<CR>")

keymap({"n", "v"}, "<leader>/", "<cmd>lua require('vscode').action('workbench.action.findInFiles')<CR>")

-- navigation
keymap({"n", "v"}, "<leader>vv", "<cmd>lua require('vscode').action('workbench.action.splitEditorRight')<CR>")
keymap({"n", "v"}, "<leader>ss", "<cmd>lua require('vscode').action('workbench.action.splitEditorDown')<CR>")
