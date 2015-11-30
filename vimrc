set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'jpo/vim-railscasts-theme'
Plugin 'sjl/gundo.vim'
Plugin 'scrooloose/nerdTree'
Plugin 'rking/ag.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'mattn/emmet-vim'
Plugin 'bling/vim-airline'
Plugin 'jiangmiao/auto-pairs'
Plugin 'tpope/vim-commentary'
Plugin 'Valloric/YouCompleteMe'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-rails'
Plugin 'tpope/vim-ragtag'
Plugin 'tpope/vim-endwise'
Plugin 'vim-ruby/vim-ruby'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

set t_Co=256    " set the terminal to 256 color
colorscheme railscasts    " awesome colorscheme

set lines=50 columns=100
set guifont=Menlo:h15 

syntax on	  " enable syntax processing

set number    " show line numbers
set showcmd   " show command in bottom bar

set tabstop=4   " number of visual spaces per TAB
set softtabstop=4   " number of spaces in tab when editing
set shiftwidth=4
set expandtab   " tabs are spaces
set autoindent

filetype indent on  " load filetype-specific indent files
if has("autocmd")
    filetype on
    autocmd BufWritePre *.php,*.py,*.js,*.txt,*.hs,*.java,*.md,*.rb :call <SID>StripTrailingWhitespaces()
    autocmd FileType make setlocal ts=8 sts=8 sw=8 noexpandtab
    autocmd FileType yaml setlocal ts=2 sts=2 sw=2 expandtab
    
    autocmd FileType html setlocal ts=2 sts=2 sw=2 expandtab
    autocmd FileType ruby setlocal ts=2 sts=2 sw=2 expandtab
    autocmd FileType css setlocal ts=2 sts=2 sw=2 expandtab
    autocmd FileType javascript setlocal ts=4 sts=4 sw=4 noexpandtab
    autocmd FileType eruby setlocal ts=2 sts=2 sw=2 expandtab
 
    autocmd BufNewFile,BufRead *.rss setfiletype xml
endif

runtime macros/matchit.vim

set cursorline  " highlight current line
set colorcolumn=80,120 " highlight columns 80 and 120

set ignorecase
set smartcase
set wildmenu    " visual autocomplete for command menu
set lazyredraw  " redraw only when we need to
set showmatch   " highlight matching [{()}]

set incsearch   " search as characters are entered
set hlsearch    " highlight matches

set foldenable  " enable folding
set foldlevelstart=10   " open most folds by default

set foldnestmax=10  " 10 nested fold max
set foldmethod=indent   "fold based on indent level

let mapleader = ' '
map <leader>q :nohl<CR>

nnoremap <leader>z za

" move vertically by visual line
nnoremap j gj
nnoremap k gk

" move to beginning/end of line
nnoremap B ^
nnoremap E $

" $,^ doesn't do anything
nnoremap $ <nop>
nnoremap ^ <nop>

" highlight last inserted text
nnoremap gV `[v`]

" jk is escape
inoremap jk <esc>

" save with Ctrl-s
inoremap <c-s> <esc>:update<cr>
vnoremap <c-s> <esc>:update<cr>
noremap <c-s> :update<cr>

" toggle gundo
nnoremap <leader>u :GundoToggle<CR>

" save session
nnoremap <leader>s :mksession<CR>

" search word under cursor
nnoremap <leader>n *

" toggle NERDTree
map <C-n> :NERDTreeToggle<CR>
let NERDTreeIgnore = ['\.pyc$', 'build', 'venv', 'egg', 'egg-info/', 'dist', 'docs']

" open ag.vim
nnoremap <leader>a :Ag

" emmet configurations
let g:user_emmet_leader_key=','

" ragTag configurations
imap <leader>, <C-X>=
imap <leader>. <C-X>-

" Mappings to access buffers
set hidden

nnoremap <leader>T :enew<cr>
nnoremap <leader>l :bnext<CR>
nnoremap <leader>h :bprevious<CR>
nnoremap <leader>bl :ls<CR>
nnoremap <leader>bq :bp <BAR> bd #<CR>

" Ctrlp  settings
let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
let g:ctrlp_custom_ignore = '\vbuild/|dist/|venv/|target/|\.(o|swp|pyc|egg)$'

" airline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'

" ragtag
let g:ragtag_global_maps = 1

" strips trailing whitespace at the end of files. this
" is called on buffer write in the autogroup above.
function! <SID>StripTrailingWhitespaces()
    " save last search & cursor position
    let _s=@/
    let l = line(".")
    let c = col(".")
    %s/\s\+$//e
    let @/=_s
    call cursor(l, c)
endfunction
