" install plug.vim if it isn't already
if empty(glob('$HOME/.config/nvim/plug.vim'))
  silent !curl -fLo $HOME/.config/nvim/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" load plug.vim
source $HOME/.config/nvim/plug.vim

call plug#begin('$HOME/.config/nvim/jeyj0-plugged')

" language support for basically everything
Plug 'sheerun/vim-polyglot'

" color-scheme
Plug 'crusoexia/vim-monokai'

" fancy status line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'bling/vim-bufferline'
Plug 'tpope/vim-fugitive'

" autocompletion
Plug 'neoclide/coc.nvim', { 'branch': 'release', 'do': 'npm ci' }

" better terminal integration (f.e. mouse support)
Plug 'wincent/terminus'

" use ranger
Plug 'francoiscabrol/ranger.vim' " this sets <leader>f to open ranger
Plug 'rbgrouleff/bclose.vim' " required by ranger

" get fzf
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

" better indentation
Plug 'vim-scripts/yaifa.vim'


call plug#end()

set noshowmode
let g:airline_powerline_fonts = 1
let g:airline_theme='minimalist'
let g:bufferline_echo = 0
let g:airline_mode_map = {}
let g:airline_mode_map['ic'] = 'INSERT'

let mapleader=","

" do not map <leader>bd to close buffer (bclose.vim)
let g:bclose_no_plugin_maps = 1

" use ranger when opening a directory
let g:ranger_map_keys = 0
let g:ranger_replace_netrw = 1
nnoremap <leader>r :RangerWorkingDirectory<cr>

" set for-me-intuitive split opening
set splitright
set splitbelow

" smart search
set ignorecase
set smartcase

" share clipboard with system
set clipboard=unnamedplus

" visual settings
set number relativenumber
colorscheme monokai

" configure neovim-remote to work properly with git
autocmd FileType gitcommit,gitrebase,gitconfig set bufhidden=delete

" navigation
nnoremap <silent> <A-h> :wincmd h<cr>
nnoremap <silent> <A-j> :wincmd j<cr>
nnoremap <silent> <A-k> :wincmd k<cr>
nnoremap <silent> <A-l> :wincmd l<cr>
nnoremap <silent> <A-U> :bprevious<cr>
nnoremap <silent> <A-u> :tabprevious<cr>
nnoremap <silent> <A-I> :bnext<cr>
nnoremap <silent> <A-i> :tabnext<cr>
tnoremap <silent> <A-h> <C-\><C-n>:wincmd h<cr>
tnoremap <silent> <A-j> <C-\><C-n>:wincmd j<cr>
tnoremap <silent> <A-k> <C-\><C-n>:wincmd k<cr>
tnoremap <silent> <A-l> <C-\><C-n>:wincmd l<cr>
tnoremap <silent> <A-U> <C-\><C-n>:bprevious<cr>
tnoremap <silent> <A-u> <C-\><C-n>:tabprevious<cr>
tnoremap <silent> <A-I> <C-\><C-n>:bnext<cr>
tnoremap <silent> <A-i> <C-\><C-n>:tabnext<cr>

" setup terminal
autocmd TermOpen * setlocal nonumber norelativenumber " no line numbers in terminal buffer
autocmd TermOpen * startinsert " start terminal in terminal-mode

"" exit terminal mode with Alt-n
tnoremap <A-n> <C-\><C-N>

"" create default terminal with <leader>-t
nnoremap <leader>t :botright 20split +terminal<cr>
""" create different terminals at different locations
nnoremap <leader>T :terminal<cr>
nnoremap <leader><leader>rt :botright vsplit +terminal<cr>
nnoremap <leader><leader>lt :topleft vsplit +terminal<cr>
nnoremap <leader><leader>tt :topleft split +terminal<cr>
nnoremap <leader><leader>bt :botright split +terminal<cr>

" coc.nvim config
set updatetime=300

"" use tab to navigate completion list and Enter to select
"" (only relevant for snippets)
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

"" use c-space to trigger completion
inoremap <silent><expr> <c-space> coc#refresh()

"" create function text object (if supported by languageserver)
xnoremap if <Plug>(coc-funcobj-i)
xnoremap af <Plug>(coc-funcobj-a)

"" highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" clear search highlighting on <space>
nnoremap <silent> <space> :noh<cr>

" open fuzzy-file-finder
"" git-aware
nnoremap <leader>f :GFiles<cr>
"" git-unaware
nnoremap <leader>F :Files<cr>
"" fuzzy-find with ag
nnoremap <leader>g :Ag<cr>
"" buffers
nnoremap <leader>b :Buffers<cr>

" closing buffers
nnoremap <silent> <leader>q :Bclose<cr>

" easily split lines in normal mode
" taken from drzel/vim-split-line
nnoremap <CR> :keeppatterns substitute/\s*\%#\s*/\r/e <bar> normal! ==<CR>
