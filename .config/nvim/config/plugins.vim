" code for all versions of nvim goes here...
" install plug.vim if it isn't already
if empty(glob('$HOME/.config/nvim/plug.vim'))
silent !curl -fLo $HOME/.config/nvim/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" load plug.vim
source $HOME/.config/nvim/plug.vim

call plug#begin('$HOME/.config/nvim/jeyj0-plugged')

" plugins for different nvim runners
if exists('g:vscode')
    " TODO remove this
    Plug 'asvetliakov/vscode-neovim', { 'dir': './vim' }
endif

" language support for basically everything
Plug 'sheerun/vim-polyglot'

" auto-close brackets,...
Plug 'cohama/lexima.vim'

" awesome git
Plug 'tpope/vim-fugitive'

" color-scheme
Plug 'crusoexia/vim-monokai'

" fancy status line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" autocompletion
Plug 'neoclide/coc.nvim', { 'branch': 'release', 'do': 'npm ci' }

" better terminal integration (f.e. mouse support)
Plug 'wincent/terminus'

" easy git staging
Plug 'airblade/vim-gitgutter'

" use ranger
Plug 'francoiscabrol/ranger.vim' " this sets <leader>f to open ranger
Plug 'rbgrouleff/bclose.vim' " required by ranger

" get fzf
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

" better indentation
Plug 'vim-scripts/yaifa.vim'

" auto-formatting
Plug 'prettier/vim-prettier', { 'do': 'npm install' }

" nice todo-list management
Plug 'jeyj0/vim-todo-lists'

call plug#end()
