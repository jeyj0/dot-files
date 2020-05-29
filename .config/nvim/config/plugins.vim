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

" language support for basically everything
Plug 'sheerun/vim-polyglot'

" color highlighting
Plug 'ap/vim-css-color'

" direnv loading
Plug 'direnv/direnv.vim'

" auto-close brackets,...
Plug 'cohama/lexima.vim'

" awesome git
Plug 'tpope/vim-fugitive'

" surround with stuff
Plug 'tpope/vim-surround'

" narrow to region
Plug 'chrisbra/NrrwRgn'

" color-scheme
Plug 'crusoexia/vim-monokai'
Plug 'morhetz/gruvbox'
Plug 'altercation/vim-colors-solarized'

" fancy status line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" autocompletion
Plug 'neoclide/coc.nvim', { 'branch': 'release' }

" better terminal integration (f.e. mouse support)
Plug 'wincent/terminus'

" easy git staging
Plug 'airblade/vim-gitgutter'

" use ranger
Plug 'francoiscabrol/ranger.vim' " this sets <leader>f to open ranger

" buffer closing
Plug 'rbgrouleff/bclose.vim' " required by ranger

" in case I forget some keybinding, this should help me
Plug 'liuchengxu/vim-which-key'

" get fzf
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

" better indentation
Plug 'vim-scripts/yaifa.vim'

" auto-formatting
" Plug 'prettier/vim-prettier', { 'do': 'npm install' }

" nice todo-list management
Plug 'jeyj0/vim-todo-lists'

" plugins for different nvim runners
if exists('g:vscode')
    " TODO remove this
    Plug 'asvetliakov/vscode-neovim', { 'dir': './vim' }
endif

" This is just a test of the narrow-to-region plugin

call plug#end()

" do not map keys from some plugins
let g:bclose_no_plugin_maps = 1
let g:ranger_map_keys = 0
let g:gitgutter_map_keys = 0
