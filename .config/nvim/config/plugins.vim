" code for all versions of nvim goes here...
" install plug.vim if it isn't already
if empty(glob('$HOME/.config/nvim/plug.vim'))
    silent !curl -fLo $HOME/.config/nvim/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    augroup autoinstallPlugVim
        autocmd!
        autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
    augroup end
endif

" load plug.vim
source $HOME/.config/nvim/plug.vim

call plug#begin('$HOME/.config/nvim/jeyj0-plugged')

" -----------------------------------------------------------------------------
" TODO: try these plugins out - I've found them in other people's configs but
" haven't evaluated them for myself yet:

" Plug 'Valloric/MatchTagAlways'
" Plug 'ntpeters/vim-better-whitespace'
" Plug 'tpope/vim-eunuch'
" Plug 'vim-test/vim-test'
" Plug 'alvan/vim-closetag'
" Plug 'AndrewRadev/switch.vim'
" Plug 'antoinemadec/coc-fzf'

Plug 'vimwiki/vimwiki'
Plug 'michal-h21/vim-zettel'

" -----------------------------------------------------------------------------

" vim window manipulation
Plug 'yaronkh/vim-winmanip'

" nice dashboard, with useful file lists
" also includes session management
Plug 'mhinz/vim-startify'

" move inside camelCase and snake_case words and alike
Plug 'chaoren/vim-wordmotion'

" more and more flexible text objects
Plug 'wellle/targets.vim'

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
Plug 'airblade/vim-gitgutter'

" surround with stuff
Plug 'tpope/vim-surround'

" case-keeping replace (+stuff I don't use)
Plug 'tpope/vim-abolish'

" switch between various naming styles (camelCase, snake_case, ...)
Plug 'LucHermitte/lh-style'
    Plug 'LucHermitte/lh-vim-lib'

" comment text object
Plug 'https://github.com/glts/vim-textobj-comment'
    Plug 'https://github.com/kana/vim-textobj-user'

" easy html (or xml) tag creation
Plug 'mattn/emmet-vim'

" color-scheme
Plug 'crusoexia/vim-monokai'
Plug 'morhetz/gruvbox'
Plug 'altercation/vim-colors-solarized'
Plug 'cideM/yui'

" fancy status line
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" autocompletion
Plug 'neoclide/coc.nvim', { 'branch': 'release' }

" snippets (used by coc-snippets)
Plug 'honza/vim-snippets'

" better terminal integration (f.e. mouse support)
Plug 'wincent/terminus'

" use ranger
Plug 'francoiscabrol/ranger.vim'

" use nnn.vim for file picking
Plug 'mcchrish/nnn.vim'

" buffer closing
Plug 'rbgrouleff/bclose.vim' " required by ranger

" in case I forget some keybinding, this should help me
Plug 'liuchengxu/vim-which-key'

" get fzf
Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'

" hoogle integration
Plug 'monkoose/fzf-hoogle.vim'

" floating, persistant (toggleable) terminals
Plug 'voldikss/vim-floaterm'

" better indentation
Plug 'vim-scripts/yaifa.vim'

" auto-formatting
" Plug 'prettier/vim-prettier', { 'do': 'npm install' }

" easy ftFT navigation by highlighting best letters
Plug 'unblevable/quick-scope'

" nice todo-list management
Plug 'jeyj0/vim-todo-lists'

" plugins for different nvim runners
if exists('g:vscode')
    " TODO remove this
    Plug 'asvetliakov/vscode-neovim', { 'dir': './vim' }
endif

if has('nvim-0.5')
    " nice tabline at the top (incl. dependencies)
    Plug 'romgrk/barbar.nvim'
        Plug 'kyazdani42/nvim-web-devicons'

    " treesitter support! :D
    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
endif

call plug#end()

" do not map keys from some plugins
let g:bclose_no_plugin_maps = 1
let g:ranger_map_keys = 0
let g:gitgutter_map_keys = 0
let g:winmanip_disable_key_mapping = 1
let g:zettel_default_mappings = 0
let g:nnn#set_default_mappings = 0
