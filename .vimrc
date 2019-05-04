" fix stuff
set nocompatible

" enable fuzzy-like finding
set path+=**

" do not save options when saving session
set ssop-=options

" enable line numbers
:set number relativenumber

" easily switch to normal mode for terminal buffer
tnoremap <ESC><ESC> <C-\><C-N>

" remap window switching to Alt+[h,j,k,l]
execute "set <M-h>=\eh"
execute "set <M-j>=\ej"
execute "set <M-k>=\ek"
execute "set <M-l>=\el"
nnoremap <silent> <M-h> :wincmd h<CR>
nnoremap <silent> <M-j> :wincmd j<CR>
nnoremap <silent> <M-k> :wincmd k<CR>
nnoremap <silent> <M-l> :wincmd l<CR>
tnoremap <M-h> <C-w>h
tnoremap <M-j> <C-w>j
tnoremap <M-k> <C-w>k
tnoremap <M-l> <C-w>l

" setup pathogen
runtime bundle/vim-pathogen/autoload/pathogen.vim
execute pathogen#infect()
syntax on
filetype plugin indent on

" select color-theme
colorscheme monokai

let NERDTreeShowLineNumbers=0
execute "set <M-f>=\ef"
nnoremap <silent> <M-f> :NERDTreeToggle<CR>
