" fix stuff
set nocompatible

" enable fuzzy-like finding
set path+=**

" enable line numbers
:set number relativenumber

:augroup numbertoggle
:  autocmd!
:  autocmd BufEnter,FocusGained,InsertLeave * set relativenumber
:  autocmd BufLeave,FocusLost,InsertEnter   * set norelativenumber
:augroup END

" easily switch to normal mode for terminal buffer
tnoremap <ESC><ESC> <C-\><C-N>

" remap window switching to Alt+[h,j,k,l]
execute "set <M-h>=\eh"
execute "set <M-j>=\ej"
execute "set <M-k>=\ek"
execute "set <M-l>=\el"
nnoremap <M-h> :wincmd h<CR>
nnoremap <M-j> :wincmd j<CR>
nnoremap <M-k> :wincmd k<CR>
nnoremap <M-l> :wincmd l<CR>
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

execute "set <M-f>=\ef"
nnoremap <silent> <M-f> :NERDTree<CR>:set norelativenumber<CR>
