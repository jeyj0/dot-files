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

" remap window switching to Ctrl+[h,j,k,l]
nnoremap <M-h> :wincmd h<CR>
nnoremap <M-j> :wincmd j<CR>
nnoremap <M-k> :wincmd k<CR>
nnoremap <M-l> :wincmd l<CR>
tnoremap <M-h> <C-w>h<CR>
tnoremap <M-j> <C-w>j<CR>
tnoremap <M-k> <C-w>k<CR>
tnoremap <M-l> <C-w>l<CR>
