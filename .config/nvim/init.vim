" install plug.vim if it isn't already
if empty(glob('$HOME/.config/nvim/plug.vim'))
  silent !curl -fLo $HOME/.config/nvim/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" load plug.vim
source $HOME/.config/nvim/plug.vim

call plug#begin('$HOME/.config/nvim/jeyj0-plugged')

" color-scheme
Plug 'crusoexia/vim-monokai'

" autocompletion
Plug 'neoclide/coc.nvim', { 'branch': 'release', 'do': 'npm ci' }

" better terminal integration (f.e. mouse support)
Plug 'wincent/terminus'

call plug#end()

" for NixOS
set shell=/bin/sh

" visual settings
set number relativenumber
colorscheme monokai

" navigation
nnoremap <silent> <A-h> :wincmd h<cr>
nnoremap <silent> <A-j> :wincmd j<cr>
nnoremap <silent> <A-k> :wincmd k<cr>
nnoremap <silent> <A-l> :wincmd l<cr>

" coc.nvim config
set updatetime=300

"" use tab for completion
inoremap <silent><expr> <TAB>
  \ pumvisible() ? "\<C-n>" :
  \ <SID>check_back_space() ? "\<TAB>" :
  \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1] =~# '\s'
endfunction

"" use c-space to trigger completion
inoremap <silent><expr> <c-space> coc#refresh()

"" create function text object (if supported by languageserver)
xnoremap if <Plug>(coc-funcobj-i)
xnoremap af <Plug>(coc-funcobj-a)
nnoremap if <Plug>(coc-funcobj-i)
nnoremap af <Plug>(coc-funcobj-a)

"" highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

