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

let mapleader=","

" set for-me-intuitive split opening
set splitright
set splitbelow

" smart search
set ignorecase
set smartcase

" visual settings
set number relativenumber
colorscheme monokai

" navigation
nnoremap <silent> <A-h> :wincmd h<cr>
nnoremap <silent> <A-j> :wincmd j<cr>
nnoremap <silent> <A-k> :wincmd k<cr>
nnoremap <silent> <A-l> :wincmd l<cr>
tnoremap <silent> <A-h> <C-\><C-n>:wincmd h<cr>
tnoremap <silent> <A-j> <C-\><C-n>:wincmd j<cr>
tnoremap <silent> <A-k> <C-\><C-n>:wincmd k<cr>
tnoremap <silent> <A-l> <C-\><C-n>:wincmd l<cr>

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
