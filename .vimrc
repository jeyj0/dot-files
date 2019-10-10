" ############
" PLUGINS
" ############
call plug#begin('~/.vim/plugged')

if executable('ack') || executable('ag')
  Plug 'mileszs/ack.vim'
endif

Plug 'ctrlpvim/ctrlp.vim'
Plug 'scrooloose/nerdtree'
Plug 'wincent/terminus'
Plug 'pangloss/vim-javascript'
Plug 'MaxMEllon/vim-jsx-pretty'
Plug 'crusoexia/vim-monokai'
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
Plug 'vim-scripts/yaifa.vim'
Plug 'ycm-core/YouCompleteMe', { 'do': 'python3 ./install.py' }

call plug#end()
" ############
" PLUGINS end
" ############

" fix stuff
set nocompatible

" do not save options when saving session
set ssop-=options

" enable line numbers
set number relativenumber

" set leader to ,
let mapleader = ','

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

" remap tab switching to Alt+[u,i]
execute "set <M-u>=\eu"
execute "set <M-i>=\ei"
nnoremap <silent> <M-u> :tabprevious<CR>
nnoremap <silent> <M-i> :tabnext<CR>
tnoremap <M-u> <C-M-PAGEDOWN>
tnoremap <M-i> <C-M-PAGEUP>

" select color-theme
colorscheme monokai

let NERDTreeShowLineNumbers=0
execute "set <M-f>=\ef"
nnoremap <silent> <M-f> :NERDTreeToggle<CR>
let NERDTreeIgnore = ['^\..*\.swp$', '^.git$']

" custom settings
set cul
inoremap <C-Space> <C-n>
set splitbelow " open buffers below current instead of above (includes :term)
set splitright " open buffers to the right instead of left

" use ag instead of grep for CtrlP file search
if executable('ag')
  " Use Ag over Grep
  set grepprg=ag\ --nogroup\ --nocolor

  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif

" CtrlP shortcut
nnoremap <silent> <leader>f :CtrlP<CR>

" do not jump to first occurence by default when using :Ack
cnoreabbrev Ack Ack!

" map :Ack to <leader>g (like grep)
nnoremap <leader>g :Ack!<Space>

" use ag for ack.vim
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

" use system clipboard
set clipboard=unnamedplus

" create terminal shortcuts
nnoremap <silent> <leader>t :terminal<CR>
nnoremap <silent> <leader>T :terminal ++curwin<CR>
