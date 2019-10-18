" ############
" PLUGINS
" ############

" download vim-plug (plugin-manager) if non-existent
" (first-time start of vim on a new machine)
" also installs plugins after vim has started
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.vim/plugged')

if executable('ack') || executable('ag')
  Plug 'mileszs/ack.vim'
endif

Plug 'tpope/vim-sensible'
Plug 'tpope/vim-airline'
Plug 'tpope/vim-repeat' " repeatable plugin actions
Plug 'tpope/vim-surround'

" simpler text substitution
Plug 'svermeulen/vim-subversive'

" color schemes
Plug 'crusoexia/vim-monokai'

" file handling
Plug 'vifm/vifm.vim'
Plug 'ctrlpvim/ctrlp.vim'

" better terminal integration
Plug 'wincent/terminus'

" frontend dev
Plug 'pangloss/vim-javascript'
Plug 'MaxMEllon/vim-jsx-pretty'
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }

Plug 'racer-rust/vim-racer'

" generic coding
Plug 'vim-scripts/yaifa.vim'
Plug 'ycm-core/YouCompleteMe', { 'do': 'python3 ./install.py' }

call plug#end()
" ############
" PLUGINS end
" ############

" fix stuff
set nocompatible

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
set esckeys
execute "set <M-u>=\eu"
execute "set <M-i>=\ei"
nnoremap <silent> <M-u> :tabprevious<CR>
nnoremap <silent> <M-i> :tabnext<CR>
tnoremap <M-u> <C-W>:tabprevious<CR>
tnoremap <M-i> <C-W>:tabnext<CR>

" select color-theme
silent! colorscheme monokai

execute "set <M-f>=\ef"
nnoremap <silent> <M-f> :Vifm<CR>

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
nnoremap <silent> <leader>t :terminal ++close<CR>
nnoremap <silent> <leader>T :terminal ++close ++curwin<CR>

" create split-shortcuts
nnoremap <silent> <leader>v :vsp<CR>
nnoremap <silent> <leader>x :sp<CR>

" make text-search more intelligent
set ignorecase
set smartcase

" vim-subversive
nmap s <plug>(SubversiveSubstitute)
nmap ss <plug>(SubversiveSubstituteLine)
nmap <leader>s <plug>(SubversiveSubstituteRange)
xmap <leader>s <plug>(SubversiveSubstituteRange)
nmap <leader>ss <plug>(SubversiveSubstituteWordRange)
