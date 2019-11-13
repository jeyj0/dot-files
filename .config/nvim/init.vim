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
Plug 'neoclide/coc.nvim', { 'branch': 'release' }

" better terminal integration (f.e. mouse support)
Plug 'wincent/terminus'

call plug#end()

colorscheme monokai
