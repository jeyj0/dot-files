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

" make some plugins support repeating actions by using .
Plug 'tpope/vim-repeat'

" modify surround brackets,...
Plug 'tpope/vim-surround'

" plugins for different nvim runners
if exists('g:vscode')
    " TODO remove this
    Plug 'asvetliakov/vscode-neovim', { 'dir': './vim' }
endif

call plug#end()

" share clipboard with system
set clipboard=unnamedplus

" KEY BINDINGS
"" use space as leader
let mapleader=" "

"" reload config with hrr
nnoremap <leader>hrr :source ~/.config/nvim/init.vim<CR>

" from here on, only settings for different nvim versions (in vscode, terminal, gui,...) goes
if exists('g:vscode')
    " VSCode only settings

    " KEY BINDINGS

    " find file
    nnoremap <leader><leader> :<C-u>call VSCodeNotify('workbench.action.quickOpen')<CR>

    " open folder/project
    nnoremap <leader>pp :<C-u>call VSCodeNotify('workbench.action.openWorkspace')<CR>

    " file save
    nnoremap <leader>fs :<C-u>call VSCodeNotify('workbench.action.files.save')<CR>

    " window split
    nnoremap <leader>ws :<C-u>call VSCodeNotify('workbench.editor.splitEditor')<CR>

    " git go
    nnoremap <leader>gg :<C-u>call VSCodeNotify('magit.status')<CR>

    " search project
    nnoremap <leader>sp :<C-u>call VSCodeNotify('workbench.action.findInFiles')<CR>
endif
if !exists('g:vscode')
    call plug#begin('$HOME/.config/nvim/jeyj0-plugged')

    " language support for basically everything
    Plug 'sheerun/vim-polyglot'

    " auto-close brackets,...
    Plug 'cohama/lexima.vim'

    " color-scheme
    Plug 'crusoexia/vim-monokai'

    " fancy status line
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    " Plug 'tpope/vim-fugitive'

    " autocompletion
    Plug 'neoclide/coc.nvim', { 'branch': 'release', 'do': 'npm ci' }

    " better terminal integration (f.e. mouse support)
    Plug 'wincent/terminus'

    " easy git staging
    Plug 'airblade/vim-gitgutter'

    " use ranger
    Plug 'francoiscabrol/ranger.vim' " this sets <leader>f to open ranger
    Plug 'rbgrouleff/bclose.vim' " required by ranger

    " get fzf
    Plug 'junegunn/fzf'
    Plug 'junegunn/fzf.vim'

    " better indentation
    Plug 'vim-scripts/yaifa.vim'

    " auto-formatting
    Plug 'prettier/vim-prettier', { 'do': 'npm install' }

    " nice todo-list management
    Plug 'jeyj0/vim-todo-lists'

    call plug#end()

    " do not move items on state change for todo lists
    let g:VimTodoListsMoveItems = 0
    let g:VimTodoListsDatesEnabled = 1

    " Original implementation taken from:
    " https://www.reddit.com/r/vim/comments/ebaoku/function_to_google_any_text_object/
    function! WebSearch(type, ...)
        let sel_save = &selection
        let &selection = "inclusive"
        let reg_save = @@

        if a:0  " Invoked from Visual mode, use '< and '> marks.
            silent exe "normal! `<" . a:type . "`>y"
        elseif a:type == 'line'
            silent exe "normal! '[V']y"
        elseif a:type == 'block'
            silent exe "normal! `[\<C-V>`]y"
        else
            silent exe "normal! `[v`]y"
        endif

        let search = substitute(trim(@@), ' \+', '+', 'g')
        silent exe "!open 'https://duckduckgo.com/?q=" . search . "'"

        let &selection = sel_save
        let @@ = reg_save
    endfunction

    nnoremap <silent> gs :set opfunc=WebSearch<CR>g@
    vnoremap <silent> gs :<C-u>call WebSearch(visualmode(), 1)<CR>

    " fzf commands / shortcuts
    let $FZF_DEFAULT_COMMAND = 'ag -g ""'

    set noshowmode
    let g:airline_powerline_fonts = 1
    let g:airline_section_y = 'B:%{bufnr("%")}' " buf.nr. instead of encoding
    let g:airline_section_z = '%l/%L:%c' " minimalistic line indicator
    let g:airline_theme='minimalist'
    let g:bufferline_echo = 0
    let g:airline_mode_map = {}
    let g:airline_mode_map['ic'] = 'INSERT'

    " keymap to open default todo list
    nnoremap <silent> <leader>ls :topleft vsplit ~/main.todo<cr>

    " do not map <leader>bd to close buffer (bclose.vim)
    let g:bclose_no_plugin_maps = 1

    " use ranger when opening a directory
    let g:ranger_map_keys = 0
    let g:ranger_replace_netrw = 1
    nnoremap <leader>r :RangerWorkingDirectory<cr>

    " set for-me-intuitive split opening
    set splitright
    set splitbelow

    " smart search
    set ignorecase
    set smartcase

    " visual settings
    set number relativenumber
    colorscheme monokai

    " configure neovim-remote to work properly with git
    autocmd FileType gitcommit,gitrebase,gitconfig,diff set bufhidden=delete

    " navigation
    nnoremap <silent> <A-h> :wincmd h<cr>
    nnoremap <silent> <A-j> :wincmd j<cr>
    nnoremap <silent> <A-k> :wincmd k<cr>
    nnoremap <silent> <A-l> :wincmd l<cr>
    nnoremap <silent> <A-U> :bprevious<cr>
    nnoremap <silent> <A-u> :tabprevious<cr>
    nnoremap <silent> <A-I> :bnext<cr>
    nnoremap <silent> <A-i> :tabnext<cr>
    tnoremap <silent> <A-h> <C-\><C-n>:wincmd h<cr>
    tnoremap <silent> <A-j> <C-\><C-n>:wincmd j<cr>
    tnoremap <silent> <A-k> <C-\><C-n>:wincmd k<cr>
    tnoremap <silent> <A-l> <C-\><C-n>:wincmd l<cr>
    tnoremap <silent> <A-U> <C-\><C-n>:bprevious<cr>
    tnoremap <silent> <A-u> <C-\><C-n>:tabprevious<cr>
    tnoremap <silent> <A-I> <C-\><C-n>:bnext<cr>
    tnoremap <silent> <A-i> <C-\><C-n>:tabnext<cr>

    " setup terminal
    autocmd TermOpen * setlocal nonumber norelativenumber " no line numbers in terminal buffer
    autocmd TermOpen * startinsert " start terminal in terminal-mode

    "" exit terminal mode with Alt-n
    tnoremap <A-n> <C-\><C-N>

    "" create default terminal with <leader>-t
    nnoremap <silent> <leader>t :botright 20split +terminal<cr>
    """ create different terminals at different locations
    nnoremap <silent> <leader>T :terminal<cr>
    nnoremap <silent> <leader><leader>rt :botright vsplit +terminal<cr>
    nnoremap <silent> <leader><leader>lt :topleft vsplit +terminal<cr>
    nnoremap <silent> <leader><leader>tt :topleft split +terminal<cr>
    nnoremap <silent> <leader><leader>bt :botright split +terminal<cr>

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

    " open fuzzy-file-finder
    "" git-aware
    nnoremap <leader>f :GFiles<cr>
    "" git-unaware
    nnoremap <leader>F :Files<cr>
    "" fuzzy-find with ag
    nnoremap <leader>g :Ag<cr>
    "" buffers
    nnoremap <leader>b :Buffers<cr>

    " closing buffers
    nnoremap <silent> <leader>q :Bclose<cr>

    " easily split lines in normal mode
    " taken from drzel/vim-split-line
    nnoremap <CR> :keeppatterns substitute/\s*\%#\s*/\r/e <bar> normal! ==<CR>

    " auto-format for different filetypes
    augroup formatbindings
    autocmd! formatbindings
    autocmd FileType *.js,*.jsx,*.mjs,*.ts,*.tsx,*.css,*.less,*.scss,*.json,*.graphql,*.md,*.vue,*.yaml,*.html
        \ nnoremap <buffer> <silent> <leader>p <Plug>(Prettier)
    autocmd FileType rust nnoremap <buffer> <silent> <leader>p :RustFmt<cr>
    augroup end
endif
