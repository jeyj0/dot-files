" use space as leader
let mapleader=" "

" reload config with hrr
nnoremap <leader>hrr :source ~/.config/nvim/init.vim<CR>

" save with fs
nnoremap <leader>fs :w<CR>

" web search
nnoremap <silent> gs :set opfunc=WebSearch<CR>g@
vnoremap <silent> gs :<C-u>call WebSearch(visualmode(), 1)<CR>

" keymap to open default todo list
nnoremap <silent> <leader>ls :topleft vsplit ~/main.todo<cr>

" open ranger
nnoremap <leader>r :RangerWorkingDirectory<cr>

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
