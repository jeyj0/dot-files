" use space as leader
let mapleader=" "

" reload config with hrr
nnoremap <leader>hrr :source ~/.config/nvim/init.vim<CR>

nnoremap <silent> <esc> :nohlsearch<cr>

" handling files
nnoremap <silent> <leader><leader> :GitFiles<cr>
nnoremap <silent> <leader>fo :Files<cr>
nnoremap <silent> <leader>fs :w<CR>
nnoremap <silent> <leader>fe :RangerWorkingDirectory<cr>

nnoremap <silent> <leader>sp :Ag<cr>

" handling buffers
nnoremap <silent> <leader>bk :Bclose<cr>
nnoremap <silent> <leader>bb :Buffers<cr>

" handling windows
nnoremap <silent> <leader>tt :tab split<cr>

" navigating code
nmap gd <Plug>(coc-definition)
nmap gr <Plug>(coc-rename)
nmap [e <Plug>(coc-diagnostic-prev)
nmap ]e <Plug>(coc-diagnostic-next)

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

nnoremap <silent> K :call <SID>show_documentation()<CR>

xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" interactive git status with gg
nnoremap <silent> <leader>gg :Git<cr>

" internal terminal
nnoremap <silent> <leader>ot :botright 20split +terminal<cr>

" navigation
nnoremap <silent> <A-h> :wincmd h<cr>
nnoremap <silent> <A-j> :wincmd j<cr>
nnoremap <silent> <A-k> :wincmd k<cr>
nnoremap <silent> <A-l> :wincmd l<cr>
nnoremap <silent> <A-u> :bprevious<cr>
nnoremap <silent> <A-U> :tabprevious<cr>
nnoremap <silent> <A-i> :bnext<cr>
nnoremap <silent> <A-I> :tabnext<cr>
tnoremap <silent> <A-h> <C-\><C-n>:wincmd h<cr>
tnoremap <silent> <A-j> <C-\><C-n>:wincmd j<cr>
tnoremap <silent> <A-k> <C-\><C-n>:wincmd k<cr>
tnoremap <silent> <A-l> <C-\><C-n>:wincmd l<cr>
tnoremap <silent> <A-u> <C-\><C-n>:bprevious<cr>
tnoremap <silent> <A-U> <C-\><C-n>:tabprevious<cr>
tnoremap <silent> <A-i> <C-\><C-n>:bnext<cr>
tnoremap <silent> <A-I> <C-\><C-n>:tabnext<cr>

" easily split lines in normal mode
" taken and modified from drzel/vim-split-line
nnoremap <A-CR> :keeppatterns substitute/\s*\%#\s*/\r/e <bar> normal! ==<CR>

" use tab to navigate completion list and Enter to select
" (only relevant for snippets)
inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" web search
nnoremap <silent> <leader>gs :set opfunc=WebSearch<CR>g@
vnoremap <silent> <leader>gs :<C-u>call WebSearch(visualmode(), 1)<CR>

