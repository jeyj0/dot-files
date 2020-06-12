" use space as leader
let mapleader=" "

" make typing colon easier and less error-prone
nnoremap <leader>; :

" make remembering leader-mappings easier using which-key
call which_key#register('<Space>', "g:which_key_map")
nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<cr>
vnoremap <silent> <leader> :<c-u>WhichKeyVisual '<Space>'<cr>

"" which-key dict setup (for nice output from which-key)
let g:which_key_map = {}
let g:which_key_map[';'] = ':'
let g:which_key_map.b = { 'name' : '+buffer' }
  let g:which_key_map.b.b = 'buffers'
  let g:which_key_map.b.u = 'prev'
  let g:which_key_map.b.i = 'next'
  let g:which_key_map.b.k = 'kill'
  let g:which_key_map.b.K = 'kill (force)'
let g:which_key_map.f = { 'name' : '+file' }
  let g:which_key_map.f.e = 'explorer'
  let g:which_key_map.f.s = 'save'
  let g:which_key_map.f.o = 'open'
let g:which_key_map.c = { 'name' : '+config' }
  let g:which_key_map.c.e = 'edit'
  let g:which_key_map.c.r = 'refresh'
  let g:which_key_map.c.t = { 'name': '+theme' }
    let g:which_key_map.c.t.d = 'dark'
    let g:which_key_map.c.t.l = 'light'
let g:which_key_map.w = { 'name' : '+window' }
  let g:which_key_map.w.q = 'close'
  let g:which_key_map.w.s = 'vsplit'
  let g:which_key_map.w.v = 'edit'
  let g:which_key_map.w.p = 'prev'
  let g:which_key_map.w.H = 'switch left'
  let g:which_key_map.w.J = 'switch down'
  let g:which_key_map.w.K = 'switch up'
  let g:which_key_map.w.L = 'switch right'
  let g:which_key_map.w['='] = 'autoresize'
let g:which_key_map.n = { 'name' : '+narrow' }
  let g:which_key_map.n.r = 'region'
let g:which_key_map.g = { 'name' : '+git' }
  let g:which_key_map.g.g = 'interactive status'
  let g:which_key_map.g.s = 'status'
  let g:which_key_map.g.c = { 'name' : '+commit' }
    let g:which_key_map.g.c.c = 'create'
  let g:which_key_map.g.f = { 'name' : '+file' }
    let g:which_key_map.g.f.b = 'blame'
  let g:which_key_map.g.b = { 'name' : '+branch' }
    let g:which_key_map.g.b.b = 'checkout'
  let g:which_key_map.g.p = 'push'
let g:which_key_map.o = { 'name' : '+open' }
  let g:which_key_map.o.t = 'terminal'
  let g:which_key_map.o.s = 'scratch'
let g:which_key_map.s = { 'name' : '+search' }
  let g:which_key_map.s.w = 'web'
  let g:which_key_map.s.p = 'project'
let g:which_key_map.t = { 'name' : '+tab' }
  let g:which_key_map.t.u = 'prev'
  let g:which_key_map.t.i = 'next'
  let g:which_key_map.t.k = 'close'
  let g:which_key_map.t.t = 'open in new'
let g:which_key_map.p = { 'name': '+project' }
  let g:which_key_map.p.p = 'open project'
let g:which_key_map.h = 'left (win)'
let g:which_key_map.j = 'down (win)'
let g:which_key_map.k = 'up (win)'
let g:which_key_map.l = 'right (win)'
let g:which_key_map.u = 'prev buffer'
let g:which_key_map.i = 'next buffer'

" CONFIG
nnoremap <silent> <leader>cr :source ~/.config/nvim/init.vim<CR>
nnoremap <silent> <leader>ce :e ~/.config/nvim/init.vim<cr>
nnoremap <silent> <leader>ctd :call DarkTheme()<cr>
nnoremap <silent> <leader>ctl :call LightTheme()<cr>

" FILES
nnoremap <silent> <leader><leader> :GitFiles<cr>
nnoremap <silent> <leader>fo :Files<cr>
nnoremap <silent> <leader>fs :w<CR>
nnoremap <silent> <leader>fe :RangerWorkingDirectory<cr>

" SEARCH
nnoremap <silent> <leader>sp :Ag<cr>
"" web search
nnoremap <silent> <leader>sw :set opfunc=WebSearch<CR>g@
vnoremap <silent> <leader>sw :<C-u>call WebSearch(visualmode(), 1)<CR>
"" remove search highlighting with esc
nnoremap <silent> <esc> :nohlsearch<cr>

" BUFFERS
nnoremap <silent> <leader>u :bprevious<cr>
nnoremap <silent> <leader>bu :bprevious<cr>
nnoremap <silent> <leader>i :bnext<cr>
nnoremap <silent> <leader>bi :bnext<cr>
nnoremap <silent> <leader>bk :Bclose<cr>
nnoremap <silent> <leader>bK :Bclose!<cr>
nnoremap <silent> <leader>bb :Buffers<cr>

" TABS
nnoremap <silent> <leader>tu :tabprevious<cr>
nnoremap <silent> <leader>ti :tabnext<cr>
nnoremap <silent> <leader>tt :tab split<cr>
nnoremap <silent> <leader>tk :tabclose<cr>

" PROJECT
nnoremap <leader>pp :cd ~/projects/

" navigating code
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gr <Plug>(coc-rename)
nmap <silent> [e <Plug>(coc-diagnostic-prev)
nmap <silent> ]e <Plug>(coc-diagnostic-next)

" taken from coc.nvim readme
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

" GIT
nnoremap <silent> <leader>gg :Git<cr>:wincmd T<cr>
nnoremap <silent> <leader>gs :Git status --short<cr>
nnoremap <silent> <leader>gcc :Git commit<cr>
nnoremap <leader>gcb :Git checkout<space>
nnoremap <silent> <leader>gfb :Git blame<cr>
nnoremap <leader>gp :Git push<cr>

"" fugitive-only
augroup fugitivemaps
  autocmd! fugitivemaps
  autocmd Filetype fugitive nmap <buffer> <silent> <tab> =
augroup end


" OPEN
nnoremap <silent> <leader>ot :botright 20split +terminal<cr>
nnoremap <silent> <leader>os :botright 20split /tmp/vim_scratch<cr>:set bufhidden=delete<cr>:set buftype=nofile<cr>:setlocal noswapfile<cr>

" WINDOWS
nnoremap <silent> <leader>wH :wincmd h<cr>:wincmd p<cr>:<C-u>call SwitchWindows()<cr>
nnoremap <silent> <leader>wJ :wincmd j<cr>:wincmd p<cr>:<C-u>call SwitchWindows()<cr>
nnoremap <silent> <leader>wK :wincmd k<cr>:wincmd p<cr>:<C-u>call SwitchWindows()<cr>
nnoremap <silent> <leader>wL :wincmd l<cr>:wincmd p<cr>:<C-u>call SwitchWindows()<cr>
nnoremap <silent> <leader>w= :wincmd =<cr>
nnoremap <silent> <leader>wv :vsplit<cr>
nnoremap <silent> <leader>ws :split<cr>
nnoremap <silent> <leader>wq :q<cr>
nnoremap <silent> <leader>h :wincmd h<cr>
nnoremap <silent> <leader>j :wincmd j<cr>
nnoremap <silent> <leader>k :wincmd k<cr>
nnoremap <silent> <leader>l :wincmd l<cr>
nnoremap <silent> <leader>wp :wincmd p<cr>

" CODE EDITING

"" easily split lines in normal mode
"" taken and modified from drzel/vim-split-line
nnoremap <A-CR> :keeppatterns substitute/\s*\%#\s*/\r/e <bar> normal! ==<CR>

"" use tab to navigate completion list and Enter to select
"" (only relevant for snippets)

inoremap <silent><expr> <TAB>
  \ pumvisible() ? "\<C-n>" :
  \ <SID>check_back_space() ? "\<TAB>" :
  \ coc#refresh()
inoremap <expr><S-Tab> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

"" trigger autocompletion with ctrl-space
inoremap <silent><expr> <c-space> coc#refresh()

"" move to next snippet part with alt-tab
let g:coc_snippet_next = '<A-TAB>'
let g:coc_snippet_prev = '<A-S-TAB>'

