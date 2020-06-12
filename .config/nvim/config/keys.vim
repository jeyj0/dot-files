" use space as leader
let mapleader=" "

" make remembering leader-mappings easier using which-key
call which_key#register('<Space>', "g:which_key_map")
nnoremap <silent> <leader> :<c-u>WhichKey '<Space>'<cr>
vnoremap <silent> <leader> :<c-u>WhichKeyVisual '<Space>'<cr>
let g:which_key_map = {}

" make typing colon easier and less error-prone
nnoremap <leader>; :
let g:which_key_map[';'] = ':'

" SESSION (z)
let g:which_key_map.z = { 'name' : '+session'}

nnoremap <silent> <leader>zs :SSave<cr>
  let g:which_key_map.z.s = 'save'
nnoremap <silent> <leader>zo :SLoad<cr>
  let g:which_key_map.z.o = 'open'
nnoremap <silent> <leader>zc :SClose<cr>
  let g:which_key_map.z.c = 'close'
nnoremap <silent> <leader>zd :SDelete<cr>
  let g:which_key_map.z.d = 'delete'

" CONFIG
let g:which_key_map.v = { 'name' : '+vim' }

nnoremap <silent> <leader>vr :source ~/.config/nvim/init.vim<CR>
  let g:which_key_map.v.r = 'config refresh'
nnoremap <silent> <leader>ve :e ~/.config/nvim/init.vim<cr>
  let g:which_key_map.v.e = 'config edit'

"" theme
  let g:which_key_map.v.t = { 'name': '+theme' }
nnoremap <silent> <leader>vtd :call DarkTheme()<cr>
    let g:which_key_map.v.t.d = 'dark'
nnoremap <silent> <leader>vtl :call LightTheme()<cr>
    let g:which_key_map.v.t.l = 'light'

" FILES
let g:which_key_map.f = { 'name' : '+file' }

nnoremap <silent> <leader><leader> :GitFiles<cr>
nnoremap <silent> <leader>fo :Files<cr>
  let g:which_key_map.f.o = 'open'
nnoremap <silent> <leader>fs :w<CR>
  let g:which_key_map.f.s = 'save'
nnoremap <silent> <leader>fe :RangerWorkingDirectory<cr>
  let g:which_key_map.f.e = 'explorer'

" SEARCH
let g:which_key_map.s = { 'name' : '+search' }

nnoremap <silent> <leader>sp :Ag<cr>
  let g:which_key_map.s.p = 'project'

"" web search
nnoremap <silent> <leader>sw :set opfunc=WebSearch<CR>g@
vnoremap <silent> <leader>sw :<C-u>call WebSearch(visualmode(), 1)<CR>
  let g:which_key_map.s.w = 'web'

"" remove search highlighting with esc
nnoremap <silent> <esc> :nohlsearch<cr>

" BUFFERS
let g:which_key_map.b = { 'name' : '+buffer' }

nnoremap <silent> <leader>u :bprevious<cr>
  let g:which_key_map.u = 'prev buffer'
nnoremap <silent> <leader>bu :bprevious<cr>
  let g:which_key_map.b.u = 'prev'
nnoremap <silent> <leader>i :bnext<cr>
  let g:which_key_map.i = 'next buffer'
nnoremap <silent> <leader>bi :bnext<cr>
  let g:which_key_map.b.i = 'next'
nnoremap <silent> <leader>bk :Bclose<cr>
  let g:which_key_map.b.k = 'kill'
nnoremap <silent> <leader>bK :Bclose!<cr>
  let g:which_key_map.b.K = 'kill (force)'
nnoremap <silent> <leader>bb :Buffers<cr>
  let g:which_key_map.b.b = 'buffers'
nnoremap <silent> <leader>bo :<C-u>call CloseAllBuffersButCurrent()<cr>
  let g:which_key_map.b.o = 'only (close others)'

" TABS
let g:which_key_map.t = { 'name' : '+tab' }

nnoremap <silent> <leader>tu :tabprevious<cr>
  let g:which_key_map.t.u = 'prev'
nnoremap <silent> <leader>ti :tabnext<cr>
  let g:which_key_map.t.i = 'next'
nnoremap <silent> <leader>tt :tab split<cr>
  let g:which_key_map.t.t = 'open in new'
nnoremap <silent> <leader>tk :tabclose<cr>
  let g:which_key_map.t.k = 'close'

" PROJECT
let g:which_key_map.p = { 'name': '+project' }

nnoremap <leader>pp :cd ~/projects/
  let g:which_key_map.p.p = 'open project'

"" comment toggle
nnoremap <leader>cc :ToggleCodeComment<cr>
vnoremap <leader>cc :ToggleCodeComment<cr>
  let g:which_key_map.c.c = 'comment (toggle)'

" navigating code
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gr <Plug>(coc-references)
nmap <silent> [e <Plug>(coc-diagnostic-prev)
nmap <silent> ]e <Plug>(coc-diagnostic-next)

nnoremap <silent> K :call ShowDocumentation()<CR>

xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" GIT
let g:which_key_map.g = { 'name' : '+git' }

nnoremap <silent> <leader>gg :Git<cr>:wincmd T<cr>
  let g:which_key_map.g.g = 'interactive status'
nnoremap <silent> <leader>gs :Git status --short<cr>
  let g:which_key_map.g.s = 'status'
nnoremap <leader>gp :Git push<cr>
  let g:which_key_map.g.p = 'push'

"" commit
  let g:which_key_map.g.c = { 'name' : '+commit' }
nnoremap <silent> <leader>gcc :Git commit<cr>
    let g:which_key_map.g.c.c = 'create'
nnoremap <silent> <leader>gca :Git commit --amend<cr>
    let g:which_key_map.g.c.a = 'amend'

"" branch
  let g:which_key_map.g.b = { 'name' : '+branch' }
nnoremap <leader>gcb :Git checkout<space>
    let g:which_key_map.g.b.b = 'checkout'

"" file
  let g:which_key_map.g.f = { 'name' : '+file' }
nnoremap <silent> <leader>gfb :Git blame<cr>
    let g:which_key_map.g.f.b = 'blame'

"" fugitive-only
augroup fugitivemaps
  autocmd! fugitivemaps
  autocmd Filetype fugitive nmap <buffer> <silent> <tab> =
augroup end


" OPEN
let g:which_key_map.o = { 'name' : '+open' }

nnoremap <silent> <leader>ot :botright 20split +terminal<cr>
  let g:which_key_map.o.t = 'terminal'
nnoremap <silent> <leader>os :botright 20split /tmp/vim_scratch<cr>:set bufhidden=delete<cr>:set buftype=nofile<cr>:setlocal noswapfile<cr>
  let g:which_key_map.o.s = 'scratch'

" WINDOWS
let g:which_key_map.w = { 'name' : '+window' }

"" window moving
nnoremap <silent> <leader>wH :wincmd h<cr>:wincmd p<cr>:<C-u>call SwitchWindows()<cr>
  let g:which_key_map.w.H = 'switch left'
nnoremap <silent> <leader>wJ :wincmd j<cr>:wincmd p<cr>:<C-u>call SwitchWindows()<cr>
  let g:which_key_map.w.J = 'switch down'
nnoremap <silent> <leader>wK :wincmd k<cr>:wincmd p<cr>:<C-u>call SwitchWindows()<cr>
  let g:which_key_map.w.K = 'switch up'
nnoremap <silent> <leader>wL :wincmd l<cr>:wincmd p<cr>:<C-u>call SwitchWindows()<cr>
  let g:which_key_map.w.L = 'switch right'

"" split handling
nnoremap <silent> <leader>w= :wincmd =<cr>
  let g:which_key_map.w['='] = 'autoresize'
nnoremap <silent> <leader>wv :vsplit<cr>
  let g:which_key_map.w.v = 'vsplit'
nnoremap <silent> <leader>ws :split<cr>
  let g:which_key_map.w.s = 'split'
nnoremap <silent> <leader>wq :q<cr>
  let g:which_key_map.w.q = 'close'

"" window navigation
nnoremap <silent> <leader>h :wincmd h<cr>
let g:which_key_map.h = 'left (win)'
nnoremap <silent> <leader>j :wincmd j<cr>
let g:which_key_map.j = 'down (win)'
nnoremap <silent> <leader>k :wincmd k<cr>
let g:which_key_map.k = 'up (win)'
nnoremap <silent> <leader>l :wincmd l<cr>
let g:which_key_map.l = 'right (win)'

nnoremap <silent> <leader>wp :wincmd p<cr>
  let g:which_key_map.w.p = 'prev'

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

