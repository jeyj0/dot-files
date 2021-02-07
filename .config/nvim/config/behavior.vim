" enable project-specific vim settings
set exrc

" share clipboard with system
set clipboard=unnamedplus

" immediately preview changes in :%s/foo/bar substitutions (or similars)
:set inccommand=nosplit

" allow switching from buffer if buffer is unsaved
set hidden

" ask to save buffer when closed unsaved
set confirm

" nnn.vim
let g:nnn#layout = { 'window': { 'width': 0.6, 'height': 0.6 } }

" use ranger when opening a directory
let g:ranger_replace_netrw = 1

" fzf commands / shortcuts
let $FZF_DEFAULT_COMMAND = 'ag -g ""'

" fzf function to switch to project
function! CdToProject(project) abort
    execute 'cd' fnameescape("~/projects/" . a:project)
endfunction
function! SwitchToProject() abort
    call fzf#run(fzf#wrap('fzf_cd_project', { 'source': 'ls ~/projects/', 'sink': funcref('CdToProject') }, 0))
endfunction

" floaterm

"" close floaterm when process exits with 0
let g:floaterm_autoclose = 1

"" open git commit in a split if invoked from floaterm
let g:floaterm_gitcommit = 'floaterm'

"" always open floaterm at the current working directory
let g:floaterm_rootmarkers = []

"" set floaterm title
let g:floaterm_title = "Terminal $1/$2"

"" open files in a split when opened from floating terminal
let g:floaterm_open_command = 'vsplit'

" function to close terminals (special handling for floaterm)
function! CloseTerminal() abort
  let l:ft = &filetype
  if l:ft == "floaterm"
    execute "normal! :FloatermHide\<cr>"
  else
    execute "normal! :q\<cr>"
  endif
endfunction

" STARTIFY

augroup startify
    autocmd!

    " do not allow saving the startify view as a file
    autocmd User Startified setlocal buftype=nofile
augroup end

" auto-open startify when closing last buffer
" (and having only one window open)
"autocmd BufEnter * if line2byte('.') == -1 && len(tabpagebuflist()) == 1 | Startify | endif

" center startify header
let g:startify_custom_header =
    \ 'startify#center(startify#fortune#cowsay())'

" autosave startify sessions once opened
let g:startify_session_persistence = 1

" limit listed files
let g:startify_files_number = 5

" cd to vcs root after opening a file
let g:startify_change_to_vcs_root = 1

" function to list modified and untracked files from git
function! s:gitModifiedAndUntracked()
    let files = systemlist('git ls-files -m 2>/dev/null && git ls-files -o --exclude-standard 2>/dev/null')
    return map(files, "{'line': v:val, 'path': v:val}")
endfunction

" define startify lists and their order
let g:startify_lists = [
        \ { 'type': 'dir', 'header': ['   Recently used in '. getcwd()] },
        \ { 'type': function('s:gitModifiedAndUntracked'),  'header': ['   git (modified and untracked)']},
        \ { 'type': 'files', 'header': ['   Recently used'] },
        \ { 'type': 'sessions', 'header': ['   Sessions'] },
        \ { 'type': 'bookmarks', 'header': ['   Bookmarks'] },
        \ { 'type': 'commands', 'header': ['   Commands'] },
        \ ]

" remember to avoid mappings starting with q,e,i,b,s,v,t,<cr>
let g:startify_bookmarks = [
        \ {'ck': '~/.config/nvim/config/keys.vim'},
        \ {'cb': '~/.config/nvim/config/behavior.vim'},
        \ {'cp': '~/.config/nvim/config/plugins.vim'},
        \ {'cv': '~/.config/nvim/config/visuals.vim'},
        \ {'n': '~/nixos/common.nix'}
        \ ]

" STARTIFY END

" configure neovim-remote to work properly with git
autocmd FileType gitcommit,gitrebase,gitconfig,diff set bufhidden=delete

" coc.nvim config
set updatetime=300

" smart search
set ignorecase
set smartcase

" always keep signcolumn open
set signcolumn=yes

" setup terminal
autocmd TermOpen * setlocal nonumber norelativenumber " no line numbers in terminal buffer
autocmd TermOpen * startinsert " start terminal in terminal-mode

" taken from
" https://stackoverflow.com/a/37866336/4554254
function! CloseAllBuffersButCurrent()
  let curr = bufnr("%")
  let last = bufnr("$")

  if curr > 1    | silent! execute "1,".(curr-1)."bd"     | endif
  if curr < last | silent! execute (curr+1).",".last."bd" | endif
endfunction

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
    silent exe "!open 'https://search.jorre.dev/?q=" . search . "'"

    let &selection = sel_save
    let @@ = reg_save
endfunction

" taken from coc.nvim readme and modified
function! ShowDocumentation()
  try
    if (index(['vim','help'], &filetype) >= 0)
      execute 'h '.expand('<cword>')
    else
      call CocAction('doHover')
    endif
  catch
    echo "Sorry, no documentation or help found"
  endtry
endfunction

" code comment toggling
" inspired by https://stackoverflow.com/a/24046914/4554254

let s:comment_map = {
    \   "c": '\/\/',
    \   "cpp": '\/\/',
    \   "go": '\/\/',
    \   "java": '\/\/',
    \   "javascript": '\/\/',
    \   "lua": '--',
    \   "scala": '\/\/',
    \   "php": '\/\/',
    \   "python": '#',
    \   "ruby": '#',
    \   "rust": '\/\/',
    \   "sh": '#',
    \   "desktop": '#',
    \   "fstab": '#',
    \   "conf": '#',
    \   "profile": '#',
    \   "bashrc": '#',
    \   "bash_profile": '#',
    \   "mail": '>',
    \   "eml": '>',
    \   "bat": 'REM',
    \   "ahk": ';',
    \   "vim": '"',
    \   "tex": '%',
    \   "Dockerfile": '#',
    \   "nix": '#',
    \   "fish": '#',
    \   "haskell": '--',
    \   "typescript": '\/\/',
    \   "yaml.docker-compose": '#',
    \ }

function! ToggleCodeComment() range
    if has_key(s:comment_map, &filetype)
        let l:win_view = winsaveview()
        let l:comment_symbol = s:comment_map[&filetype]

        let l:should_comment = match(getline(a:firstline), '^\s*' . l:comment_symbol . ' ') == -1

        if l:should_comment
            execute "silent " . a:firstline . "," . a:lastline . "s/\\(^\\s*\\)\/\\1" . l:comment_symbol . " /"
        else
            execute "silent " . a:firstline . "," . a:lastline . "s/\\(^\\s*\\)" . l:comment_symbol . "\\s*/\\1/"
        endif

        call winrestview(l:win_view)
    else
        echo "No comment symbol found for filetype"
    endif
endfunction

command! -range ToggleCodeComment <line1>,<line2>call ToggleCodeComment()

" quick-scope
let g:qs_buftype_blacklist = ['terminal', 'vimwiki', 'startify']

augroup quick-scope
    autocmd!

    " do not allow saving the startify view as a file
    autocmd FileType terminal let b:qs_local_disable=1
    autocmd FileType vimwiki let b:qs_local_disable=1
    autocmd FileType startify let b:qs_local_disable=1
augroup end
