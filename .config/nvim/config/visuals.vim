" visual settings
set number relativenumber

set scrolloff=3
set nowrap

" customize current-line-color (less extreme)
highlight CursorLine ctermbg=black

function LightTheme()
  colorscheme solarized
  set background=light
  AirlineTheme solarized
endfunction

function DarkTheme()
  let g:gruvbox_italic = 1
  let g:gruvbox_contrast_dark = 'hard'
  let g:gruvbox_sign_column = 'bg0'
  set background=dark
  colorscheme gruvbox
  AirlineTheme gruvbox
endfunction

call DarkTheme()

set noshowmode
let g:airline_powerline_fonts = 0
let g:airline_section_y = 'B:%{bufnr("%")}' " buf.nr. instead of encoding
let g:airline_section_z = '%l/%L:%c' " minimalistic line indicator
let g:airline_theme='gruvbox'
let g:bufferline_echo = 0
let g:airline_mode_map = {}
let g:airline_mode_map['ic'] = 'INSERT'
let g:airline_mode_map = {
  \ '__'     : '-',
  \ 'c'      : 'c',
  \ 'i'      : 'i',
  \ 'ic'     : 'i',
  \ 'ix'     : 'i',
  \ 'n'      : 'n',
  \ 'multi'  : 'm',
  \ 'ni'     : 'n',
  \ 'no'     : 'n',
  \ 'R'      : 'r',
  \ 'Rv'     : 'r',
  \ 's'      : 's',
  \ 'S'      : 's',
  \ ''     : 's',
  \ 't'      : 't',
  \ 'v'      : 'v',
  \ 'V'      : 'v',
  \ ''     : 'v',
  \ }

" highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" highlight current line (in current window)
augroup CursorLine
  au!
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  au WinLeave * setlocal nocursorline
augroup END
