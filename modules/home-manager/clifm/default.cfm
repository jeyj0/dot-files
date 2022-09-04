# Default color scheme for CliFM

# FiletypeColors defines the color used for file types when listing files,
# just as InterfaceColors defines colors for CliFM's interface and ExtColors
# for file extensions. They all make use of the same format used by the
# LS_COLORS environment variable. Thus, "di=01;34" means that (non-empty)
# directories will be listed in bold blue.
# Color codes are traditional ANSI escape sequences less the escape char and
# the final 'm'. 8 bit, 256 colors, and RGB colors are supported.
# A detailed explanation of all these codes can be found in the manpage.

FiletypeColors="di=01;34:nd=01;31:ed=02;34:ne=02;31:fi=0:ef=02;33:nf=02;31:ln=01;36:mh=30;46:or=02;36:pi=00;35:so=01;35:bd=01;33:cd=01:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:ee=00;32:no=00;31;47:uf=34;47:"

InterfaceColors="el=01;33:mi=01;36:dl=00;34:tx=0:df=0:dc=00;02;34:wc=01;36:dh=00;36:li=01;32:si=01;34:ti=01;36:em=01;31:wm=01;33:nm=01;32:bm=01;36:sb=02;33:sh=02;35:sf=04;02;36:sc=02;31:sx=02;32:sp=02;31:hb=00;36:hc=02;31:hd=00;36:he=00;36:hn=00;35:hp=00;36:hq=00;33:hr=00;31:hs=00;32:hv=00;32:tt=02;01;36:ts=04;35:ws1=00;34:ws2=0;31:ws3=00;33:ws4=00;32:ws5=00;36:ws6=00;36:ws7=00;36:ws8=00;36:"

# Same as FiletypeColors, but for file extensions. The format is always
# *.EXT=COLOR
ExtColors="*.tar=01;31:*.tgz=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.rar=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.jpg=01;35:*.JPG=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.GIF=01;35:*.bmp=01;35:*.xbm=01;35:*.xpm=01;35:*.png=01;35:*.PNG=01;35:*.svg=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.webp=01;35:*.ogm=01;35:*.mp4=01;35:*.MP4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.wmv=01;35:*.flc=01;35:*.avi=01;35:*.flv=01;35:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mp3=00;36:*.MP3=00;36:*.ogg=00;36:*.wav=00;36:*.pdf=01;31:*.PDF=01;31:*.doc=35:*.docx=35:*.xls=35:*.xlsx=35:*.ppt=35:*.pptx=35:*.odt=35:*.ods=35:*.odp=35:*.cache=02;37:*.tmp=02;37:*.temp=02;37:*.log=02;37:*.bak=02;37:*.bk=02;37:*.in=02;37:*.out=02;37:*.part=02;37:*.aux=02;37:*.c=00;01:*.c++=00;01:*.h=00;01:*.cc=00;01:*.cpp=00;01:*.h=00;01:*.h++=00;01:*.hh=00;01:*.go=00;01:*.java=00;01:*.js=00;01:*.lua=00;01:*.rb=00;01:*.rs=00;01:"

DirIconColor="00;33"

DividingLine="-"

# Prompt="\[\e[0m\][\S\[\e[0m\]]\l \A \u:\H \[\e[00;36m\]\w\n\[\e[0m\]<\z\[\e[0m\]>\[\e[0;34m\] \$\[\e[0m\] "
Prompt="$(starship prompt)"

WarningPromptStr="\[\e[0m\]\[\e[00;02;31m\](!) > "

FzfTabOption="--color=16,prompt:6,fg+:-1,pointer:4,hl:5,hl+:5,gutter:-1,marker:2 --bind tab:accept,right:accept,left:abort --inline-info --layout=reverse-list"

