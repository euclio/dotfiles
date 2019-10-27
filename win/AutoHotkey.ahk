; Function to set an environment variable permanently and globally.
EnvSetX(variable, value){
  Run, %comspec% /c setx "%variable%" "%value%",, Hide
}

EnvGet, home, HOME

; Set some environment variables
if (home == "") {
  EnvSetX("HOME", "%HOMEDRIVE%%HOMEPATH%")
}
EnvSetX("XDG_CONFIG_HOME", "%HOME%\.config")
EnvSetX("XDG_DATA_HOME", "%HOME%\.local\share")
EnvSetX("XDG_CACHE_HOME", "%HOME%\.cache")
EnvUpdate

; Variables necessary for Vim
EnvSetX("VIMINIT", "let $MYVIMRC=$XDG_CONFIG_HOME . '/vim/vimrc' | source $MYVIMRC")
EnvSetX("GVIMINIT", "let $MYGVIMRC=$XDG_CONFIG_HOME . '/vim/gvimrc/' | source $MYGVIMRC")

; Treat Caps Lock as Left Control and disable Left Control.
Capslock::Ctrl
LCtrl::Return

; Map Win+P to Win+Q
#p::#q
