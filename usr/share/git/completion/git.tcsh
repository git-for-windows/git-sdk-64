if (${?prompt}) then
complete git  'p,*,`bash /usr/share/tcsh/git.complete git "${COMMAND_LINE}"`,'
complete gitk 'p,*,`bash /usr/share/tcsh/git.complete gitk "${COMMAND_LINE}"`,'
endif
