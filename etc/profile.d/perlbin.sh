# Set path to perl scriptdirs if they exist
# https://wiki.archlinux.org/index.php/Perl_Policy#Binaries_and_Scripts
# Added /usr/bin/*_perl dirs for scripts
# Remove /usr/lib/perl5/*_perl/bin in next release

if test -z "${PERL_PATH_EXTRA}"; then

    [ -d /usr/bin/site_perl ] && PERL_PATH_EXTRA="${PERL_PATH_EXTRA:+${PERL_PATH_EXTRA}:}/usr/bin/site_perl"
    [ -d /usr/lib/perl5/site_perl/bin ] && PERL_PATH_EXTRA="${PERL_PATH_EXTRA:+${PERL_PATH_EXTRA}:}/usr/lib/perl5/site_perl/bin"

    [ -d /usr/bin/vendor_perl ] && PERL_PATH_EXTRA="${PERL_PATH_EXTRA:+${PERL_PATH_EXTRA}:}/usr/bin/vendor_perl"
    [ -d /usr/lib/perl5/vendor_perl/bin ] && PERL_PATH_EXTRA="${PERL_PATH_EXTRA:+${PERL_PATH_EXTRA}:}/usr/lib/perl5/vendor_perl/bin"

    [ -d /usr/bin/core_perl ] && PERL_PATH_EXTRA="${PERL_PATH_EXTRA:+${PERL_PATH_EXTRA}:}/usr/bin/core_perl"

fi

export PERL_PATH_EXTRA PATH="${PATH}:${PERL_PATH_EXTRA}"

# If you have modules in non-standard directories you can add them here.
#export PERLLIB=dir1:dir2

