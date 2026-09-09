#compdef trust

local context state state_descr line
local -A opt_args
local -a reply

local -a optcommon=(
    '(-h --help)'{-h,--help}'[show a help message and exit]'
    '(-v --verbose)'{-v,--verbose}'[show verbose debug output]'
    '(-q --quiet)'{-q,--quiet}'[suppress command output]'
)

_trust_pkcs11() {
  local pkcs11_full="$(_call_program pkcs11 trust list)"
  local -a pkcs11=(${${${(M)${(@f)pkcs11_full}:#pkcs*}%;*}//:/\\:})
  local -a pkcs11_label=(
    ${${${(M)${(@f)pkcs11_full}:#[[:space:]]#label:*}#[[:space:]]#label: }//:/\\:}
  )

  _describe -t pkcs11 'pkcs11 uri' pkcs11_label pkcs11 "$@"
}

_trust_extract_filter() {
  _alternative \
    'filter:filter:(ca-anchors blocklist trust-policy certificates)' \
    'pkcs11:pkcs11 uri:_trust_pkcs11'
}

_trust_dump_filter() {
  _alternative \
    'filter:filter:(all)' \
    'pkcs11:pkcs11 uri:_trust_pkcs11'
}

_trust_anchor() {
  _alternative \
    'files:file:_files' \
    'pkcs11:pkcs11 uri:_trust_pkcs11'
}

local -a trust_commands=(
  'list:list trust or certificates'
  'extract:extract certificates and trust'
  'extract-compat:extract trust compatibility bundles'
  'anchor:add, remove, or change trust anchors'
  'dump:dump trust objects in internal format'
  'check-format:check the format of .p11-kit files'
)

local -a formats=(
  x509-file x509-directory
  pem-bundle pem-directory pem-directory-hash
  openssl-bundle openssl-directory
  java-cacerts edk2-cacerts
)

_arguments -S \
  '(-h --help)'{-h,--help}'[show a help message a exit]' \
  '1:trust command:->command' \
  '*:: :->argument'

local ret=1
case $state-$line[1] in
  command-*)
    _describe -t trust-command 'trust command' trust_commands && ret=0
    ;;
  argument-list)
    _arguments -S $optcommon \
      '--filter=[filter of what to export]:filter:_trust_extract_filter' \
      '--purpose=[limit to certificates usable for the purpose]:purpose:(server-auth client-auth email code-signing)' \
      && ret=0
    ;;
  argument-extract)
    _arguments -S $optcommon \
      '--filter=[filter of what to export]:filter:_trust_extract_filter' \
      '--format=[format to extract to]:format:'"($formats)" \
      '--purpose=[limit to certificates usable for the purpose]:purpose:(server-auth client-auth email code-signing)' \
      '(-f --overwrite)'{-f,--overwrite}'[overwrite output file or directory]' \
      '--comment[add comments to bundles if possible]' \
      '1:destination:_files' && ret=0
    ;;
  argument-extract-compat)
    _arguments -S $optcommon \
      '(-o --output)'{-o+,--output=}'[write the extracted trust store to directory instead of updating /etc/ca-certificates/extracted]' \
      && ret=0
    ;;
  argument-anchor)
    _arguments -S $optcommon \
      + '(action)' \
        '--store: :_files' \
        '--remove: :_trust_anchor' && ret=0
    ;;
  argument-dump)
    _arguments -S $optcommon \
      '--filter=[filter what to export]:filter:_trust_dump_filter' && ret=0
    ;;
  argument-check-format)
    _arguments -S $optcommon \
      '1:file:_files' && ret=0
    ;;
esac
return ret
