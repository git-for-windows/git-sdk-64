#compdef p11-kit

local -a reply

local -a _p11_kit_commands=(
  'list-modules:list modules and tokens'
  'list-tokens:list tokens'
  'list-objects:list objects of a token'
  'import-object:import object into a token'
  'export-object:export object matching PKCS11 URI'
  'delete-object:delete objects matching PKCS11 URI'
  'generate-keypair:generate key-pair on a PKCS11 token'
  'list-profiles:list PKCS11 profiles supported by the token'
  'add-profile:add PKCS11 profile to the token'
  'delete-profile:delete PKCS11 profile from the token'
  'print-config:print merged configuration'
  'list-mechanisms:list supported mechanisms'
  'remote:run a specific PKCS11 module remotely'
  'server:run a server process that exposes PKCS11 module remotely'
)

_regex_words p11-kit-commands 'p11-kit command' $_p11_kit_commands
_regex_arguments _p11-kit_cmd /$'[^\0]#\0'/ "$reply[@]"

_arguments -S \
  '(-h --help)'{-h,--help}'[show a help message and exit]' \
  '*:: := _p11-kit_cmd'
