# Each exectuable to test is run with the `version` subcommand; It is
# expected to return with exit code 0.

$executables_to_test = @(
    'C:\test\git.exe',
    'C:\test\scalar.exe'
)

foreach ($executable in $executables_to_test)
{
    Write-Output "Now testing $($executable)"
    &$executable 'version'
    if ($LASTEXITCODE -ne 0) {
	# If the command failed, run the debugger to find out what function or
	# DLL could not be found and then exit the script with failure. The
	# missing DLL or EXE will be referenced near the end of the output

        # Set flag to ask the debugger to
	# - show loader stub dianostics,
	# - enable system critical breaks,
	# - stop on exception, and
	# - stop on unhandled user-mode exception
	#
	# For further details, see
	# https://learn.microsoft.com/en-us/windows-hardware/drivers/debugger/gflags-flag-table
        C:\dbg\gflags -i $executable +SLS +SCB +SOE +SUE

        C:\dbg\cdb.exe -c "g" -c "q" $executable 'version'

        exit 1
    }
}

exit 0
