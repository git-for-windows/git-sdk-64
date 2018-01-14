#ifndef EXIT_PROCESS_H
#define EXIT_PROCESS_H

/*
 * This file contains functions to terminate a Win32 process, as gently as
 * possible.
 *
 * If appropriate, we will attempt to generate a console Ctrl event.
 * Otherwise we will fall back to terminating the entire process tree.
 *
 * As we do not want to export this function in the MSYS2 runtime, these
 * functions are marked as file-local.
 */

#include <tlhelp32.h>

/**
 * Terminates the process corresponding to the process ID
 *
 * This way of terminating the processes is not gentle: the process gets
 * no chance of cleaning up after itself (closing file handles, removing
 * .lock files, terminating spawned processes (if any), etc).
 */
static int
terminate_process(HANDLE process, int exit_code)
{
  return int(TerminateProcess (process, exit_code));
}

/**
 * Terminates the process corresponding to the process ID and all of its
 * directly and indirectly spawned subprocesses using the provided
 * terminate callback function
 */
static int
terminate_process_tree(HANDLE main_process, int exit_code, int (*terminate)(HANDLE, int))
{
  HANDLE snapshot = CreateToolhelp32Snapshot (TH32CS_SNAPPROCESS, 0);
  PROCESSENTRY32 entry;
  DWORD pids[16384];
  int max_len = sizeof (pids) / sizeof (*pids), i, len, ret = 0;
  pid_t pid = GetProcessId (main_process);

  pids[0] = (DWORD) pid;
  len = 1;

  /*
   * Even if Process32First()/Process32Next() seem to traverse the
   * processes in topological order (i.e. parent processes before
   * child processes), there is nothing in the Win32 API documentation
   * suggesting that this is guaranteed.
   *
   * Therefore, run through them at least twice and stop when no more
   * process IDs were added to the list.
   */
  for (;;)
    {
      int orig_len = len;
      pid_t cyg_pid;

      memset (&entry, 0, sizeof (entry));
      entry.dwSize = sizeof (entry);

      if (!Process32First (snapshot, &entry))
        break;

      do
        {
          for (i = len - 1; i >= 0; i--)
            {
              cyg_pid = cygwin_winpid_to_pid(entry.th32ProcessID);
              if (cyg_pid > -1)
                {
                  kill(cyg_pid, exit_code);
                  continue;
                }
              if (pids[i] == entry.th32ProcessID)
                break;
              if (pids[i] == entry.th32ParentProcessID)
                pids[len++] = entry.th32ProcessID;
            }
        }
      while (len < max_len && Process32Next (snapshot, &entry));

      if (orig_len == len || len >= max_len)
        break;
    }

  for (i = len - 1; i >= 0; i--)
    {
      HANDLE process = i == 0 ? main_process :
        OpenProcess (PROCESS_TERMINATE, FALSE, pids[i]);

      if (process)
        {
          if (!(*terminate) (process, exit_code))
            ret = -1;
          CloseHandle (process);
        }
    }

  return ret;
}

/**
 * For SIGINT/SIGTERM, call GenerateConsoleCtrlEven(). Otherwise fall back to
 * running terminate_process_tree().
 */
static int
exit_process(HANDLE process, int exit_code, int okay_to_kill_this_process)
{
  DWORD code;

  if (GetExitCodeProcess (process, &code) && code == STILL_ACTIVE)
    {
      int signal = exit_code & 0x7f;
      if (signal == SIGINT || signal == SIGTERM)
        {
#ifndef __INSIDE_CYGWIN__
          if (!okay_to_kill_this_process)
            return -1;
          FreeConsole();
          if (!AttachConsole(GetProcessId(process)))
            return -1;
          if (GenerateConsoleCtrlEvent(signal == SIGINT ? CTRL_C_EVENT : CTRL_BREAK_EVENT, 0))
            return 0;
#else
          path_conv helper ("/bin/kill.exe");
          if (helper.exists ())
            {
              STARTUPINFOW si = {};
              PROCESS_INFORMATION pi;
              size_t len = helper.get_wide_win32_path_len ();
              WCHAR cmd[len + (2 * strlen (" -f -32 0xffffffff")) + 1];
              WCHAR title[] = L"kill";

              helper.get_wide_win32_path (cmd);
              __small_swprintf (cmd + len, L" -f -%d %d", signal, (int)GetProcessId(ch_spawn));

              si.cb = sizeof (si);
              si.dwFlags = STARTF_USESHOWWINDOW;
              si.wShowWindow = SW_HIDE;
              si.lpTitle = title;

              /* Create a new hidden process.  Use the two event handles as
                 argv[1] and argv[2]. */
              BOOL x = CreateProcessW (NULL, cmd, &sec_none_nih, &sec_none_nih,
                  true, CREATE_NO_WINDOW, NULL, NULL, &si, &pi);
              if (x)
                {
                  CloseHandle (pi.hThread);
                  if (WaitForSingleObject (pi.hProcess, 10000) == WAIT_OBJECT_0)
                    {
                      CloseHandle (pi.hProcess);
                      return 0;
                    }
                  CloseHandle (pi.hProcess);
                }
            }
#endif
        }

      return terminate_process_tree (process, exit_code, terminate_process);
    }

  return -1;
}

#endif
