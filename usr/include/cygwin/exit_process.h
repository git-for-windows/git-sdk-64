#ifndef EXIT_PROCESS_H
#define EXIT_PROCESS_H

/*
 * This file contains functions to terminate a Win32 process, as gently as
 * possible.
 *
 * At first, we will attempt to inject a thread that calls ExitProcess(). If
 * that fails, we will fall back to terminating the entire process tree.
 *
 * As we do not want to export this function in the MSYS2 runtime, these
 * functions are marked as file-local.
 */

#include <tlhelp32.h>

/**
 * Terminates the process corresponding to the process ID and all of its
 * directly and indirectly spawned subprocesses.
 *
 * This way of terminating the processes is not gentle: the processes get
 * no chance of cleaning up after themselves (closing file handles, removing
 * .lock files, terminating spawned processes (if any), etc).
 */
static int
terminate_process_tree(HANDLE main_process, int exit_code)
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

      memset (&entry, 0, sizeof (entry));
      entry.dwSize = sizeof (entry);

      if (!Process32First (snapshot, &entry))
        break;

      do
        {
          for (i = len - 1; i >= 0; i--)
            {
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
          if (!TerminateProcess (process, exit_code))
            ret = -1;
          CloseHandle (process);
        }
    }

  return ret;
}

/**
 * Determine whether a process runs in the same architecture as the current
 * one. That test is required before we assume that GetProcAddress() returns
 * a valid address *for the target process*.
 */
static inline bool
process_architecture_matches_current(HANDLE process)
{
  static BOOL current_is_wow = -1;
  BOOL is_wow;

  if (current_is_wow == -1 &&
      !IsWow64Process (GetCurrentProcess (), &current_is_wow))
        current_is_wow = -2;
  if (current_is_wow == -2)
    return false; /* could not determine current process' WoW-ness */
  if (!IsWow64Process (process, &is_wow))
    return false; /* cannot determine */
  return is_wow == current_is_wow;
}

/**
 * Inject a thread into the given process that runs ExitProcess().
 *
 * Note: as kernel32.dll is loaded before any process, the other process and
 * this process will have ExitProcess() at the same address.
 *
 * This function expects the process handle to have the access rights for
 * CreateRemoteThread(): PROCESS_CREATE_THREAD, PROCESS_QUERY_INFORMATION,
 * PROCESS_VM_OPERATION, PROCESS_VM_WRITE, and PROCESS_VM_READ.
 *
 * The idea comes from the Dr Dobb's article "A Safer Alternative to
 * TerminateProcess()" by Andrew Tucker (July 1, 1999),
 * http://www.drdobbs.com/a-safer-alternative-to-terminateprocess/184416547
 *
 * If this method fails, we fall back to running terminate_process_tree().
 */
static int
exit_process(HANDLE process, int exit_code)
{
  DWORD code;

  if (GetExitCodeProcess (process, &code) && code == STILL_ACTIVE)
    {
      /*
       * We cannot determine the address of ExitProcess() for a process
       * that does not match the current architecture (e.g. for a 32-bit
       * process when we're running in 64-bit mode).
       */
      if (process_architecture_matches_current (process))
        {
          static LPTHREAD_START_ROUTINE exit_process_address;
          if (!exit_process_address)
            {
              HINSTANCE kernel32 = GetModuleHandle ("kernel32");
              exit_process_address = (LPTHREAD_START_ROUTINE)
                GetProcAddress (kernel32, "ExitProcess");
            }
          DWORD thread_id;
          HANDLE thread = !exit_process_address ? NULL :
            CreateRemoteThread (process, NULL, 0, exit_process_address,
                                (PVOID)exit_code, 0, &thread_id);

          if (thread)
            {
              CloseHandle (thread);
              /*
               * Wait 10 seconds (arbitrary constant) for the process to
               * finish; After that grace period, fall back to terminating
               * non-gently.
               */
              if (WaitForSingleObject (process, 10000) == WAIT_OBJECT_0)
                return 0;
            }
        }

      return terminate_process_tree (process, exit_code);
    }

  return -1;
}

#endif
