# Directory Synchronization Utility

The Directory Synchronization program synchronizes the file contents of directories on a personal computer running Microsoft Window.

The program compares file names and modification dates for a source directory and a target directory (and optionally any subdirectories).

- If a newer file is found in the source directory, the program replaces the matching file in the target directory. (Optionally, older source files may also replace newer target files.)

- If a file exists only in the source directory, the program copies the file to the target directory.

- The program will optionally delete any files or directories that exist only in the target directory.

- An option allows the synchronization to operate in both directions between the two directories.

- Prior to copying or deleting any files, the program prompts the user for final confirmation. Upon completion of any copies or deletions, the program displays a log window that describes all file actions that were performed. (The confirmation dialog and log window do not appear if the Automatic option is enabled and the NotifyUser option is disabled.)

- An option allows the program to automatically check for any file synchronization, or to wait for the user to complete a synchronization dialog. In this automatic mode, the program may also launch another application upon completion.

This program may be used for the following purposes:

- Program Copy Management: Automatically copy updated files from a shared file server directory to a directory on the computer's hard disk. The user's desktop hard disk is treated only as a local cache for the application programs. The file server contains the single centrally managed official copy of the application.

- File Backup: Copy revised files from a working directory to a backup directory on another disk or on a file server.

- Dual Computer File Coordination: Use an external USB drive (or diskette) as a transport mechanism to move data files, in both directions, between an office computer and a home computer. (_Note_: You cannot target a newly formatted diskette. At least one file or empty directory must be present on the diskette.)

The user may also design variations and combinations of these techniques.
