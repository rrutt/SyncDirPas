REM Prepare a downloadable ZIP file folder.
REM @echo on

REM Navigate to the folder containing this "bat shell" file.
pushd %~dp0
pause

rem robocopy ./LICENSE.txt ./SyncDirPas /MIR
copy .\src\SyncDirPas.exe .\SyncDirPas 
copy .\src\SyncDirPas.html .\SyncDirPas
copy .\src\SyncDir.ini .\SyncDirPas
robocopy .\img .\SyncDirPas\img

popd

echo "Done!"
pause