Attribute VB_Name = "modGlbl"
' (32-bit Visual Basic 6.0 version)

Option Explicit

Global Const gcProgName = "Directory Synchronization Utility"  ' Program Name

Global gsCurrDir As String   ' Current working directory
Global gsInitFile As String  ' Initialization File
Global gsInitDrv As String   ' Initialization file disk drive
Global gsInitSect As String  ' Initialization Section
Global gsNextSect As String  ' Next Initialization Section to load
Global gsOrigSect As String  ' Original Initialization Section

Global gbAutoSync As Boolean  ' Automatically Synchronize?
Global gbStopNext As Boolean  ' Stop looping to next configuration section?
Global gbNtfyUser As Boolean  ' Ask and Notify the User about copies and deletions?
Global gbShowErr As Boolean   ' Show MsgBox for file copy and delete errors?
Global gbMinLog As Boolean    ' Skip log messages for files that are skipped?
Global gsRunDir As String     ' Working Directory for gsRunCmd
Global gsRunCmd As String     ' Run Command string
Global gsAltRunDir As String  ' Working Directory for gsAltRunCmd
Global gsAltRunCmd As String  ' Alternate Run Command string (if files copied)
Global gbSyncBoth As Boolean  ' Run synchronization in both directions?
Global gbCopyOld As Boolean   ' Copy older source files?
Global gbDelXtra As Boolean   ' Delete extra target files?
Global gbDelDirs As Boolean   ' Delete extra target directories?
Global gbSyncSub As Boolean   ' Synchronize Subdirectories?
Global gbSkipDirs As Boolean  ' Skip missing directories?
Global gbProcHidn As Boolean  ' Process Hidden (and System) Files?
Global gbSkipReadOnlyTrgt As Boolean ' Skip target files that are read-only
Global gbUserCncl As Boolean  ' User Cancellation request

Global gsLog As String  ' Activity log report string

Global gnSync As Long  ' # files affected by the synchronization

Global gsCRLF As String  ' Carriage Return, Line Feed
Global gsLF As String    ' Line Feed


Declare Function GetPrivateProfileString Lib "kernel32" _
          Alias "GetPrivateProfileStringA" _
          (ByVal lpApplicationName As String, _
           lpKeyName As Any, _
           ByVal lpDefault As String, _
           ByVal lpReturnedString As String, _
           ByVal nSize As Long, _
           ByVal lpFileName As String) As Long

Function bFileExists(ByVal vsFileName As String) As Boolean
    Dim sTime As String

    On Error Resume Next  ' See if file exists
        sTime = FileDateTime(vsFileName)
        If Err.Number = 0 Then
            bFileExists = True
        Else
            bFileExists = False
        End If
    On Error GoTo 0
End Function

Sub ExecuteRunCommand(ByVal vnSync As Long)
    Dim hInsc As Long  ' Handle to program instance
    Dim sDir As String
    Dim sCmd As String
    
    If vnSync > 0 Then  ' Copied files, so use alternate command
        sDir = gsAltRunDir
        sCmd = gsAltRunCmd
    Else  ' Nothing copied, use normal command
        sDir = gsRunDir
        sCmd = gsRunCmd
    End If

    If sCmd <> "" Then
        On Error Resume Next
            SetWorkingDirectory sDir
            If Err.Number <> 0 Then
                Beep
                MsgBox "Could not set working directory:" & gsLF & sDir & gsLF & Err.Description, vbInformation, gcProgName & " [" & gsInitSect & "]"
            End If
        On Error GoTo 0

        On Error Resume Next
            hInsc = Shell(sCmd, 1) ' Run program normally
            If Err.Number <> 0 Then
                Beep
                MsgBox "Could not execute command:" & gsLF & sCmd & gsLF & Err.Description, vbInformation, gcProgName & " [" & gsInitSect & "]"
            End If
        On Error GoTo 0
    End If
End Sub

Sub SetWorkingDirectory(ByVal vsDir)
    Dim sDrv As String
    
    If Len(vsDir) > 0 Then  ' Set working directory
        If InStr(vsDir, ":") > 0 Then
            sDrv = Left$(vsDir, 1)
            ChDrive sDrv
        End If
        ChDir vsDir
    End If
End Sub

Sub InitializeLogText()
    Dim dCurrTime  ' Variant (date)

    dCurrTime = Now
    gsLog = gcProgName & " " & _
            LTrim$(Str$(App.Major)) & "." & _
            LTrim$(Str$(App.Minor)) & "." & _
            LTrim$(Str$(App.Revision)) & _
            " run on " & Format(dCurrTime, "d-mmm-yyyy") & _
            " at " & Format(dCurrTime, "h:nnam/pm")
End Sub

Sub Main()
    Dim sCmd As String
    Dim i As Long

    gsCRLF = Chr$(13) & Chr$(10)  ' Pseudo constant (carriage-return, line-feed)
    gsLF = Chr$(10)

    gsCurrDir = sEnsureBackslash(CurDir$)

    sCmd = Trim$(Command$)  ' Command line argument(s)
    If Left$(sCmd, 1) = """" Then  ' "Init File" InitSect
        i = InStr(2, sCmd, """")  ' Find closing quote
        If i = 0 Then  ' Missing closing quote
            Beep
            MsgBox "Missing closing quote on command line:" & vbLf & sCmd, vbInformation, gcProgName
            gsInitFile = ""
            gsInitSect = ""  ' Use default name
        Else
            gsInitFile = Mid$(sCmd, 2, (i - 2))
            gsInitSect = Trim$(Mid$(sCmd, (i + 1)))  ' Rest of string
        End If
    Else
        i = InStr(sCmd, " ") ' InitFile InitSect
        If i = 0 Then
            gsInitFile = sCmd
            gsInitSect = ""  ' Use default name
        Else
            gsInitFile = Left$(sCmd, (i - 1))
            gsInitSect = Trim$(Mid$(sCmd, (i + 1)))  ' Rest of string
        End If
    End If

    If gsInitFile = "" Then
        gsInitFile = gsCurrDir & "SyncDir.ini"
        gsInitSect = ""  ' Use default name
        If Not bFileExists(gsInitFile) Then
            gsInitFile = ""
            gsInitSect = ""
        End If
    Else
        If (InStr(gsInitFile, ":") = 0) _
        And (InStr(gsInitFile, "\") = 0) Then  ' Supply directory path
            gsInitFile = gsCurrDir & gsInitFile
        End If
        
        If Not bFileExists(gsInitFile) Then
            Beep
            MsgBox "Could not open initialization file [" & gsInitFile & "]", vbInformation, gcProgName
            gsInitFile = ""
            gsInitSect = ""
        End If
    End If
    
    If gsInitSect = "" Then
        gsInitSect = "SyncDir"  ' Default name
    End If
    
    gsOrigSect = gsInitSect
    
    gsInitDrv = Left$(gsCurrDir, 2)
    If InStr(gsInitFile, ":") = 2 Then
        gsInitDrv = Left$(gsInitFile, 2)
    End If

    gbAutoSync = False
    gbStopNext = False
    gbNtfyUser = True
    gbShowErr = True
    gbMinLog = False
    gbSyncBoth = False
    gbCopyOld = False
    gbDelXtra = False
    gbDelDirs = False
    gbSyncSub = False
    gbSkipDirs = False
    gbProcHidn = False
    gbSkipReadOnlyTrgt = False
    
    gsRunDir = ""
    gsRunCmd = ""
    gsAltRunDir = ""
    gsAltRunCmd = ""

    Load frmMain  ' Will finish reading initialization file
End Sub

Function nSynchronizeDirectories(ByVal vbCopyOld As Boolean, ByVal vbDelXtra As Boolean, ByVal vbDelDirs As Boolean, ByVal vsSrcDir As String, ByVal vsTrgtDir As String, ByVal vsIgnrType As String, ByVal vsProcType As String) As Long
    Dim nCopy As Long          ' # files copied
    Dim nDel As Long           ' # files deleted
    Dim nDelDir As Long        ' # directories deleted
    Dim nSkipReadOnly As Long  ' # read-only target files skipped
    Dim sMsg As String         ' MsgBox message string
    Dim sSrcSubMsg As String   ' Source subdirectory Message
    Dim sTrgtSubMsg As String  ' Target subdirectory Message
    Dim uRslt As Long          ' MsgBox result code
    Dim bSync As Boolean       ' User chose to synchronize?
    Dim sIgnrType As String    ' File types (extensions) to ignore
    Dim sProcType As String    ' File types (extensions) to process

    bSync = True

    sIgnrType = UCase$(vsIgnrType)
    If sIgnrType <> "" Then
        sIgnrType = "," & sIgnrType & ","  ' Allow searches for ",XXX," file type
    End If

    sProcType = UCase$(vsProcType)
    If sProcType <> "" Then
        sProcType = "," & sProcType & ","  ' Allow searches for ",XXX," file type
    End If

    If gbSyncSub Then
        sSrcSubMsg = " and its subdirectories"
        
        If gbSkipDirs Then
            sTrgtSubMsg = " and its matching subdirectories"
        Else
            sTrgtSubMsg = ""
        End If
    Else
        sSrcSubMsg = ""
        sTrgtSubMsg = ""
    End If

    ' Make a pass, without actually synchronizing
    Call SynchronizeFiles(False, vbCopyOld, vbDelXtra, vbDelDirs, vsSrcDir, vsTrgtDir, sIgnrType, sProcType, nCopy, nDel, nDelDir, nSkipReadOnly)
    If (nCopy > 0) Or (nDel > 0) Or (nDelDir > 0) Then
        sMsg = "In order to synchronize target directory [" & vsTrgtDir & "]" & sTrgtSubMsg & " with source directory [" & vsSrcDir & "]" & sSrcSubMsg & ", " & LTrim$(Str$(nCopy)) & " file(s) will be copied"
        If nDel > 0 Then
            sMsg = sMsg & " and " & LTrim$(Str$(nDel)) & " file(s) will be deleted"
        End If
        If nDelDir > 0 Then
            sMsg = sMsg & " and " & LTrim$(Str$(nDelDir)) & " directory(s) will be deleted"
        End If
        If nSkipReadOnly > 0 Then
            sMsg = sMsg & " and " & LTrim$(Str$(nSkipReadOnly)) & " read-only target file(s) will be skipped"
        End If
        sMsg = sMsg & "."

        If gbAutoSync And (Not gbNtfyUser) Then
            uRslt = vbOK
        Else
            uRslt = MsgBox(sMsg, (vbQuestion + vbOKCancel), gcProgName & " [" & gsInitSect & "]")
        End If
        If uRslt = vbOK Then
            gsLog = gsLog & gsCRLF & gsCRLF & sMsg
            
            DoEvents  ' Allow forms refresh
            ' Make a second pass, actually synchronizing this time
            Call SynchronizeFiles(True, vbCopyOld, vbDelXtra, vbDelDirs, vsSrcDir, vsTrgtDir, sIgnrType, sProcType, nCopy, nDel, nDelDir, nSkipReadOnly)
        Else
            bSync = False
            gbUserCncl = True
            gsLog = gsLog & gsCRLF & "*** Directory synchronization cancelled at user request"
        End If
    End If
    
    nSynchronizeDirectories = nCopy + nDel + nDelDir
    
    If nSynchronizeDirectories < 0 Then
        MsgBox "Directory """ & vsTrgtDir & """ can not be synchronized with directory """ & vsSrcDir & """", vbInformation, gcProgName & " [" & gsInitSect & "]"
    ElseIf Not bSync Then
        nSynchronizeDirectories = 0
    ElseIf (Not gbAutoSync) And (nCopy = 0) And (nDel = 0) And (nDelDir = 0) Then
        If nSkipReadOnly = 0 Then
            MsgBox "Directory [" & vsTrgtDir & "]" & sTrgtSubMsg & " already matched directory [" & vsSrcDir & "]" & sSrcSubMsg, vbInformation, gcProgName & " [" & gsInitSect & "]"
        Else
            MsgBox "Directory [" & vsTrgtDir & "]" & sTrgtSubMsg & " already matched directory [" & vsSrcDir & "]" & sSrcSubMsg & ", after skipping " & LTrim$(Str$(nSkipReadOnly)) & " read-only target file(s)", vbInformation, gcProgName & " [" & gsInitSect & "]"
        End If
    End If
    
    If bSync And (nSkipReadOnly > 0) Then
        gsLog = gsLog & gsCRLF & "  Skipped " & LTrim$(Str$(nSkipReadOnly)) & " read-only target file(s)"
    End If
End Function

Function sInitializationEntry(ByVal vsName As String, ByVal vsDflt As String, Optional ByVal vbUCase As Boolean = True) As String
    Dim sBuff As String   ' Character buffer for Windows API function call
    Dim nBuffLen As Long  ' Buffer length returned by Windows API function

    If gsInitFile = "" Then
        sInitializationEntry = vsDflt
    Else
        sBuff = Space$(255)  ' Pre-allocate the buffer
        nBuffLen = GetPrivateProfileString( _
                gsInitSect, ByVal vsName, vsDflt, _
                sBuff, Len(sBuff), gsInitFile)
        If vbUCase Then
            sInitializationEntry = UCase$(Left$(sBuff, nBuffLen))
        Else
            sInitializationEntry = Left$(sBuff, nBuffLen)
        End If
    End If
End Function

Sub SynchronizeFiles(ByVal vbExec As Boolean, ByVal vbCopyOld As Boolean, ByVal vbDelXtra As Boolean, ByVal vbDelDirs As Boolean, ByVal vsSrcDir As String, ByVal vsTrgtDir As String, ByVal vsIgnrType As String, ByVal vsProcType As String, rnCopy As Long, rnDel As Long, rnDelDir As Long, rnSkipReadOnly As Long)
    Dim sSrc As String    ' Local copy of Source directory name
    Dim sTrgt As String   ' Local copy of Target directory name
    Dim sFile As String   ' Current filename
    Dim sType As String   ' Current file's type (extension)
    Dim sSrcTime As String   ' Source file timestamp
    Dim sTrgtTime As String  ' Target file timestamp
    Dim sTrgtLogPrfx As String  ' Full path prefix for minimized log messages
    Dim bErr As Boolean  ' Error in processing
    Dim uRslt As Long
    Dim s As String
    Dim uFileAttr As Long

    bErr = False
    
    Screen.MousePointer = vbHourglass
    
    sSrc = sEnsureBackslash(vsSrcDir)
    sTrgt = sEnsureBackslash(vsTrgtDir)
    
    If gbMinLog Then
        sTrgtLogPrfx = sTrgt
    Else
        sTrgtLogPrfx = ""
    End If

    rnCopy = 0
    rnDel = 0
    rnDelDir = 0
    rnSkipReadOnly = 0
    
    If vbExec And (Not gbMinLog) Then  ' This is the actual execution pass
        gsLog = gsLog & gsCRLF & gsCRLF & "Synchronizing files:" & gsCRLF & "  Source: """ & sSrc & "*.*""" & gsCRLF & "  Target: """ & sTrgt & "*.*"""
    
        If vbCopyOld Then
            gsLog = gsLog & gsCRLF & gsCRLF & "Copying different source files ..."
        Else
            gsLog = gsLog & gsCRLF & gsCRLF & "Copying newer source files ..."
        End If
    End If

    On Error Resume Next
        s = Dir$(sTrgt, vbDirectory)  ' Test for existence
        If (s = "") Or (Err.Number <> 0) Then
            Beep
            MsgBox "Not a valid target directory: """ & sTrgt & """", vbExclamation, gcProgName & " [" & gsInitSect & "]"
            bErr = True
        End If
    On Error GoTo 0

    On Error Resume Next
        If gbProcHidn Then  ' Include system and hidden files
            uFileAttr = vbNormal Or vbHidden Or vbSystem
        Else
            uFileAttr = vbNormal
        End If
        sFile = Dir$(sSrc & "*.*", uFileAttr)  ' This also primes the While loop call to Dir$
        If Err.Number <> 0 Then
            Beep
            MsgBox "Not a valid source directory: """ & sSrc & """", vbExclamation, gcProgName & " [" & gsInitSect & "]"
            bErr = True
        End If
    On Error GoTo 0
    
    While (Not bErr) And (Not gbUserCncl) And (sFile <> "")
        sType = sExtractFileTypeFromFileName(sFile)  ' Get file type (without ".")
        If (vsProcType <> "") And (InStr(vsProcType, ("," & sType & ",")) = 0) Then
            If vbExec And (Not gbMinLog) Then
                gsLog = gsLog & gsCRLF & "  (Did not process file """ & sFile & """)"
            End If
        ElseIf (vsIgnrType <> "") And (InStr(vsIgnrType, ("," & sType & ",")) > 0) Then
            If vbExec And (Not gbMinLog) Then
                gsLog = gsLog & gsCRLF & "  (Ignored file """ & sFile & """)"
            End If
        Else
            On Error Resume Next
                sSrcTime = Format(FileDateTime(sSrc & sFile), "yyyymmdd.hhnnss")
                If Err.Number <> 0 Then
                    sSrcTime = "INVALID"
                End If
            On Error GoTo 0
    
            On Error Resume Next
                sTrgtTime = Format(FileDateTime(sTrgt & sFile), "yyyymmdd.hhnnss")
                If Err.Number <> 0 Then
                    sTrgtTime = ""
                End If
            On Error GoTo 0
            
            If sSrcTime = "INVALID" Then
                If vbExec Then
                    gsLog = gsLog & gsCRLF & "  (""" & sFile & """ has an invalid filename)"
                End If
            ElseIf vbCopyOld And (sSrcTime = sTrgtTime) Then
                If vbExec And (Not gbMinLog) Then
                    gsLog = gsLog & gsCRLF & "  (""" & sFile & """ matched the source file)"
                End If
            ElseIf (Not vbCopyOld) And (sSrcTime <= sTrgtTime) Then
                If vbExec And (Not gbMinLog) Then
                    gsLog = gsLog & gsCRLF & "  (""" & sFile & """ was up to date)"
                End If
            Else
                On Error Resume Next
                    uRslt = GetAttr(sTrgt & sFile)
                    If Err.Number <> 0 Then
                        uRslt = 0
                    End If
                On Error GoTo 0
                If gbSkipReadOnlyTrgt And ((uRslt And vbReadOnly) <> 0) Then
                    rnSkipReadOnly = rnSkipReadOnly + 1
                    If vbExec Then
                        gsLog = gsLog & gsCRLF & "  (""" & sFile & """ skipped since the target is read-only)"
                    End If
                Else
                    rnCopy = rnCopy + 1
                    If vbExec Then
                        On Error Resume Next
                            FileCopy (sSrc & sFile), (sTrgt & sFile)
                            If Err.Number = 0 Then
                                gsLog = gsLog & gsCRLF & "  Copied file """ & sTrgtLogPrfx & sFile & """"
                            Else
                                If gbShowErr Then
                                    Beep
                                    uRslt = MsgBox("From " & sSrc & vbLf & "To " & sTrgt & vbLf & vbLf & Err.Description, (vbExclamation + vbOKCancel), ("Error copying file """ & sFile & """"))
                                Else
                                    uRslt = vbOK
                                End If
                                gsLog = gsLog & gsCRLF & "  *** Error copying file """ & sTrgt & sFile & """" & gsCRLF & "      " & Err.Description
                                If uRslt = vbCancel Then
                                    gbUserCncl = True
                                    gsLog = gsLog & gsCRLF & "*** Directory synchronization cancelled at user request"
                                End If
                            End If
                        On Error GoTo 0
                    End If
                End If
            End If  ' Else copied file
        End If ' Else not ignoring file
        
        sFile = Dir$  ' Get next matching file

        DoEvents  ' Be a good Windows citizen
        Screen.MousePointer = vbHourglass
    Wend
    
    If (Not bErr) And (Not gbUserCncl) And vbDelXtra Then
        If vbExec And (Not gbMinLog) Then
            gsLog = gsLog & gsCRLF & gsCRLF & "Deleting extra target files ..."
        End If

        On Error Resume Next
            If gbProcHidn Then  ' Include system and hidden files
                uFileAttr = vbNormal Or vbHidden Or vbSystem
            Else
                uFileAttr = vbNormal
            End If
            sFile = Dir$(sTrgt & "*.*", uFileAttr)
            If Err.Number <> 0 Then
                Beep
                MsgBox "Not a valid target directory: """ & sTrgt & """", vbExclamation, gcProgName & " [" & gsInitSect & "]"
                bErr = True
            End If
        On Error GoTo 0
        
        While (Not bErr) And (Not gbUserCncl) And (sFile <> "")
            sType = UCase$(Mid$(sFile, (1 + InStr(sFile, "."))))  ' Get file type (without ".")
            sTrgtTime = Format(FileDateTime(sTrgt & sFile), "yyyymmdd.hhnnss")
    
            On Error Resume Next
                sSrcTime = Format(FileDateTime(sSrc & sFile), "yyyymmdd.hhnnss")
                If Err.Number <> 0 Then
                    sSrcTime = ""
                End If
            On Error GoTo 0
            
            If sSrcTime = "" Then
                If (vsProcType <> "") And (InStr(vsProcType, ("," & sType & ",")) = 0) Then
                    If vbExec And (Not gbMinLog) Then
                        gsLog = gsLog & gsCRLF & "  (Did not process extra file """ & sFile & """)"
                    End If
                ElseIf (vsIgnrType <> "") And (InStr(vsIgnrType, ("," & sType & ",")) > 0) Then
                    If vbExec And (Not gbMinLog) Then
                        gsLog = gsLog & gsCRLF & "  (Ignored extra file """ & sFile & """)"
                    End If
                Else
                    rnDel = rnDel + 1
                    If vbExec Then
                        On Error Resume Next
                            SetAttr (sTrgt & sFile), vbNormal  ' Make sure to clear any special attributes
                            Kill (sTrgt & sFile)
                            If Err.Number = 0 Then
                                gsLog = gsLog & gsCRLF & "  Deleted file """ & sTrgtLogPrfx & sFile & """"
                            Else
                                If gbShowErr Then
                                    Beep
                                    uRslt = MsgBox("From " & sTrgt & vbLf & vbLf & Err.Description, (vbExclamation + vbOKCancel), ("Error deleting file """ & sFile & """"))
                                Else
                                    uRslt = vbOK
                                End If
                                gsLog = gsLog & gsCRLF & "  *** Error deleting file """ & sTrgt & sFile & """" & gsCRLF & "      " & Err.Description
                                If uRslt = vbCancel Then
                                    gbUserCncl = True
                                    gsLog = gsLog & gsCRLF & "*** Directory synchronization cancelled at user request"
                                End If
                            End If
                        On Error GoTo 0
                    End If
                End If
            End If  ' Else not an ignored file type
            
            sFile = Dir$  ' Get next matching file

            DoEvents  ' Be a good Windows citizen
            Screen.MousePointer = vbHourglass
        Wend
    End If

    If (Not bErr) And gbSyncSub Then
        Call SynchronizeSubdirectories(vbExec, vbCopyOld, vbDelXtra, vbDelDirs, sSrc, sTrgt, vsIgnrType, vsProcType, rnCopy, rnDel, rnDelDir, rnSkipReadOnly)
    End If
    
    If (Not bErr) And (Not gbUserCncl) And vbDelDirs Then
        Call DeleteExtraDirectories(vbExec, vbDelXtra, sSrc, sTrgt, rnDel, rnDelDir)
    End If

    Screen.MousePointer = vbNormal
    
    If bErr Then
        rnCopy = -1
        rnDel = -1
        rnDelDir = -1
        rnSkipReadOnly = -1
    End If
End Sub

Sub SynchronizeSubdirectories(ByVal vbExec As Boolean, ByVal vbCopyOld As Boolean, ByVal vbDelXtra As Boolean, ByVal vbDelDirs As Boolean, ByVal vsSrcDir As String, ByVal vsTrgtDir As String, ByVal vsIgnrType As String, ByVal vsProcType As String, rnCopy As Long, rnDel As Long, rnDelDir As Long, rnSkipReadOnly As Long)
    Dim nCopy As Long          ' # files copied
    Dim nDel As Long           ' # files deleted
    Dim nDelDir As Long        ' # directories deleted
    Dim nSkipReadOnly As Long  ' # read-only target files skipped
    Dim sSubList As String     ' Subdirectory List
    Dim sSubDir As String      ' Sub-Directory
    Dim sSrcSub As String      ' Source Subdirectory
    Dim sTrgtSub As String     ' Target Subdirectory
    Dim uRslt As Long
    Dim bErr As Boolean
    Dim s As String
    Dim i As Long
    Dim n1 As Long
    Dim n2 As Long

    bErr = False
    
    n1 = Len(vsSrcDir)
    n2 = Len(vsTrgtDir)
    If n1 < n2 Then
        If UCase$(vsSrcDir) = UCase$(Left$(vsTrgtDir, n1)) Then
            bErr = True
            If (Not vbExec) Then  ' Only display message on first pass
                Beep
                MsgBox "The target directory """ & vsTrgtDir & """ is a subdirectory of the source directory """ & vsSrcDir & """, source subdirectories will not be processed", vbExclamation, gcProgName & " [" & gsInitSect & "]"
                gsLog = gsLog & gsCRLF & gsCRLF & "*** The target directory """ & vsTrgtDir & """" & gsCRLF & "    is a subdirectory of the source directory """ & vsSrcDir & """," & gsCRLF & "    source subdirectories will not be processed"
            End If
        End If
    End If

    On Error Resume Next
        s = Dir$(vsSrcDir, vbDirectory)  ' Test for existence
        If (s = "") Or (Err.Number <> 0) Then
            Beep
            MsgBox "Not a valid source (sub)directory: """ & vsSrcDir & """", vbExclamation, gcProgName & " [" & gsInitSect & "]"
            bErr = True
        End If
    On Error GoTo 0
    
    sSubList = SubdirectoryList(vbExec, vsSrcDir)  ' Build subdirectory list before actually processing it

    While (Not bErr) And (Not gbUserCncl) And (sSubList <> "")
        i = InStr(sSubList, "|")
        If i = 0 Then
            sSubDir = sSubList
            sSubList = ""
        Else
            sSubDir = Left$(sSubList, (i - 1))
            sSubList = Mid$(sSubList, (i + 1))  ' Rest of string
        End If
    
        sSrcSub = sEnsureBackslash(vsSrcDir) & sSubDir
        sTrgtSub = sEnsureBackslash(vsTrgtDir) & sSubDir
    
        bErr = False
        
        On Error Resume Next
            s = Dir$(sTrgtSub, vbDirectory)  ' Test for existence
            If (s = "") Or (Err.Number <> 0) Then
                ' Does not exist.
                bErr = True
                If vbExec Then
                    ' We created new subdirectories on first pass.
                    ' If the directory is still missing now,
                    ' then we had a problem on the first pass,
                    ' or we are skipping missing target directories.
                    If Not gbMinLog Then
                        gsLog = gsLog & gsCRLF & gsCRLF & "Skipping missing target subdirectory:" & gsCRLF & "  """ & sTrgtSub & """"
                    End If
                ElseIf Not gbSkipDirs Then
                    MkDir sTrgtSub
                    If Err.Number = 0 Then
                        bErr = False
                        If gbMinLog Then
                            gsLog = gsLog & gsCRLF & "  "
                        Else
                            gsLog = gsLog & gsCRLF & gsCRLF
                        End If
                        gsLog = gsLog & "Created subdirectory:" & gsCRLF & "  """ & sTrgtSub & """"
                    Else
                        If gbShowErr Then
                            Beep
                            uRslt = MsgBox("Within target directory """ & vsTrgtDir & """" & vbLf & vbLf & Err.Description, (vbExclamation + vbOKCancel), ("Could not create target subdirectory: """ & sSubDir & """"))
                        Else
                            uRslt = vbOK
                        End If
                        gsLog = gsLog & gsCRLF & gsCRLF & "*** Could not create target subdirectory:" & gsCRLF & "  """ & sTrgtSub & """" & gsCRLF & "   " & Err.Description
                        If uRslt = vbCancel Then
                            gbUserCncl = True
                            gsLog = gsLog & gsCRLF & "*** Directory synchronization cancelled at user request"
                        End If
                    End If
                End If
            End If
        On Error GoTo 0
    
        If bErr Then
            nCopy = 0
            nDel = 0
            nDelDir = 0
            nSkipReadOnly = 0
            bErr = False  ' Allow While loop to continue
        Else
            Call SynchronizeFiles(vbExec, vbCopyOld, vbDelXtra, vbDelDirs, sSrcSub, sTrgtSub, vsIgnrType, vsProcType, nCopy, nDel, nDelDir, nSkipReadOnly)
            rnCopy = rnCopy + nCopy
            rnDel = rnDel + nDel
            rnDelDir = rnDelDir + nDelDir
            rnSkipReadOnly = rnSkipReadOnly + nSkipReadOnly
        End If
        
        If gbUserCncl Or (nCopy < 0) Or (nDel < 0) Or (nDelDir < 0) Then  ' Error
            rnCopy = -1
            rnDel = -1
            rnDelDir = -1
            rnSkipReadOnly = -1
            Exit Sub
        End If

        DoEvents  ' Be a good Windows citizen
        Screen.MousePointer = vbHourglass
    Wend
End Sub

Sub DeleteExtraDirectories(ByVal vbExec As Boolean, ByVal vbDelFiles As Boolean, ByVal vsSrcDir As String, ByVal vsTrgtDir As String, rnDel As Long, rnDelDir As Long)
    Dim sSubList As String  ' Subdirectory List
    Dim sSrcSub As String   ' Source Subdirectory
    Dim sTrgtSub As String  ' Target Subdirectory
    Dim s As String
    Dim i As Long
    
    If vbExec Then
        If Not gbMinLog Then
            If vbDelFiles Then
                gsLog = gsLog & gsCRLF & gsCRLF & "Deleting extra target subdirectories (and files) within " & gsCRLF & "  """ & vsTrgtDir & """ ..."
            Else
                gsLog = gsLog & gsCRLF & gsCRLF & "Deleting extra empty target subdirectories within " & gsCRLF & "  """ & vsTrgtDir & """ ..."
            End If
        End If
    End If
    
    sSubList = SubdirectoryList(vbExec, vsTrgtDir)  ' Build subdirectory list before actually processing it

    Do While (Not gbUserCncl) And (sSubList <> "")
        i = InStr(sSubList, "|")
        If i = 0 Then
            s = sSubList
            sSubList = ""
        Else
            s = Left$(sSubList, (i - 1))
            sSubList = Mid$(sSubList, (i + 1))  ' Rest of string
        End If
    
        sSrcSub = sEnsureBackslash(vsSrcDir) & s
        sTrgtSub = sEnsureBackslash(vsTrgtDir) & s
        
        On Error Resume Next
            s = Dir$(sSrcSub, vbDirectory)  ' Test for existence
            If (s = "") Or (Err.Number <> 0) Then
                Call DeleteDirectory(vbExec, vbDelFiles, sTrgtSub, rnDel, rnDelDir)
            End If
        On Error GoTo 0
        
        If gbUserCncl Then  ' Error
            rnDel = -1
            rnDelDir = -1
            Exit Sub
        End If

        DoEvents  ' Be a good Windows citizen
        Screen.MousePointer = vbHourglass
    Loop
End Sub

Sub DeleteDirectory(ByVal vbExec As Boolean, ByVal vbDelFiles As Boolean, ByVal vsDir As String, rnDel As Long, rnDelDir As Long)
    Dim sSubList As String  ' Subdirectory List
    Dim sSubDir As String   ' Subdirectory
    Dim sErrDesc As String  ' Error Description
    Dim uRslt As Long
    Dim s As String
    Dim i As Long
    
    sSubList = SubdirectoryList(vbExec, vsDir)  ' Build subdirectory list before actually processing it

    Do While (Not gbUserCncl) And (sSubList <> "")
        i = InStr(sSubList, "|")
        If i = 0 Then
            s = sSubList
            sSubList = ""
        Else
            s = Left$(sSubList, (i - 1))
            sSubList = Mid$(sSubList, (i + 1))  ' Rest of string
        End If
    
        sSubDir = sEnsureBackslash(vsDir) & s
        
        ' Recursively delete sub-subdirectory:
        Call DeleteDirectory(vbExec, vbDelFiles, sSubDir, rnDel, rnDelDir)
        
        If gbUserCncl Then  ' Error
            rnDel = -1
            rnDelDir = -1
            Exit Sub
        End If

        DoEvents  ' Be a good Windows citizen
        Screen.MousePointer = vbHourglass
    Loop
    
    If vbDelFiles Then
        Call DeleteFilesFromDirectory(vbExec, vsDir, rnDel)
    End If

    rnDelDir = rnDelDir + 1
    If vbExec Then
        On Error Resume Next
            RmDir vsDir
            If Err.Number = 0 Then
                gsLog = gsLog & gsCRLF & "  Deleted extra directory """ & vsDir & """"
            Else
                If Err.Number = 75 Then  ' Path/file access error
                    sErrDesc = "Directory contains files"
                Else
                    sErrDesc = Err.Description
                End If
                
                If gbShowErr Then
                    Beep
                    uRslt = MsgBox("""" & vsDir & """" & vbLf & vbLf & sErrDesc, (vbExclamation + vbOKCancel), ("Error deleting directory"))
                Else
                    uRslt = vbOK
                End If
                gsLog = gsLog & gsCRLF & "  *** Error deleting directory """ & vsDir & """" & gsCRLF & "      " & sErrDesc
                If uRslt = vbCancel Then
                    gbUserCncl = True
                    gsLog = gsLog & gsCRLF & "*** Directory synchronization cancelled at user request"
                End If
            End If
        On Error GoTo 0
    End If
End Sub

Sub DeleteFilesFromDirectory(ByVal vbExec As Boolean, ByVal vsDir As String, rnDel As Long)
    Dim sDir As String
    Dim sFile As String
    Dim uFileAttr As Long
    Dim uRslt As Long
    Dim bErr As Boolean
    
    bErr = False
    
    sDir = sEnsureBackslash(vsDir)

    On Error Resume Next
        uFileAttr = vbNormal Or vbHidden Or vbSystem
        sFile = Dir$(sDir & "*.*", uFileAttr)  ' This also primes the While loop call to Dir$
        If Err.Number <> 0 Then
            Beep
            MsgBox "Not a valid directory: """ & vsDir & """", vbExclamation, gcProgName & " [" & gsInitSect & "]"
            bErr = True
        End If
    On Error GoTo 0
    
    Do While (Not bErr) And (Not gbUserCncl) And (sFile <> "")
        rnDel = rnDel + 1
        If vbExec Then
            On Error Resume Next
                SetAttr (sDir & sFile), vbNormal  ' Make sure to clear any special attributes
                Kill (sDir & sFile)
                If Err.Number = 0 Then
                    gsLog = gsLog & gsCRLF & "  Deleted file """ & sFile & """" & gsCRLF & "    from extra directory """ & vsDir & """"
                Else
                    If gbShowErr Then
                        Beep
                        uRslt = MsgBox("From extra directory " & vsDir & vbLf & vbLf & Err.Description, (vbExclamation + vbOKCancel), ("Error deleting file """ & sFile & """"))
                    Else
                        uRslt = vbOK
                    End If
                    gsLog = gsLog & gsCRLF & "  *** Error deleting file """ & sFile & """ from extra directory """ & vsDir & """" & gsCRLF & "      " & Err.Description
                    If uRslt = vbCancel Then
                        gbUserCncl = True
                        gsLog = gsLog & gsCRLF & "*** Directory synchronization cancelled at user request"
                    End If
                End If
            On Error GoTo 0
        End If
        
        sFile = Dir$  ' Get next matching file
        
        DoEvents  ' Be a good Windows citizen
        Screen.MousePointer = vbHourglass
    Loop
End Sub

Function SubdirectoryList(ByVal vbExec As Boolean, ByVal vsDir As String) As String
    ' Build subdirectory list using "|" as delimiter.
    Dim sDir As String
    Dim sSubList As String
    Dim s As String
    Dim uRslt As Long
    
    sDir = sEnsureBackslash(vsDir)
    
    sSubList = ""
    s = Dir$(sDir & "*.*", vbDirectory)
    Do Until s = ""
        If (s <> ".") And (s <> "..") Then
            On Error Resume Next
                uRslt = GetAttr(sDir & s)
                If Err.Number <> 0 Then
                    uRslt = 0
                    If vbExec Then
                        gsLog = gsLog & gsCRLF & "  *** Skipping invalid directory name """ & sDir & s & """"
                    End If
                End If
            On Error GoTo 0
            If (uRslt And vbDirectory) <> 0 Then
                If sSubList = "" Then
                    sSubList = s
                Else
                    sSubList = sSubList & "|" & s
                End If
            End If
        End If
        s = Dir$  ' Get next value in current search
    Loop
    
    SubdirectoryList = sSubList
End Function

Function sExtractFileTypeFromFileName(ByVal vsFileName As String) As String
    ' Get file type (without "."), using last "." as the delimiter.
    
    Dim sType As String
    Dim iPos As Long
    Dim iNext As Long
    
    iPos = 0
    iNext = InStr(vsFileName, ".")  ' Start searching at 1st character
    Do While iNext > 0
        iPos = iNext + 1
        iNext = InStr(iPos, vsFileName, ".")
    Loop
    
    If iPos = 0 Then
        sType = "."
    Else
        sType = UCase$(Mid$(vsFileName, iPos))  ' Rest of string
    End If
    
    sExtractFileTypeFromFileName = sType
End Function

Function sEnsureBackslash(ByVal vsDir As String) As String
    Dim sDir As String
    
    sDir = vsDir
    If Right$(sDir, 1) <> "\" Then
        sDir = sDir & "\"
    End If
    
    sEnsureBackslash = sDir
End Function
