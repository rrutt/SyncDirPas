VERSION 5.00
Begin VB.Form frmMain 
   Caption         =   "Synchronize Directories"
   ClientHeight    =   6240
   ClientLeft      =   2265
   ClientTop       =   1455
   ClientWidth     =   9480
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H80000008&
   HelpContextID   =   1
   Icon            =   "SD32Main.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6240
   ScaleWidth      =   9480
   StartUpPosition =   3  'Windows Default
   Begin VB.CheckBox chkSkipReadOnlyTrgt 
      Caption         =   "Skip read-only target files"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      HelpContextID   =   214
      Left            =   4800
      TabIndex        =   17
      Top             =   5400
      Width           =   4575
   End
   Begin VB.TextBox txtProcType 
      Height          =   285
      HelpContextID   =   210
      Left            =   4800
      TabIndex        =   7
      Text            =   "txtProcType"
      Top             =   3600
      Width           =   4575
   End
   Begin VB.CheckBox chkDelDirs 
      Caption         =   " Delete extra directories"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      HelpContextID   =   213
      Left            =   4800
      TabIndex        =   15
      Top             =   4680
      Width           =   4575
   End
   Begin VB.CheckBox chkMinlog 
      Caption         =   " Minimize log messages"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      HelpContextID   =   211
      Left            =   120
      TabIndex        =   12
      Top             =   5400
      Width           =   4575
   End
   Begin VB.CheckBox chkSkipDirs 
      Caption         =   " Skip missing directories"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      HelpContextID   =   212
      Left            =   4800
      TabIndex        =   14
      Top             =   4320
      Width           =   4575
   End
   Begin VB.CheckBox chkProcHidn 
      Caption         =   " Process hidden files"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      HelpContextID   =   209
      Left            =   120
      TabIndex        =   9
      Top             =   4320
      Width           =   4575
   End
   Begin VB.CheckBox chkShowErr 
      Caption         =   " Show error messages"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      HelpContextID   =   208
      Left            =   120
      TabIndex        =   11
      Top             =   5040
      Width           =   4575
   End
   Begin VB.CheckBox chkSyncSub 
      Caption         =   " Include subdirectories"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      HelpContextID   =   207
      Left            =   4800
      TabIndex        =   13
      Top             =   3960
      Width           =   4575
   End
   Begin VB.TextBox txtTrgtDir 
      Height          =   615
      HelpContextID   =   202
      Left            =   4800
      MultiLine       =   -1  'True
      ScrollBars      =   1  'Horizontal
      TabIndex        =   5
      Text            =   "SD32Main.frx":030A
      Top             =   360
      Width           =   4575
   End
   Begin VB.TextBox txtSrcDir 
      Height          =   615
      HelpContextID   =   201
      Left            =   120
      MultiLine       =   -1  'True
      ScrollBars      =   1  'Horizontal
      TabIndex        =   2
      Text            =   "SD32Main.frx":0317
      Top             =   360
      Width           =   4575
   End
   Begin VB.TextBox txtIgnrType 
      Height          =   285
      HelpContextID   =   206
      Left            =   4800
      TabIndex        =   6
      Text            =   "txtIgnrType"
      Top             =   3240
      Width           =   4575
   End
   Begin VB.CheckBox chkCopyOld 
      Caption         =   " Copy older files"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      HelpContextID   =   203
      Left            =   120
      TabIndex        =   8
      Top             =   3960
      Width           =   4575
   End
   Begin VB.CommandButton btnHelp 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Help"
      Height          =   375
      Left            =   5040
      TabIndex        =   18
      Top             =   5760
      Width           =   855
   End
   Begin VB.CheckBox chkSyncBoth 
      Caption         =   " Synchronize both ways"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      HelpContextID   =   204
      Left            =   120
      TabIndex        =   10
      Top             =   4680
      Width           =   4575
   End
   Begin VB.CheckBox chkDelXtra 
      Caption         =   " Delete extra files"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      HelpContextID   =   205
      Left            =   4800
      TabIndex        =   16
      Top             =   5040
      Width           =   4575
   End
   Begin VB.CommandButton btnExit 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Cancel          =   -1  'True
      Caption         =   "Exit"
      Height          =   375
      Left            =   8520
      TabIndex        =   20
      Top             =   5760
      Width           =   855
   End
   Begin VB.CommandButton btnSync 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "Synchronize"
      Default         =   -1  'True
      Height          =   375
      Left            =   6480
      TabIndex        =   19
      Top             =   5760
      Width           =   1455
   End
   Begin VB.DirListBox dirTrgt 
      Height          =   1665
      HelpContextID   =   202
      Left            =   4800
      TabIndex        =   3
      Top             =   1080
      Width           =   4575
   End
   Begin VB.DirListBox dirSrc 
      Height          =   1665
      HelpContextID   =   201
      Left            =   120
      TabIndex        =   0
      Top             =   1080
      Width           =   4575
   End
   Begin VB.DriveListBox drvTrgt 
      Height          =   315
      HelpContextID   =   202
      Left            =   4800
      TabIndex        =   4
      Top             =   2880
      Width           =   4575
   End
   Begin VB.DriveListBox drvSrc 
      Height          =   315
      HelpContextID   =   201
      Left            =   120
      TabIndex        =   1
      Top             =   2880
      Width           =   4575
   End
   Begin VB.Label lblNextSectLbl 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Next section:"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   120
      TabIndex        =   28
      Top             =   6000
      Width           =   2295
   End
   Begin VB.Label lblNextSect 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "lblNextSect"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   2520
      TabIndex        =   27
      Top             =   6000
      Width           =   2295
   End
   Begin VB.Label lblInitSect 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "lblInitSect"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   2520
      TabIndex        =   26
      Top             =   5760
      Width           =   2295
   End
   Begin VB.Label Label5 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Initialization file section:"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   120
      TabIndex        =   25
      Top             =   5760
      Width           =   2295
   End
   Begin VB.Label Label4 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Only Process File Types:"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   120
      TabIndex        =   24
      Top             =   3600
      Width           =   4575
   End
   Begin VB.Label Label3 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Ignore File Types:"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   120
      TabIndex        =   23
      Top             =   3240
      Width           =   4575
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Target Directory:"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   4800
      TabIndex        =   22
      Top             =   120
      Width           =   4575
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      Caption         =   "Source Directory:"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   120
      TabIndex        =   21
      Top             =   120
      Width           =   4575
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' (32-bit Visual Basic 6.0 version)

Option Explicit

Sub ValidateSourceDirectory()
    Dim sDir As String
    Dim bErr As Boolean
    Dim s As String
    
    bErr = False
    sDir = txtSrcDir.Text
        
    If sDir = "" Then
        sDir = gsCurrDir
        txtSrcDir.Text = sDir
    End If
    
    sDir = sEnsureBackslash(sDir)
        
    On Error Resume Next
        ' Make sure any new setting in a chained loop is interpreted relative
        ' to the program's current working directory.
        dirSrc.Path = gsCurrDir
        dirSrc.Path = sDir
        If Err.Number <> 0 Then
            bErr = True
        End If
    On Error GoTo 0
    
    If bErr Then
        Beep
        MsgBox "Not a valid source directory: """ & sDir & """", vbExclamation, gcProgName & " [" & gsInitSect & "]"
        gbAutoSync = False  ' Make sure the form appears to the user
        gbStopNext = True  ' Stop looping to any next configuration section
        
        dirSrc.BackColor = frmMain.BackColor
        drvSrc.BackColor = frmMain.BackColor
    
    ElseIf Left$(sDir, 2) = "\\" Then  ' Universal Naming Convention
        drvSrc.BackColor = frmMain.BackColor
    
    Else
        drvSrc.Enabled = False  ' Signal to avoid recursive change events
        drvSrc.Drive = Left$(dirSrc.Path, 1)
        drvSrc.Enabled = True
        
        dirSrc.BackColor = txtSrcDir.BackColor
        drvSrc.BackColor = txtSrcDir.BackColor
    End If
End Sub

Sub ValidateTargetDirectory()
    Dim sDir As String
    Dim bErr As Boolean
    Dim s As String
    
    bErr = False
    sDir = txtTrgtDir.Text
        
    If sDir = "" Then
        sDir = gsCurrDir
        txtTrgtDir.Text = sDir
    End If
    
    sDir = sEnsureBackslash(sDir)
        
    On Error Resume Next
        ' Make sure any new setting in a chained loop is interpreted relative
        ' to the program's current working directory.
        dirTrgt.Path = gsCurrDir
        dirTrgt.Path = sDir
        If Err.Number <> 0 Then
            bErr = True
        End If
    On Error GoTo 0
    
    If bErr Then
        Beep
        MsgBox "Not a valid target directory: """ & sDir & """", vbExclamation, gcProgName & " [" & gsInitSect & "]"
        gbAutoSync = False  ' Make sure the form appears to the user
        gbStopNext = True  ' Stop looping to any next configuration section
        
        dirTrgt.BackColor = frmMain.BackColor
        drvTrgt.BackColor = frmMain.BackColor
    
    ElseIf Left$(sDir, 2) = "\\" Then  ' Universal Naming Convention
        drvTrgt.BackColor = frmMain.BackColor
    
    Else
        drvTrgt.Enabled = False  ' Signal to avoid recursive change events
        drvTrgt.Drive = Left$(dirTrgt.Path, 1)
        drvTrgt.Enabled = True
        
        dirTrgt.BackColor = txtTrgtDir.BackColor
        drvTrgt.BackColor = txtTrgtDir.BackColor
    End If
End Sub

Private Sub btnExit_Click()
    End  ' Stop program execution
End Sub

Private Sub btnHelp_Click()
    SendKeys "{F1}"
End Sub

Private Sub btnSync_Click()
    Dim sSrcDir As String    ' Source Directory
    Dim sTrgtDir As String   ' Target Directory
    Dim sSectList As String  ' Section List
    Dim bSyncMore As Boolean
    Dim bShowLog As Boolean
    
    gnSync = 0  ' # files affected
    btnSync.Enabled = False
    gbUserCncl = False

    Call InitializeLogText
    
    sSectList = ""
    bShowLog = False
    Do
        If InStr(UCase$(sSectList), "[" & UCase$(gsInitSect) & "]") > 0 Then
            Beep
            MsgBox "The initialization file section [" & gsInitSect & "] has already been processed:" & gsLF & sSectList, vbInformation, gcProgName & " [" & gsInitSect & "]"
            Exit Do
        Else
            sSectList = sSectList & "[" & gsInitSect & "] "
        End If
        
        sSrcDir = txtSrcDir.Text
        sTrgtDir = txtTrgtDir.Text
        
        gsLog = gsLog & gsCRLF & gsCRLF & "Processing initialization file section [" & gsInitSect & "]"
    
        If UCase$(sSrcDir) = UCase$(sTrgtDir) Then
            Beep
            MsgBox "The source directory and target directory are the same: """ & sSrcDir & """", vbInformation, gcProgName & " [" & gsInitSect & "]"
        Else
            gnSync = nSynchronizeDirectories(gbCopyOld, gbDelXtra, gbDelDirs, sSrcDir, sTrgtDir, txtIgnrType, txtProcType)
            
            If gbSyncBoth Then  ' Sync. in opposite direction
                gnSync = gnSync + nSynchronizeDirectories(gbCopyOld, gbDelXtra, gbDelDirs, sTrgtDir, sSrcDir, txtIgnrType, txtProcType)
            End If
        End If
        
        If gbUserCncl Then
            bShowLog = True
            bSyncMore = False
        Else
            If (gbNtfyUser Or (Not gbAutoSync)) And (gnSync > 0) Then
                bShowLog = True
            End If
            
            If gbAutoSync Then
                Call ExecuteRunCommand(gnSync)
            End If
        
            If Len(gsNextSect) > 0 Then
                ' Automatically chain to the next section.
                gsInitSect = gsNextSect
                gsNextSect = ""
                LoadInitializationSettings
                bSyncMore = Not gbStopNext
            Else
                ' Prepare the user interface to start synchronizing from the first section.
                If gsOrigSect <> gsInitSect Then
                    gsInitSect = gsOrigSect
                    gsNextSect = ""
                    LoadInitializationSettings
                End If
                bSyncMore = False
            End If
        End If
    Loop While bSyncMore
        
    If bShowLog Then
        frmLog.Show  ' Will exit program when closed, if gbAutoSync is true
    ElseIf gbAutoSync Then
        End  ' Exit program
    End If
    
    btnSync.Enabled = True
End Sub

Private Sub chkCopyOld_Click()
    If chkCopyOld = vbChecked Then
        gbCopyOld = True
    Else
        gbCopyOld = False
    End If
End Sub

Private Sub chkDelXtra_Click()
    If chkDelXtra = vbChecked Then
        gbDelXtra = True
        chkSyncBoth = vbUnchecked  ' Cannot have both true
    Else
        gbDelXtra = False
    End If
End Sub

Private Sub chkDelDirs_Click()
    If chkDelDirs = vbChecked Then
        gbDelDirs = True
        chkSyncBoth = vbUnchecked  ' Cannot have both true
    Else
        gbDelDirs = False
    End If
End Sub

Private Sub chkProcHidn_Click()
    If chkProcHidn = vbChecked Then
        gbProcHidn = True
    Else
        gbProcHidn = False
    End If
End Sub

Private Sub chkShowErr_Click()
    If chkShowErr = vbChecked Then
        gbShowErr = True
    Else
        gbShowErr = False
    End If
End Sub

Private Sub chkMinLog_Click()
    If chkMinlog = vbChecked Then
        gbMinLog = True
    Else
        gbMinLog = False
    End If
End Sub

Private Sub chkSyncBoth_Click()
    If chkSyncBoth = vbChecked Then
        gbSyncBoth = True
        chkDelXtra = vbUnchecked      ' Cannot have both true
        chkDelDirs = vbUnchecked  ' Cannot have both true
    Else
        gbSyncBoth = False
    End If
End Sub

Private Sub chkSyncSub_Click()
    If chkSyncSub = vbChecked Then
        gbSyncSub = True
    Else
        gbSyncSub = False
    End If
End Sub

Private Sub chkSkipDirs_Click()
    If chkSkipDirs = vbChecked Then
        gbSkipDirs = True
    Else
        gbSkipDirs = False
    End If
End Sub

Private Sub chkSkipReadOnlyTrgt_Click()
    If chkSkipReadOnlyTrgt = vbChecked Then
        gbSkipReadOnlyTrgt = True
    Else
        gbSkipReadOnlyTrgt = False
    End If
End Sub

Private Sub dirSrc_Change()
    Dim s As String
    
    s = dirSrc.Path
    txtSrcDir.Text = s
    
    If Left$(s, 2) = "\\" Then  ' Universal Naming Convention
        drvSrc.BackColor = frmMain.BackColor
    Else
        drvSrc.BackColor = txtSrcDir.BackColor
    End If
    
    dirSrc.BackColor = txtSrcDir.BackColor
End Sub

Private Sub dirTrgt_Change()
    Dim s As String
    
    s = dirTrgt.Path
    txtTrgtDir.Text = s
    
    If Left$(s, 2) = "\\" Then  ' Universal Naming Convention
        drvTrgt.BackColor = frmMain.BackColor
    Else
        drvTrgt.BackColor = txtTrgtDir.BackColor
    End If
    
    dirTrgt.BackColor = txtTrgtDir.BackColor
End Sub

Private Sub drvSrc_Change()
    If drvSrc.Enabled Then  ' Avoid recursive change events
        On Error Resume Next
            dirSrc.Path = Left$(drvSrc.Drive, 1) & ":"
            If Err.Number <> 0 Then
                Beep
                MsgBox "Not a valid source drive:" & gsLF & Err.Description, vbExclamation, gcProgName & " [" & gsInitSect & "]"
            End If
        On Error GoTo 0
    End If
End Sub

Private Sub drvTrgt_Change()
    If drvTrgt.Enabled Then  ' Avoid recursive change events
        On Error Resume Next
            dirTrgt.Path = Left$(drvTrgt.Drive, 1) & ":"
            If Err.Number <> 0 Then
                Beep
                MsgBox "Not a valid target drive:" & gsLF & Err.Description, vbExclamation, gcProgName & " [" & gsInitSect & "]"
            End If
        On Error GoTo 0
    End If
End Sub

Private Sub Form_Load()
    Me.Caption = Me.Caption & " " & LTrim$(Str$(App.Major)) & "." & LTrim$(Str$(App.Minor)) ' & "." & LTrim$(Str$(App.Revision))
    
    LoadInitializationSettings

    If gbAutoSync Then
        btnSync_Click
    Else
        Me.Show
    End If
End Sub

Private Sub LoadInitializationSettings()
    Dim s As String
    
    ' Make sure we always start relative to the original working directory if chaining.
    SetWorkingDirectory gsCurrDir
    
    gsNextSect = sInitializationEntry("NextSection", "", False)
    
    UpdateSectionLabels

    s = sInitializationEntry("Automatic", "NO")
    If (s = "YES") Or (s = "TRUE") Or (s = "1") Then
        gbAutoSync = True
    Else
        gbAutoSync = False
    End If

    s = sInitializationEntry("NotifyUser", "YES")
    If (s = "YES") Or (s = "TRUE") Or (s = "1") Then
        gbNtfyUser = True
    Else
        gbNtfyUser = False
    End If

    s = sInitializationEntry("ShowErrorMessages", "YES")
    If (s = "YES") Or (s = "TRUE") Or (s = "1") Then
        gbShowErr = True
        chkShowErr = vbChecked
    Else
        gbShowErr = False
        chkShowErr = vbUnchecked
    End If

    s = sInitializationEntry("MinimizeLogMessages", "YES")
    If (s = "YES") Or (s = "TRUE") Or (s = "1") Then
        gbMinLog = True
        chkMinlog = vbChecked
    Else
        gbMinLog = False
        chkMinlog = vbUnchecked
    End If
    
    gsRunDir = sInitializationEntry("RunDirectory", "", False)
    gsRunCmd = sInitializationEntry("RunCommand", "", False)
    
    gsAltRunDir = sInitializationEntry("AlternateRunDirectory", gsRunDir, False)
    gsAltRunCmd = sInitializationEntry("AlternateRunCommand", gsRunCmd, False)
    
    s = sInitializationEntry("SourceDirectory", "", False)
    If s = "" Then
        s = gsCurrDir
    ElseIf (InStr(s, ":") = 0) And (InStr(s, "\\") = 0) Then
        s = gsInitDrv & s
    End If
    txtSrcDir.Text = s
    Call ValidateSourceDirectory
    
    s = sInitializationEntry("TargetDirectory", "", False)
    If s = "" Then
        s = gsCurrDir
    ElseIf (InStr(s, ":") = 0) And (InStr(s, "\\") = 0) Then
        s = gsInitDrv & s
    End If
    txtTrgtDir.Text = s
    Call ValidateTargetDirectory

    s = sInitializationEntry("IgnoreFileTypes", "")
    txtIgnrType = s

    s = sInitializationEntry("OnlyProcessFileTypes", "")
    txtProcType = s

    s = sInitializationEntry("CopyOlderFiles", "NO")
    If (s = "YES") Or (s = "TRUE") Or (s = "1") Then
        gbCopyOld = True
        chkCopyOld = vbChecked
    Else
        gbCopyOld = False
        chkCopyOld = vbUnchecked
    End If

    s = sInitializationEntry("SynchronizeBothWays", "NO")
    If (s = "YES") Or (s = "TRUE") Or (s = "1") Then
        gbSyncBoth = True
        chkSyncBoth = vbChecked
    Else
        gbSyncBoth = False
        chkSyncBoth = vbUnchecked
    End If

    s = sInitializationEntry("IncludeSubdirectories", "NO")
    If (s = "YES") Or (s = "TRUE") Or (s = "1") Then
        gbSyncSub = True
        chkSyncSub = vbChecked
    Else
        gbSyncSub = False
        chkSyncSub = vbUnchecked
    End If

    s = sInitializationEntry("SkipMissingDirectories", "NO")
    If (s = "YES") Or (s = "TRUE") Or (s = "1") Then
        gbSkipDirs = True
        chkSkipDirs = vbChecked
    Else
        gbSkipDirs = False
        chkSkipDirs = vbUnchecked
    End If

    s = sInitializationEntry("SkipReadOnlyTargetFiles", "NO")
    If (s = "YES") Or (s = "TRUE") Or (s = "1") Then
        gbSkipReadOnlyTrgt = True
        chkSkipReadOnlyTrgt = vbChecked
    Else
        gbSkipReadOnlyTrgt = False
        chkSkipReadOnlyTrgt = vbUnchecked
    End If

    s = sInitializationEntry("ProcessHiddenFiles", "NO")
    If (s = "YES") Or (s = "TRUE") Or (s = "1") Then
        gbProcHidn = True
        chkProcHidn = vbChecked
    Else
        gbProcHidn = False
        chkProcHidn = vbUnchecked
    End If

    s = sInitializationEntry("DeleteExtraFiles", "NO")
    If (s = "YES") Or (s = "TRUE") Or (s = "1") Then
        If gbSyncBoth Then  ' Cannot both be true
            gbDelXtra = False
            chkDelXtra = vbUnchecked
            Beep
            MsgBox "Initialization setup error in file [" & gsInitFile & "]: SynchronizeBothWays and DeleteExtraFiles cannot both be enabled", vbExclamation, gcProgName & " [" & gsInitSect & "]"
            gbAutoSync = False  ' Popup selection form due to error
        Else
            gbDelXtra = True
            chkDelXtra = vbChecked
        End If
    Else
        gbDelXtra = False
        chkDelXtra = vbUnchecked
    End If

    s = sInitializationEntry("DeleteExtraDirectories", "NO")
    If (s = "YES") Or (s = "TRUE") Or (s = "1") Then
        If gbSyncBoth Then  ' Cannot both be true
            gbDelDirs = False
            chkDelDirs = vbUnchecked
            Beep
            MsgBox "Initialization setup error in file [" & gsInitFile & "]: SynchronizeBothWays and DeleteExtraDirectories cannot both be enabled", vbExclamation, gcProgName & " [" & gsInitSect & "]"
            gbAutoSync = False  ' Popup selection form due to error
        Else
            gbDelDirs = True
            chkDelDirs = vbChecked
        End If
    Else
        gbDelDirs = False
        chkDelDirs = vbUnchecked
    End If
End Sub

Private Sub UpdateSectionLabels()
    lblInitSect.Caption = "[" & gsInitSect & "]"
    If Len(gsNextSect) > 0 Then
        lblNextSect.Caption = "[" & gsNextSect & "]"
        lblNextSectLbl.Visible = True
    Else
        lblNextSect.Caption = ""
        lblNextSectLbl.Visible = False
    End If
End Sub

Private Sub txtSrcDir_LostFocus()
    Call ValidateSourceDirectory
End Sub

Private Sub txtTrgtDir_LostFocus()
    Call ValidateTargetDirectory
End Sub
