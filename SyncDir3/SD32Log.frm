VERSION 5.00
Begin VB.Form frmLog 
   Caption         =   "Directory Synchronization Log"
   ClientHeight    =   6090
   ClientLeft      =   525
   ClientTop       =   900
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
   HelpContextID   =   103
   Icon            =   "SD32Log.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6090
   ScaleWidth      =   9480
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton btnPrt 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "&Print Text"
      Height          =   375
      Left            =   1800
      TabIndex        =   3
      Top             =   5640
      Width           =   1095
   End
   Begin VB.CommandButton btnCopy 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "&Copy Text"
      Height          =   375
      Left            =   120
      TabIndex        =   2
      Top             =   5640
      Width           =   1095
   End
   Begin VB.CommandButton btnClos 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Cancel          =   -1  'True
      Caption         =   "Close"
      Default         =   -1  'True
      Height          =   375
      Left            =   8520
      TabIndex        =   1
      Top             =   5640
      Width           =   855
   End
   Begin VB.TextBox txtLog 
      BackColor       =   &H8000000F&
      Height          =   5295
      Left            =   120
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   0
      Text            =   "SD32Log.frx":030A
      Top             =   240
      Width           =   9255
   End
End
Attribute VB_Name = "frmLog"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' (32-bit Visual Basic 6.0 version)

Option Explicit

Private Sub btnClos_Click()
    If gbAutoSync Then
        Me.Hide
        End  ' Exit program
    Else
        Unload Me
    End If
End Sub

Private Sub btnCopy_Click()
    Clipboard.Clear
    Clipboard.SetText gsLog
    MsgBox "The log text has been copied to the Windows clipboard", vbInformation, gcProgName
End Sub

Private Sub btnPrt_Click()
    Dim sPrintLine As String
    Dim i As Long
    Dim j As Long
    Dim lLogPos As Long  ' Current starting position within sLogText
    Dim lLogLen As Long  ' Length of sLogText
    Dim lRet As Long
    
    On Error GoTo PrinterError
    
    Screen.MousePointer = vbHourglass
    
    Printer.Print "START OF DIRECTORY SYNCHRONIZATION LOG"
    
    lLogLen = Len(gsLog)
    lLogPos = 1
    Do While lLogPos <= lLogLen
        i = InStr(lLogPos, gsLog, vbLf)
        j = InStr(lLogPos, gsLog, vbCr)
        
        If (j > 0) And (j < i) Then
            i = j
        End If
        
        If i = 0 Then
            sPrintLine = Mid$(gsLog, lLogPos) ' Rest of string
            lLogPos = lLogLen + 1
        Else
            sPrintLine = Mid$(gsLog, lLogPos, (i - lLogPos))
            lLogPos = i + 1
        End If
        
        Do While sPrintLine <> ""  ' Simple line-wrapping
            i = Len(sPrintLine)
            If i > 70 Then
                j = 70
            Else
                j = i
            End If
            Printer.Print "     " & Left$(sPrintLine, j) ' Create small left "margin"
            sPrintLine = Mid$(sPrintLine, (j + 1)) ' Rest of line
        Loop
    Loop
    
    Printer.Print "END OF DIRECTORY SYNCHRONIZATION LOG"
    Printer.EndDoc
    
    Screen.MousePointer = vbNormal
    MsgBox "The directory synchronization log has been submitted to the printer", vbInformation, gcProgName
Exit Sub

PrinterError:
    Screen.MousePointer = vbNormal
    MsgBox "Error #" & Format$(Err.Number) & " while printing log" & vbLf & Err.Description, vbExclamation, gcProgName
End Sub

Private Sub Form_Load()
    Me.Caption = Me.Caption & " " & LTrim$(Str$(App.Major)) & "." & LTrim$(Str$(App.Minor)) ' & "." & LTrim$(Str$(App.Revision))
    
    txtLog = gsLog
End Sub


