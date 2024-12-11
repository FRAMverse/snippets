Sub ComputeMR()
'
' ComputeMR Macro
' Macro recorded 01/20/2009 by Angelika
'
' Keyboard Shortcut: Ctrl+Shift+C
'

 Dim TaMR() As Single
    Dim FrMr() As Single
    Dim SpecialMr() As Single
    Dim Age As Integer
    Dim DevType As String
    Dim i As Integer
    Dim lnCount As Integer
    Dim FramStk As Integer
    Dim TAMMStk As Integer
    Dim SpecialStk As Integer
    Dim FrMarked() As Long
    Dim FrUnmarked() As Long
    Dim TaMarked() As Long
    Dim TaUnmarked() As Long
    Dim SpecialMarked() As Long
    Dim SpecialUnmarked() As Long
    Dim Run As String
    Dim Region As String
    Dim Basin As String
    Dim Hatchery As String
    Dim RunAr() As String
    Dim RegionAr() As String
    Dim BasinAr() As String
    Dim HatcheryAr() As String
    Dim StkNameAr() As String
    Dim DevTypeAr() As String
    
    'CE: making sure we're on the correct worksheet
    'Workbooks("ChinRSScalars&MR09.xls").Activate
    ActiveWorkbook.Activate
    Worksheets("RMIS_Mod").Activate
    
    'count the number of lines
    i = 1
    Do While ActiveSheet.Cells(i, 2).Value <> 0
        i = i + 1
    Loop
    lnCount = i
    
    'CE: Restructuring our declared variables
    'CE: Pretty sure Fr and Ta refer to FRAM and TAMM respectively
    'CE: Not sure on Special, except that there are some calculations that treat a few fisheries differently
    ReDim FrMarked(40, 5)
    ReDim FrUnmarked(40, 5)
    ReDim FrMr(40, 5)
    ReDim TaMarked(57, 5)
    ReDim TaUnmarked(57, 5)
    ReDim TaMR(57, 5)
    ReDim SpecialMarked(2, 5)
    ReDim SpecialUnmarked(2, 5)
    ReDim SpecialMr(2, 5)
    
    'ReDim FramStk(lnCount)
    'ReDim TAMMStk(lnCount)
    ReDim RunAr(lnCount)
    ReDim RegionAr(lnCount)
    ReDim BasinAr(lnCount)
    ReDim HatcheryAr(lnCount)
    ReDim StkNameAr(lnCount)
    ReDim DevTypeAr(lnCount)
    
    ' map to stock
    'CE: N is counter for possible errors (storing every entry which is not assigned a FRAM stock.
    N = 1
    For i = 2 To lnCount - 1
        'these fields are needed to define a TAMM/FRAM stock
        Run = Cells(i, 1).Value
        Region = Cells(i, 4).Value ' "PSC Region"
        Basin = Cells(i, 5).Value ' "PSC Basin"
        Hatchery = Cells(i, 6).Value ' "Hatchery Name"
        DevType = Cells(i, 13).Value ' "Fing/Yrl"
        StkName = Cells(i, 7).Value ' "Stock Location Name"
        
        FramStk = 0
        TAMMStk = 0
        SpecialStk = 0
        
        'CE: basically a giant lookup table; based on entry, should assign FRAM, TAMM, and/or Special stock number.
        'CE: Those numbers determine whether the marked and unmarked fish contribute to counts for appropriate stock-age-markedstatus counts.
        Select Case Run
            Case "FCH"
                Select Case Region
                    Case "CECA", "SAFA"
                        FramStk = 35
                    Case "CECR"
                        Select Case Basin
                            Case "DESC"
                                SpecialStk = 1
                                FramStk = 24
                            Case "UMAT", "KLIC"
                                SpecialStk = 2
                                FramStk = 24
                            Case "WIND"
                                Select Case Hatchery
                                    Case "LTL WHITE SALMON NFH", "WILLARD NFH", "DRANO LK PENS -FWS", "WHITE SALMON ES(WHIT"
                                        SpecialStk = 2
                                        FramStk = 24
                                    Case "NA", "SPRING CR NFH", "SPRING CR    29.0159"

                                        Select Case DevType
                                            Case "Fing"
                                                FramStk = 22
                                        End Select
                                End Select
                        End Select
                    Case "CRGN"
                        Select Case Basin
                            Case "CRGNG"
                                Select Case Hatchery
                                    Case "TURTLE ROCK HATCHERY", "NA", "WELLS HATCHERY", "GRANT COUNTY PUD", "WELLS DAM SP CHANNEL", "OROVILLE ES(OROVILLE", "WATERVILLE SD 209", "MANSFIELD SD 207", "RFEG 14 CASCADE COLUMBIA", "FOSTER CREEK CONSERVATION", "BRIDGEPORT ES"
                                        FramStk = 23
                                    Case "PRIEST RAPIDS HATCHERY", "RINGOLD SPRINGS HATCHERY", "FRANKLIN CONSERV DIST", "YAKIMA BASIN ENVIRONMENTA", "GRANT COUNTY CONS. DIST.", "BENTON CONSERVATION DISTR"
                                        SpecialStk = 1
                                        FramStk = 24
                                End Select
                        End Select
                    Case "FRTH"
                        Select Case Basin
                            Case "LWFR", "LOFR", "FRTHG"
                                Select Case Hatchery
                                    Case "H-Chilliwack River H", "H-Chehalis River H", "H-Alouette River, South H", "H-Coquitlam River H"
                                        FramStk = 30
                                    Case "H-Little Campbell River H", "H-Serpentine River H", "H-Tynehead H"
                                        FramStk = 32
                                    Case "H-Pr George (Mackenzie) S"
                                        FramStk = 31
                                End Select
                            Case "TOMF"
                                FramStk = 31
                            Case "UPFR", "TOMM", "UPTR"
                                FramStk = 31
                        End Select
                    Case "GST"
                        Select Case Basin
                            Case "GSVI"
                                FramStk = 32
                            Case "GSMS"
                                FramStk = 32
                            Case "GSMN"
                                FramStk = 32
                        End Select
                    Case "HOOD"
                        Select Case DevType
                            Case "Fing"
                                FramStk = 16
                            Case "Year"
                                FramStk = 17
                        End Select
                        Select Case Hatchery
                              Case "HOODSPORT HATCHERY"
                                  Select Case DevType
                                      Case "Fing"
                                          TAMMStk = 12
                                      Case "Year"
                                          TAMMStk = 13
                                  End Select
                              Case "GEORGE ADAMS HATCHRY"
                                  TAMMStk = 16
                          End Select
                    Case "JUAN"
                        Select Case Basin
                            Case "ELDU"
                                FramStk = 18
                                TAMMStk = 56
                            Case "LYHO"
                                FramStk = 38
                                TAMMStk = 57
                        End Select
                    Case "LOCR"
                        Select Case Basin
                            Case "YOCL"
                                Select Case StkName
                                    Case "BIG CR HATCHERY", "EEL LK (COOS BASIN)", "COLE RIVERS HATCHERY", "KLASKANINE R", "BIG CR HATCHER", "BONNEVILLE HATCHERY", _
                                        "CEDC YOUNGS BAY NET", "KLASKANINE HATCHERY", "KLASKANINE S FK POND", "TANNER CR (BNVILLE)", "WASHOUGAL R  28.0159"

                                        FramStk = 19
                                    End Select
                            Case "COWL", "GREL", "SAWA"
                                FramStk = 20
                            Case "LEWI"
                                Select Case Hatchery
                                    Case "FALLERT CR HATCHERY", "KALAMA FALLS HATCHERY"
                                        FramStk = 20
                                    Case "NA"
                                        FramStk = 21
                                End Select
                            Case "SAND"
                                Select Case Hatchery
                                    Case "BONNEVILLE HATCHERY", "SANDY HATCHERY", "STAYTON PD (WILLAMET"

                                        SpecialStk = 2
                                        FramStk = 24
                                End Select
                            Case "Will"
                            Select Case Hatchery
                                    Case "WILLAMETTE HATCHERY"
                                        FramStk = 19
                                End Select
                          End Select
                    Case "MPS"
                        Select Case DevType
                            Case "Fing"
                                Select Case Hatchery
                                    Case "PORTAGE BAY HATCHERY"
                                        TAMMStk = 23
                                        FramStk = 12
                                    Case Else
                                        FramStk = 11
                                 End Select
                            Case "Year"
                                FramStk = 14
                        End Select
                        Select Case Basin
                            Case "DUWA"
                                
                                        Select Case DevType
                                            Case "Year"
                                                TAMMStk = 26
                                            Case Else
                                                TAMMStk = 24
                                        End Select
                             
                            Case "EKPN"
                                Select Case Hatchery
                                    Case "GROVERS CR HATCHERY"
                                        TAMMStk = 20
                                    Case "GORST CR REARING PND"
                                        Select Case DevType
                                            Case "Fing"
                                                TAMMStk = 27
                                            Case "Year"
                                                TAMMStk = 28
                                        End Select
                               End Select
                            Case "LAKW"
                                Select Case Hatchery
                                    Case "ISSAQUAH HATCHERY"
                                        TAMMStk = 21
                                End Select
                            Case "PUYA"
                                                TAMMStk = 29
                       End Select
                         
                    Case "NOWA"
                        FramStk = 1
                        Select Case Hatchery
                            Case "GLENWOOD SPRINGS"
                                TAMMStk = 48
                            Case "LUMMI SEA PONDS"
                                TAMMStk = 51
                            Case "SAMISH HATCHERY", "LUMMI HATCHERY -POND", "WHATCOM CR HATCHERY"
                                TAMMStk = 52
                        End Select
                    Case "NOOR"
                        FramStk = 28
                    Case "NWC"
                        FramStk = 36


                    Case "SJOA"
                        FramStk = 35
                    Case "SKAG"
                        Select Case DevType
                                    Case "Fing"
                                        FramStk = 4
                                        TAMMStk = 2
                                    Case "Year"
                                        FramStk = 5
                                        TAMMStk = 3
                                End Select
                    Case "SNAK"
                        Select Case Basin
                            Case "CLEA", "GRIA", "LOSN", "SNAKG", "UPSN", "SALM"
                                FramStk = 27
                        End Select
                        
                               
                    Case "SPS"
                        Select Case Run
                            Case "FCH"
                                Select Case DevType
                                    Case "Fing"
                                        FramStk = 13
                                    Case "Year"
                                        FramStk = 14
                                End Select
                        End Select
                        Select Case Basin
                            Case "CHAM"
                                Select Case Hatchery
                                    Case "CHAMBERS CR + GARRISON", "CHAMBERS CR HATCHERY", "GARRISON HATCHERY", "LAKEWOOD HATCHERY"
                                        Select Case DevType
                                            Case "Fing"
                                                TAMMStk = 35
                                            Case "Year"
                                                TAMMStk = 36
                                        End Select
                                        
                                    Case "NISQUALLY HATCHERY"
                                        TAMMStk = 37
                                End Select
                            Case "DES"
                                Select Case Hatchery
                                    Case "COULTER CR REARING P"
                                        TAMMStk = 44
                                    Case "TUMWATER FALLS HATCH", "PERCIVAL COVE+TUMWATER FA", "PERCIVAL COVE NET PN", "MCKERNAN HATCHERY"

                                        Select Case DevType
                                            Case "Fing"
                                                TAMMStk = 42
                                            Case "Year"
                                                TAMMStk = 43
                                        End Select
                                End Select
                            Case "EKPS"
                                Select Case DevType
                                    Case "Fing"
                                         TAMMStk = 32
                                    Case "Year"
                                       Select Case Hatchery
                                            Case "MINTER CR HATCHERY"
                                                TAMMStk = 34
                                       End Select
                                End Select
                            
                            
                               
                            Case "NISQ"
                                        TAMMStk = 37
                        End Select
                    Case "NPS"
                        Select Case Basin
                            Case "SNOH"
                                Select Case Hatchery
                                    Case "BERNIE GOBIN HATCH"
                                        FramStk = 10
                                        TAMMStk = 9
                                    Case " WALLACE R HATCHERY"
                                        Select Case DevType
                                            Case "Fing"
                                                TAMMStk = 4
                                                FramStk = 7
                                            Case "Year"
                                                TAMMStk = 5
                                                FramStk = 8
                                        End Select
                                    Case Else
                                        Select Case DevType
                                            Case "Fing"
                                                FramStk = 7
                                                TAMMStk = 4
                                            Case "Year"
                                                FramStk = 8
                                                TAMMStk = 5
                                        End Select
                                    End Select
                            Case "STIL"
                                        FramStk = 9
                                        TAMMStk = 6
                        End Select
                    Case "UPCR"
                        Select Case Basin
                            Case "YAKI", "MNPR"
                                SpecialStk = 1
                                FramStk = 24
                            Case Else
                                FramStk = 23
                        End Select
                    Case "WCVI"
                        FramStk = 29
                    
                    Case "WILP"
                        FramStk = 37
                    Case "SOOR"
                        Select Case Basin
                            Case "COOS", "COQU", "SIXE", "UMPQ", "CHET"
                                FramStk = 39
                        End Select
                End Select
            
            Case "SPCH"
                Select Case Region

                    Case "FRTH"
                            FramStk = 31
                    Case "GST"
                        Select Case Hatchery
                            Case "H-Quesnel River H"
                                FramStk = 31
                        End Select
                        
                    Case "JUAN"
                        FramStk = 18
                        Select Case StkName
                            Case "DUNGENESS R  18.0018"
                                TAMMStk = 55
                        End Select
                    Case "LOCR"
                        Select Case Basin
                            Case "COWL", "GREL", "LEWI"
                                FramStk = 25
                            Case "SAND", "YOCL", "WILL", "NOOR"
                                FramStk = 26
                        End Select
                    Case "MPS"
                        Select Case DevType
                            Case "Fing"
                                FramStk = 15
                            Case "Year"
                                FramStk = 33
                        End Select
                    Case "NOWA"
                        
                        Select Case Hatchery
                            Case "KENDALL CR HATCHERY"
                                FramStk = 2
                            Case "SKOOKUM CR HATCHERY"
                                FramStk = 3
                            Case "LUMMI SEA PONDS"
                                FramStk = 2
                            Case "LUMMI HATCHERY -POND"
                                FramStk = 2
                        End Select
                    Case "SKAG"
                        Select Case DevType
                            Case "Year", "Fing"
                                FramStk = 6
                                TAMMStk = 1
                        End Select
                    Case "SPS"
                        Select Case Basin
                            Case "PUYA", "EKPS"
                                Select Case DevType
                                    Case "Fing"
                                        FramStk = 15
                                    Case "Year"
                                        FramStk = 33
                                End Select
                            End Select
                        End Select
                    
            
                
            End Select
            Age = Cells(i, 12).Value
            'CE: Add sum of clipped fish to the current values for this row to the ongoing total for marked fish of FRStock-age
            FrMarked(FramStk, Age) = FrMarked(FramStk, Age) + Cells(i, 8).Value + Cells(i, 10).Value
            'CE: Add sum of unclipped fish to the current values for this row to the ongoing total for marked fish of FRStock-age
            FrUnmarked(FramStk, Age) = FrUnmarked(FramStk, Age) + Cells(i, 9).Value + Cells(i, 11).Value
            'CE: Calc ratio of marked to total
            FrMr(FramStk, Age) = FrMarked(FramStk, Age) / (FrMarked(FramStk, Age) + FrUnmarked(FramStk, Age))

            'CE: As above, but for TAMM stock totals instead
            TaMarked(TAMMStk, Age) = TaMarked(TAMMStk, Age) + Cells(i, 8).Value + Cells(i, 10).Value
            TaUnmarked(TAMMStk, Age) = TaUnmarked(TAMMStk, Age) + Cells(i, 9).Value + Cells(i, 11).Value
            TaMR(TAMMStk, Age) = TaMarked(TAMMStk, Age) / (TaMarked(TAMMStk, Age) + TaUnmarked(TAMMStk, Age))
            
            'CE: As above, but for Special stock totals instead
            SpecialMarked(SpecialStk, Age) = SpecialMarked(SpecialStk, Age) + Cells(i, 8).Value + Cells(i, 10).Value
            SpecialUnmarked(SpecialStk, Age) = SpecialUnmarked(SpecialStk, Age) + Cells(i, 9).Value + Cells(i, 11).Value
            SpecialMr(SpecialStk, Age) = SpecialMarked(SpecialStk, Age) / (SpecialMarked(SpecialStk, Age) + SpecialUnmarked(SpecialStk, Age))
            
            
            
            'CE: Add appropriate stock numbers to the sheet (presumably for error checking)
            Cells(i, 14).Value = FramStk
            Cells(i, 15).Value = TAMMStk
            
            'If no FRAM stock assigned, fill arrays for error report
            If FramStk = 0 Then
                RunAr(N) = Cells(i, 1).Value
                RegionAr(N) = Cells(i, 4).Value
                BasinAr(N) = Cells(i, 5).Value
                HatcheryAr(N) = Cells(i, 6).Value
                StkNameAr(N) = Cells(i, 7).Value
                DevTypeAr(N) = Cells(i, 13).Value
                N = N + 1
            End If
    Next i
    'CE: Switch to "MR" tab
    Worksheets("MR").Activate
    For stk = 1 To 39
        For Age = 2 To 5
            Select Case stk
            'CE: zeroing out some entries, or else entering calculated ratios.
            'CE: Note that we are NOT zeroing out all the stock with "not needed" proportions, and there are many others with zeroes in the 2023 version. Not sure what's up with that.
            'CE: Note that we have 3 rows of header and 3 columns before age entries,
            'CE:    hence the stk+1 and Age + 2 indexes (really Age + 3 - 1, since ages start at age 2).
                Case 11, 13, 14 'CE: MidPS Fall Fing, SPS Fall Fing, SPS Fall Year
                    Cells(stk + 4, Age + 2).Value = 0
'                Case 24 'add Snake to Upriver Brights
'                    Cells(stk + 3, Age + 2).Value = (FrMarked(24, Age) + FrMarked(27, Age)) / (FrMarked(24, Age) + FrMarked(27, Age) + FrUnmarked(24, Age) + FrUnmarked(27, Age))
                Case Else
                    Cells(stk + 4, Age + 2).Value = FrMr(stk, Age)
            End Select
        Next Age
    Next stk
    
    'CE: same deal as above, but for TAMM info, which starts on row 47.
    For stk = 1 To 57
        For Age = 2 To 5
            Select Case stk
                Case 8
                    If TaMarked(4, Age) + TaMarked(5, Age) + TaUnmarked(4, Age) + TaUnmarked(5, Age) = 0 Then
 Cells(stk + 46, Age + 2).Value = 0
                    Else
                        Cells(stk + 46, Age + 2).Value = (TaMarked(4, Age) + TaMarked(5, Age)) / (TaMarked(4, Age) + TaMarked(5, Age) + TaUnmarked(4, Age) + TaUnmarked(5, Age))
                    End If
                Case 17
                    Cells(stk + 46, Age + 2).Value = TaMR(16, Age)
                Case 31
                    Cells(stk + 46, Age + 2).Value = 0
                Case 45
                    Cells(stk + 46, Age + 2).Value = 0
                Case 46
                    Cells(stk + 46, Age + 2).Value = 0
                Case Else
                    Cells(stk + 46, Age + 2).Value = TaMR(stk, Age)
            End Select
        Next Age
    Next stk
    
    'CE: As above(ish), but dealing with Upriver Brights and Mid-river brights, which get special handling in terms of calculations, and live on rows 107 and 108
    For stk = 1 To 2
        For Age = 2 To 5
            Select Case stk 'add Snake R to Upriver Brights
                Case 1
                        Cells(stk + 106, Age + 2).Value = (SpecialMarked(1, Age) + FrMarked(27, Age)) / (SpecialMarked(1, Age) + FrMarked(27, Age) + SpecialUnmarked(1, Age) + FrUnmarked(27, Age))
                Case Else
                        Cells(stk + 106, Age + 2).Value = SpecialMr(stk, Age)
            End Select
        Next Age
    Next stk
    
    'CE: save error-checking info for debugging
    Worksheets("ErrorCheck").Activate
    For x = 1 To N
            Cells(x + 2, 1).Value = RunAr(x)
            Cells(x + 2, 2).Value = RegionAr(x)
            Cells(x + 2, 3).Value = BasinAr(x)
            Cells(x + 2, 4).Value = HatcheryAr(x)
            Cells(x + 2, 5).Value = StkNameAr(x)
            Cells(x + 2, 6).Value = DevTypeAr(x)
    Next x
   
   
   
End Sub


