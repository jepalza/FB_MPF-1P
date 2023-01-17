
Sub Refresco_INTR(t As Integer)
    intRTemp = intRTemp + t
End Sub

Sub adc_a(b As Integer)
    Dim wans As integer, ans As integer, c As Integer
    c = -(fC)
    wans = regA + b + c
    ans = wans And &HFF
    fS = (ans And F_S) <> 0
    f3 = (ans And F_3) <> 0
    f5 = (ans And F_5) <> 0
    fZ = (ans = 0)
    fC = (wans And &H100) <> 0
    fPV = ((regA Xor ((Not b) And &HFFFF)) And (regA Xor ans) And &H80) <> 0
    fH = (((regA And &HF) + (b And &HF) + c) And F_H) <> 0
    fN = False
    regA = ans
End Sub

Function adc16(a As integer, b As integer) As Integer
    Dim c As integer, ans As Integer
    c = -(fC)
    ans = (a + b + c) And &HFFFF
    fS = (ans And (F_S * 256)) <> 0
    f3 = (ans And (F_3 * 256)) <> 0
    f5 = (ans And (F_5 * 256)) <> 0
    fZ = (ans = 0)
    fC = ((a + b + c) And &H10000) <> 0
    fPV = ((a Xor ((Not b) And &HFFFF)) And (a Xor ans) And &H8000) <> 0
    fH = (((a And &HFFF) + (b And &HFFF) + c) And &H1000) <> 0
    fN = False
    adc16 = ans
End Function

Sub add_a(b As Integer)
    Dim ans As integer, wans as Integer
    wans = (regA + b)
    ans = wans And &HFF
    fS = (ans And F_S) <> 0
    f3 = (ans And F_3) <> 0
    f5 = (ans And F_5) <> 0
    fZ = (ans = 0)
    fC = (wans And &H100) <> 0
    fPV = ((regA Xor ((Not (b)) And &HFFFF)) And (regA Xor ans) And &H80) <> 0
    fH = (((regA And &HF) + (b And &HF)) And F_H) <> 0
    fN = False
    regA = ans
End Sub

Function add16(a As integer, b As integer) As Integer
    Dim ans As Integer
    ans = (a + b) And &HFFFF
    f3 = (ans And (F_3 * 256)) <> 0
    f5 = (ans And (F_5 * 256)) <> 0
    fC = ((a + b) And &H10000) <> 0
    fH = (((a And &HFFF) + (b And &HFFF)) And &H1000) <> 0
    fN = False
    add16 = ans
End Function

Sub and_a(b As Integer)
    regA = (regA And b)
    fS = (regA And F_S) <> 0
    f3 = (regA And F_3) <> 0
    f5 = (regA And F_5) <> 0
    fH = True
    fPV = Parity(regA)
    fZ = (regA = 0)
    fN = False
    fC = False
End Sub

Sub bitv(b As integer, r As Integer)
    Dim IsbitSet As Integer
    IsbitSet = (r And b) <> 0
    fN = False
    fH = True
    'f3 = (r And F_3) <> 0 ' anulados por ahora
    'f5 = (r And F_5) <> 0 ' los tengo bajo supervision
    fS = IsbitSet And (b = F_S)
    fZ = (IsbitSet = 0)
    fPV = fZ
End Sub

Function bitResv(bitvalor As integer, valor As integer) As Integer
    bitResv = valor And (bitvalor Xor &HFFFF)
End Function

Function bitSetv(bitvalor As integer, valor As integer) As Integer
    bitSetv = valor Or bitvalor
End Function

Sub ccf()
    f3 = (regA And F_3) <> 0
    f5 = (regA And F_5) <> 0
    fH = fC
    fN = False
    fC = Not fC
End Sub

 Sub cp_a(b As Integer)
    Dim ans As Integer
    
    ans = (regA - b) And &HFF   
    fS = (ans And F_S) <> 0
    f3 = (b And F_3) <> 0
    f5 = (b And F_5) <> 0
    fN = True
    fZ = (ans = 0)
    fC = ((regA - b) And &H100) <> 0
    fH = (((regA And &HF) - (b And &HF)) And F_H) <> 0
    fPV = ((regA Xor b) And (regA Xor ans) And &H80) <> 0
End Sub

Sub cpl_a()
    regA = (regA Xor &HFF) And &HFF
    
    f3 = (regA And F_3) <> 0
    f5 = (regA And F_5) <> 0
    fH = True
    fN = True
End Sub

Sub daa_a()
    Dim incr As Integer
    
    If (fH = True) Or ((regA And &HF) > &H9) Then
        incr = incr Or &H6
    End If
    
    If (fC = True) Or (regA > &H9F) Then
        incr = incr Or &H60
    End If
    
    If ((regA > &H8F) And ((regA And &HF) > 9)) Then
        incr = incr Or &H60
    End If
    
    If (regA > &H99) Then fC = True
    
    If (fN = True) Then
        ' sub_a incr
        fH = (((regA And &HF) - (incr And &HF)) And F_H) <> 0
        regA = (regA - incr) And &HFF
    Else
        ' add_a incr
        fH = (((regA And &HF) + (incr And &HF)) And F_H) <> 0
        regA = (regA + incr) And &HFF
    End If
    
    fS = (regA And F_S) <> 0
    f3 = (regA And F_3) <> 0
    f5 = (regA And F_5) <> 0
    fZ = (regA = 0)
    fPV = Parity(regA)
End Sub

Function dec16(a As integer) As Integer
    dec16 = (a - 1) And &HFFFF
End Function

Sub ex_af_af()
    Dim t As Integer
    
    t = ((regA * 256) Or getF)
    setAF regAF_
    regAF_ = t
End Sub

Function execute_cb() As Integer
    Dim xxx As Integer
    
    Refresco_INTR(1)
    
    xxx = peekb(regPC)
    regPC = regPC + 1
    
    If (xxx And 128) Then GoTo ex_cb128_255 Else GoTo ex_cb0_127
    
ex_cb0_127:
    If (xxx And 64) Then GoTo ex_cb64_127 Else GoTo ex_cb0_63
    
ex_cb0_63:
    If (xxx And 32) Then GoTo ex_cb32_63 Else GoTo ex_cb0_31
    
ex_cb0_31:
    If (xxx And 16) Then GoTo ex_cb16_31 Else GoTo ex_cb0_15
    
ex_cb0_15:
    If (xxx And 8) Then GoTo ex_cb8_15 Else GoTo ex_cb0_7
    
ex_cb0_7:
    If (xxx And 4) Then GoTo ex_cb4_7 Else GoTo ex_cb0_3
    
ex_cb0_3:
    If (xxx And 2) Then GoTo ex_cb2_3 Else GoTo ex_cb0_1
    
ex_cb0_1:
    If xxx = 0 Then
        ' 000 RLC B
        regB = rlc(regB)
        execute_cb = 8
    Else
        ' 001 RLC C
        regC = rlc(regC)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb2_3:
    If xxx = 2 Then
        ' 002 RLC D
        setD rlc(Div256(regDE))
        execute_cb = 8
    Else
        ' 003 RLC E
        setE rlc(getE)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb4_7:
    If (xxx And 2) Then GoTo ex_cb6_7 Else GoTo ex_cb4_5
    
ex_cb4_5:
    If xxx = 4 Then
        ' 004 RLC H
        setH rlc(Div256(regHL))
        execute_cb = 8
    Else
        ' 005 RLC L
        setL rlc(regHL And &HFF)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb6_7:
    If xxx = 6 Then
        ' 006 RLC (HL)
        pokeb regHL, rlc(peekb(regHL))
        execute_cb = 15
    Else
        ' 007 RLC A
        regA = rlc(regA)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb8_15:
    If (xxx And 4) Then GoTo ex_cb12_15 Else GoTo ex_cb8_11
    
ex_cb8_11:
    If (xxx And 2) Then GoTo ex_cb10_11 Else GoTo ex_cb8_9
    
ex_cb8_9:
    If xxx = 8 Then
        ' 008 RRC B
        regB = rrc(regB)
        execute_cb = 8
    Else
        ' 009 RRC C
        regC = rrc(regC)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb10_11:
    If xxx = 10 Then
        ' 010 RRC D
        setD rrc(Div256(regDE))
        execute_cb = 8
    Else
        ' 011 RRC E
        setE rrc(getE)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb12_15:
    If (xxx And 2) Then GoTo ex_cb14_15 Else GoTo ex_cb12_13
    
ex_cb12_13:
    If xxx = 12 Then
        ' 012 RRC H
        setH rrc(Div256(regHL))
        execute_cb = 8
    Else
        ' 013 RRC L
        setL rrc(regHL And &HFF)
        execute_cb = 8
    End If
    Exit Function

ex_cb14_15:
    If xxx = 14 Then
        ' 014 RRC (HL)
        pokeb regHL, rrc(peekb(regHL))
        execute_cb = 15
    Else
        ' 015 RRC A
        regA = rrc(regA)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb16_31:
    If (xxx And 8) Then GoTo ex_cb24_31 Else GoTo ex_cb16_23
    
ex_cb16_23:
    If (xxx And 4) Then GoTo ex_cb20_23 Else GoTo ex_cb16_19
    
ex_cb16_19:
    If (xxx And 2) Then GoTo ex_cb18_19 Else GoTo ex_cb16_17
    
ex_cb16_17:
    If xxx = 16 Then
        ' 016 RL B
        regB = rl(regB)
        execute_cb = 8
    Else
        ' 017 RL C
        regC = rl(regC)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb18_19:
    If xxx = 18 Then
        ' 018 RL D
        setD rl(Div256(regDE))
        execute_cb = 8
    Else
        ' 019 RL E
        setE rl(getE)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb20_23:
    If (xxx And 2) Then GoTo ex_cb22_23 Else GoTo ex_cb20_21
    
ex_cb20_21:
    If xxx = 20 Then
        ' 020 RL H
        setH rl(Div256(regHL))
        execute_cb = 8
    Else
        ' 021 RL L
        setL rl(regHL And &HFF)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb22_23:
    If xxx = 22 Then
        ' 022 RL (HL)
        pokeb regHL, rl(peekb(regHL))
        execute_cb = 15
    Else
        ' 023 RL A
        regA = rl(regA)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb24_31:
    If (xxx And 4) Then GoTo ex_cb28_31 Else GoTo ex_cb24_27

ex_cb24_27:
    If (xxx And 2) Then GoTo ex_cb26_27 Else GoTo ex_cb24_25
    
ex_cb24_25:
    If xxx = 24 Then
        ' 024 RR B
        regB = rr(regB)
        execute_cb = 8
    Else
        ' 025 RR C
        regC = rr(regC)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb26_27:
    If xxx = 26 Then
        ' 026 RR D
        setD rr(Div256(regDE))
        execute_cb = 8
    Else
        ' 027 RR E
        setE rr(getE)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb28_31:
    If (xxx And 2) Then GoTo ex_cb30_31 Else GoTo ex_cb28_29
    
ex_cb28_29:
    If xxx = 28 Then
        ' 028 RR H
        setH rr(Div256(regHL))
        execute_cb = 8
    Else
        ' 029 RR L
        setL rr(regHL And &HFF)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb30_31:
    If xxx = 30 Then
        ' 030 RR (HL)
        pokeb regHL, rr(peekb(regHL))
        execute_cb = 15
    Else
        ' 031 RR A
        regA = rr(regA)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb32_63:
    If (xxx And 16) Then GoTo ex_cb48_63 Else GoTo ex_cb32_47
    
ex_cb32_47:
    If (xxx And 8) Then GoTo ex_cb40_47 Else GoTo ex_cb32_39
    
ex_cb32_39:
    If (xxx And 4) Then GoTo ex_cb36_39 Else GoTo ex_cb32_35
    
ex_cb32_35:
    If (xxx And 2) Then GoTo ex_cb34_35 Else GoTo ex_cb32_33
    
ex_cb32_33:
    If xxx = 32 Then
        ' 32 ' SLA B
        regB = sla(regB)
        execute_cb = 8
    Else
        ' 33 ' SLA C
        regC = sla(regC)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb34_35:
    If xxx = 34 Then
        ' 34 ' SLA D
        setD sla(Div256(regDE))
        execute_cb = 8
    Else
        ' 35 ' SLA E
        setE sla(getE)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb36_39:
    If (xxx And 2) Then GoTo ex_cb38_39 Else GoTo ex_cb36_37
    
ex_cb36_37:
    If xxx = 36 Then
        ' 36 ' SLA H
        setH sla(Div256(regHL))
        execute_cb = 8
    Else
        ' 37 ' SLA L
        setL sla(regHL And &HFF)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb38_39:
    If xxx = 38 Then
        ' 38 ' SLA (HL)
        pokeb regHL, sla(peekb(regHL))
        execute_cb = 15
    Else
        ' 39 ' SLA A
        regA = sla(regA)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb40_47:
    If (xxx And 4) Then GoTo ex_cb44_47 Else GoTo ex_cb40_43
    
ex_cb40_43:
    If (xxx And 2) Then GoTo ex_cb42_43 Else GoTo ex_cb40_41
    
ex_cb40_41:
    If xxx = 40 Then
        ' 40 ' SRA B
        regB = sra(regB)
        execute_cb = 8
    Else
        ' 41 ' SRA C
        regC = sra(regC)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb42_43:
    If xxx = 42 Then
        ' 42 ' SRA D
        setD sra(Div256(regDE))
        execute_cb = 8
    Else
        ' 43 ' SRA E
        setE sra(getE)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb44_47:
    If (xxx And 2) Then GoTo ex_cb46_47 Else GoTo ex_cb44_45
    
ex_cb44_45:
    If xxx = 44 Then
        ' 44 ' SRA H
        setH sra(Div256(regHL))
        execute_cb = 8
    Else
        ' 45  ' SRA L
        setL sra(regHL And &HFF)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb46_47:
    If xxx = 46 Then
        ' 46 ' SRA (HL)
        pokeb regHL, sra(peekb(regHL))
        execute_cb = 15
    Else
        ' 47 ' SRA A
        regA = sra(regA)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb48_63:
    If (xxx And 8) Then GoTo ex_cb56_63 Else GoTo ex_cb48_55
    
ex_cb48_55:
    If (xxx And 4) Then GoTo ex_cb52_55 Else GoTo ex_cb48_51
    
ex_cb48_51:
    If (xxx And 2) Then GoTo ex_cb50_51 Else GoTo ex_cb48_49
    
ex_cb48_49:
    If xxx = 48 Then
        ' 48 ' SLS B
        regB = sls(regB)
        execute_cb = 8
    Else
        ' 49 ' SLS C
        regC = sls(regC)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb50_51:
    If xxx = 50 Then
        ' 50 ' SLS D
        setD sls(Div256(regDE))
        execute_cb = 8
    Else
        ' 51 ' SLS E
        setE sls(getE)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb52_55:
    If (xxx And 2) Then GoTo ex_cb54_55 Else GoTo ex_cb52_53
    
ex_cb52_53:
    If xxx = 52 Then
        ' 52 ' SLS H
        setH sls(Div256(regHL))
        execute_cb = 8
    Else
        ' 53 ' SLS L
        setL sls(regHL And &HFF)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb54_55:
    If xxx = 54 Then
        ' 54 ' SLS (HL)
        pokeb regHL, sls(peekb(regHL))
        execute_cb = 15
    Else
        ' 55 ' SLS A
        regA = sls(regA)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb56_63:
    If (xxx And 4) Then GoTo ex_cb60_63 Else GoTo ex_cb56_59
    
ex_cb56_59:
    If (xxx And 2) Then GoTo ex_cb58_59 Else GoTo ex_cb56_57
    
ex_cb56_57:
    If xxx = 56 Then
        ' 56 ' SRL B
        regB = srl(regB)
        execute_cb = 8
    Else
        ' 57 ' SRL C
        regC = srl(regC)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb58_59:
    If xxx = 58 Then
        ' 58 ' SRL D
        setD srl(Div256(regDE))
        execute_cb = 8
    Else
        ' 59 ' SRL E
        setE srl(getE)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb60_63:
    If (xxx And 2) Then GoTo ex_cb62_63 Else GoTo ex_cb60_61
    
ex_cb60_61:
    If xxx = 60 Then
        ' 60 ' SRL H
        setH srl(Div256(regHL))
        execute_cb = 8
    Else
        ' 61 ' SRL L
        setL srl(regHL And &HFF)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb62_63:
    If xxx = 62 Then
        ' 62 ' SRL (HL)
        pokeb regHL, srl(peekb(regHL))
        execute_cb = 15
    Else
        ' 63 ' SRL A
        regA = srl(regA)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb64_127:
    If (xxx And 32) Then GoTo ex_cb96_127 Else GoTo ex_cb64_95
    
ex_cb64_95:
    If (xxx And 16) Then GoTo ex_cb80_95 Else GoTo ex_cb64_79
    
ex_cb64_79:
    If (xxx And 8) Then GoTo ex_cb72_79 Else GoTo ex_cb64_71
    
ex_cb64_71:
    If (xxx And 4) Then GoTo ex_cb68_71 Else GoTo ex_cb64_67
    
ex_cb64_67:
    If (xxx And 2) Then GoTo ex_cb66_67 Else GoTo ex_cb64_65
    
ex_cb64_65:
    If xxx = 64 Then
        ' 064 BIT 0,B
        bitv &H1, regB
        execute_cb = 8
    Else
        ' 065 ' BIT 0,C
        bitv 1, regC
        execute_cb = 8
    End If
    Exit Function
    
ex_cb66_67:
    If xxx = 66 Then
        ' 066 BIT 0,D
        bitv 1, Div256(regDE)
        execute_cb = 8
    Else
        ' 067 BIT 0,E
        bitv 1, getE
        execute_cb = 8
    End If
    Exit Function
    
ex_cb68_71:
    If (xxx And 2) Then GoTo ex_cb70_71 Else GoTo ex_cb68_69
    
ex_cb68_69:
    If xxx = 68 Then
        ' 068 BIT 0,H
        bitv 1, Div256(regHL)
        execute_cb = 8
    Else
        ' 069 BIT 0,L
        bitv 1, (regHL And &HFF)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb70_71:
    If xxx = 70 Then
        ' 070 BIT 0,(HL)
        bitv 1, peekb(regHL)
        execute_cb = 12
    Else
        ' 071 BIT 0,A
        bitv 1, regA
        execute_cb = 8
    End If
    Exit Function
    
ex_cb72_79:
    If (xxx And 4) Then GoTo ex_cb76_79 Else GoTo ex_cb72_75
    
ex_cb72_75:
    If (xxx And 2) Then GoTo ex_cb74_75 Else GoTo ex_cb72_73
    
ex_cb72_73:
    If xxx = 72 Then
        ' 72 ' BIT 1,B
        bitv 2, regB
        execute_cb = 8
    Else
        ' 73 ' BIT 1,C
        bitv 2, regC
        execute_cb = 8
    End If
    Exit Function
    
ex_cb74_75:
    If xxx = 74 Then
        ' 74 ' BIT 1,D
        bitv 2, Div256(regDE)
        execute_cb = 8
    Else
        ' 75 ' BIT 1,E
        bitv 2, getE
        execute_cb = 8
    End If
    Exit Function
    
ex_cb76_79:
    If (xxx And 2) Then GoTo ex_cb78_79 Else GoTo ex_cb76_77
    
ex_cb76_77:
    If xxx = 76 Then
        ' 76 ' BIT 1,H
        bitv 2, Div256(regHL)
        execute_cb = 8
    Else
        ' 77 ' BIT 1,L
        bitv 2, (regHL And &HFF)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb78_79:
    If xxx = 78 Then
        ' 78 ' BIT 1,(HL)
        bitv 2, peekb(regHL)
        execute_cb = 12
    Else
        ' 79 ' BIT 1,A
        bitv 2, regA
        execute_cb = 8
    End If
    Exit Function

ex_cb80_95:
    If (xxx And 8) Then GoTo ex_cb88_95 Else GoTo ex_cb80_87
    
ex_cb80_87:
    Select Case xxx
    Case 80 ' bitv2,B
        bitv 4, regB
        execute_cb = 8
    Case 81 ' bitv2,C
        bitv 4, regC
        execute_cb = 8
    Case 82 ' bitv2,D
        bitv 4, Div256(regDE)
        execute_cb = 8
    Case 83 ' bitv2,E
        bitv 4, getE
        execute_cb = 8
    Case 84 ' bitv2,H
        bitv 4, Div256(regHL)
        execute_cb = 8
    Case 85 ' bitv2,L
        bitv 4, (regHL And &HFF)
        execute_cb = 8
    Case 86 ' bitv2,(HL)
        bitv 4, peekb(regHL)
        execute_cb = 12
    Case 87 ' bitv2,A
        bitv 4, regA
        execute_cb = 8
    End Select
    Exit Function
    
ex_cb88_95:
    Select Case xxx
    Case 88 ' bitv3,B
        bitv 8, regB
        execute_cb = 8
    Case 89 ' bitv3,C
        bitv 8, regC
        execute_cb = 8
    Case 90 ' bitv3,D
        bitv 8, Div256(regDE)
        execute_cb = 8
    Case 91 ' bitv3,E
        bitv 8, getE
        execute_cb = 8
    Case 92 ' bitv3,H
        bitv 8, Div256(regHL)
        execute_cb = 8
    Case 93 ' bitv3,L
        bitv 8, (regHL And &HFF)
        execute_cb = 8
    Case 94 ' bitv3,(HL)
        bitv 8, peekb(regHL)
        execute_cb = 12
    Case 95 ' bitv3,A
        bitv 8, regA
        execute_cb = 8
    End Select
    Exit Function
    
ex_cb96_127:
    If (xxx And 16) Then GoTo ex_cb112_127 Else GoTo ex_cb96_111
    
ex_cb96_111:
    If (xxx And 8) Then GoTo ex_cb104_111 Else GoTo ex_cb96_103
    
ex_cb96_103:
    Select Case xxx
    Case 96 ' bitv4,B
        bitv &H10, regB
        execute_cb = 8
    Case 97 ' bitv4,C
        bitv &H10, regC
        execute_cb = 8
    Case 98 ' bitv4,D
        bitv &H10, Div256(regDE)
        execute_cb = 8
    Case 99 ' bitv4,E
        bitv &H10, getE
        execute_cb = 8
    Case 100 ' bitv4,H
        bitv &H10, Div256(regHL)
        execute_cb = 8
    Case 101 ' bitv4,L
        bitv &H10, (regHL And &HFF)
        execute_cb = 8
    Case 102 ' bitv4,(HL)
        bitv &H10, peekb(regHL)
        execute_cb = 12
    Case 103 ' bitv4,A
        bitv &H10, regA
        execute_cb = 8
    End Select
    Exit Function
    
ex_cb104_111:
    Select Case xxx
    Case 104 ' bitv5,B
        bitv &H20, regB
        execute_cb = 8
    Case 105 ' bitv5,C
        bitv &H20, regC
        execute_cb = 8
    Case 106 ' bitv5,D
        bitv &H20, Div256(regDE)
        execute_cb = 8
    Case 107 ' bitv5,E
        bitv &H20, getE
        execute_cb = 8
    Case 108 ' bitv5,H
        bitv &H20, Div256(regHL)
        execute_cb = 8
    Case 109 ' bitv5,L
        bitv &H20, (regHL And &HFF)
        execute_cb = 8
    Case 110 ' bitv5,(HL)
        bitv &H20, peekb(regHL)
        execute_cb = 12
    Case 111 ' bitv5,A
        bitv &H20, regA
        execute_cb = 8
    End Select
    Exit Function

ex_cb112_127:
    If (xxx And 8) Then GoTo ex_cb120_127 Else GoTo ex_cb112_119
    
ex_cb112_119:
    If (xxx And 4) Then GoTo ex_cb116_119 Else GoTo ex_cb112_115
    
ex_cb112_115:
    If (xxx And 2) Then GoTo ex_cb114_115 Else GoTo ex_cb112_113
    
ex_cb112_113:
    If xxx = 112 Then
        ' 112 bitv6,B
        bitv &H40, regB
        execute_cb = 8
    Else
        ' 113 bitv6,C
        bitv &H40, regC
        execute_cb = 8
    End If
    Exit Function

ex_cb114_115:
    If xxx = 114 Then
        ' 114 bitv6,D
        bitv &H40, Div256(regDE)
        execute_cb = 8
    Else
        ' 115 bitv6,E
        bitv &H40, getE
        execute_cb = 8
    End If
    Exit Function

ex_cb116_119:
    If (xxx And 2) Then GoTo ex_cb118_119 Else GoTo ex_cb116_117
    
ex_cb116_117:
    If xxx = 116 Then
        ' 116 bitv6,H
        bitv &H40, Div256(regHL)
        execute_cb = 8
    Else
        ' 117 bitv6,L
        bitv &H40, (regHL And &HFF)
        execute_cb = 8
    End If
    Exit Function

ex_cb118_119:
    If xxx = 118 Then
        ' 118 bitv6,(HL)
        bitv &H40, peekb(regHL)
        execute_cb = 12
    Else
        ' 119 ' bitv6,A
        bitv &H40, regA
        execute_cb = 8
    End If
    Exit Function
    
ex_cb120_127:
    If (xxx And 4) Then GoTo ex_cb124_127 Else GoTo ex_cb120_123
    
ex_cb120_123:
    If (xxx And 2) Then GoTo ex_cb122_123 Else GoTo ex_cb120_121
    
ex_cb120_121:
    If xxx = 120 Then
        ' 120 bitv7,B
        bitv &H80, regB
        execute_cb = 8
    Else
        ' 121 bitv7,C
        bitv &H80, regC
        execute_cb = 8
    End If
    Exit Function
    
ex_cb122_123:
    If xxx = 122 Then
        ' 122 bitv7,D
        bitv &H80, Div256(regDE)
        execute_cb = 8
    Else
        ' 123 bitv7,E
        bitv &H80, getE
        execute_cb = 8
    End If
    Exit Function

ex_cb124_127:
    If (xxx And 2) Then GoTo ex_cb126_127 Else GoTo ex_cb124_125
    
ex_cb124_125:
    If xxx = 124 Then
        ' 124 bitv7,H
        bitv &H80, Div256(regHL)
        execute_cb = 8
    Else
        ' 125 bitv7,L
        bitv &H80, (regHL And &HFF)
        execute_cb = 8
    End If
    Exit Function

ex_cb126_127:
    If xxx = 126 Then
        ' 126 bitv7,(HL)
        bitv &H80, peekb(regHL)
        execute_cb = 12
    Else
        ' 127 bitv7,A
        bitv &H80, regA
        execute_cb = 8
    End If
    Exit Function
    
ex_cb128_255:
    If (xxx And 64) Then GoTo ex_cb192_255 Else GoTo ex_cb128_191
    
ex_cb128_191:
    If (xxx And 32) Then GoTo ex_cb160_191 Else GoTo ex_cb128_159
    
ex_cb128_159:
    If (xxx And 16) Then GoTo ex_cb144_159 Else GoTo ex_cb128_143
    
ex_cb128_143:
    If (xxx And 8) Then GoTo ex_cb136_143 Else GoTo ex_cb128_135
    
ex_cb128_135:
    Select Case xxx
    Case 128 ' RES 0,B
        regB = bitResv(1, regB)
        execute_cb = 8
    Case 129 ' RES 0,C
        regC = bitResv(1, regC)
        execute_cb = 8
    Case 130 ' RES 0,D
        setD bitResv(1, Div256(regDE))
        execute_cb = 8
    Case 131 ' RES 0,E
        setE bitResv(1, getE)
        execute_cb = 8
    Case 132 ' RES 0,H
        setH bitResv(1, Div256(regHL))
        execute_cb = 8
    Case 133 ' RES 0,L
        setL bitResv(1, regHL And &HFF)
        execute_cb = 8
    Case 134 ' RES 0,(HL)
        pokeb regHL, bitResv(&H1, peekb(regHL))
        execute_cb = 15
    Case 135 ' RES 0,A
        regA = bitResv(1, regA)
        execute_cb = 8
    End Select
    Exit Function
    
ex_cb136_143:
    Select Case xxx
    Case 136 ' RES 1,B
        regB = BitResv(2, regB)
        execute_cb = 8
    Case 137 ' RES 1,C
        regC = BitResv(2, regC)
        execute_cb = 8
    Case 138 ' RES 1,D
        setD BitResv(2, Div256(regDE))
        execute_cb = 8
    Case 139 ' RES 1,E
        setE BitResv(2, getE)
        execute_cb = 8
    Case 140 ' RES 1,H
        setH BitResv(2, Div256(regHL))
        execute_cb = 8
    Case 141 ' RES 1,L
        setL BitResv(2, regHL And &HFF)
        execute_cb = 8
    Case 142 ' RES 1,(HL)
        pokeb regHL, BitResv(2, peekb(regHL))
        execute_cb = 15
    Case 143 ' RES 1,A
        regA = BitResv(2, regA)
        execute_cb = 8
    End Select
    Exit Function
    
ex_cb144_159:
    Select Case xxx
    Case 144 ' RES 2,B
        regB = BitResv(4, regB)
        execute_cb = 8
    Case 145 ' RES 2,C
        regC = BitResv(4, regC)
        execute_cb = 8
    Case 146 ' RES 2,D
        setD BitResv(4, Div256(regDE))
        execute_cb = 8
    Case 147 ' RES 2,E
        setE BitResv(4, getE)
        execute_cb = 8
    Case 148 ' RES 2,H
        setH BitResv(4, Div256(regHL))
        execute_cb = 8
    Case 149 ' RES 2,L
        setL BitResv(4, regHL And &HFF)
        execute_cb = 8
    Case 150 ' RES 2,(HL)
        pokeb regHL, BitResv(4, peekb(regHL))
        execute_cb = 15
    Case 151 ' RES 2,A
        regA = BitResv(4, regA)
        execute_cb = 8



    Case 152 ' RES 3,B
        regB = BitResv(8, regB)
        execute_cb = 8
    Case 153 ' RES 3,C
        regC = BitResv(8, regC)
        execute_cb = 8
    Case 154 ' RES 3,D
        setD BitResv(8, Div256(regDE))
        execute_cb = 8
    Case 155 ' RES 3,E
        setE BitResv(8, getE)
        execute_cb = 8
    Case 156 ' RES 3,H
        setH BitResv(8, Div256(regHL))
        execute_cb = 8
    Case 157 ' RES 3,L
        setL BitResv(8, regHL And &HFF)
        execute_cb = 8
    Case 158 ' RES 3,(HL)
        pokeb regHL, BitResv(8, peekb(regHL))
        execute_cb = 15
    Case 159 ' RES 3,A
        regA = BitResv(8, regA)
        execute_cb = 8
    End Select
    Exit Function
    
ex_cb160_191:
    If (xxx And 16) Then GoTo ex_cb176_191 Else GoTo ex_cb160_175
    
ex_cb160_175:
    If (xxx And 8) Then GoTo ex_cb168_175 Else GoTo ex_cb160_167
    
ex_cb160_167:
    Select Case xxx
    Case 160 ' RES 4,B
        regB = BitResv(&H10, regB)
        execute_cb = 8
    Case 161 ' RES 4,C
        regC = BitResv(&H10, regC)
        execute_cb = 8
    Case 162 ' RES 4,D
        setD BitResv(&H10, Div256(regDE))
        execute_cb = 8
    Case 163 ' RES 4,E
        setE BitResv(&H10, getE)
        execute_cb = 8
    Case 164 ' RES 4,H
        setH BitResv(&H10, Div256(regHL))
        execute_cb = 8
    Case 165 ' RES 4,L
        setL BitResv(&H10, regHL And &HFF)
        execute_cb = 8
    Case 166 ' RES 4,(HL)
        pokeb regHL, BitResv(&H10, peekb(regHL))
        execute_cb = 15
    Case 167 ' RES 4,A
        regA = BitResv(&H10, regA)
        execute_cb = 8
    End Select
    Exit Function

ex_cb168_175:
    If (xxx And 4) Then GoTo ex_cb172_175 Else GoTo ex_cb168_171
    
ex_cb168_171:
    If (xxx And 2) Then GoTo ex_cb170_171 Else GoTo ex_cb168_169
    
ex_cb168_169:
    If xxx = 168 Then
        ' 168 RES 5,B
        regB = BitResv(&H20, regB)
        execute_cb = 8
    Else
        ' 169 RES 5,C
        regC = BitResv(&H20, regC)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb170_171:
    If xxx = 170 Then
        ' 170 RES 5,D
        setD BitResv(&H20, Div256(regDE))
        execute_cb = 8
    Else
        ' 171 RES 5,E
        setE BitResv(&H20, getE)
        execute_cb = 8
    End If
    Exit Function
    
ex_cb172_175:
    Select Case xxx
    Case 172 ' RES 5,H
        setH BitResv(&H20, Div256(regHL))
        execute_cb = 8
    Case 173 ' RES 5,L
        setL BitResv(&H20, regHL And &HFF)
        execute_cb = 8
    Case 174 ' RES 5,(HL)
        pokeb regHL, BitResv(&H20, peekb(regHL))
        execute_cb = 15
    Case 175 ' RES 5,A
        regA = BitResv(&H20, regA)
        execute_cb = 8
    End Select
    Exit Function
    
ex_cb176_191:
    Select Case xxx
    Case 176 ' RES 6,B
        regB = BitResv(&H40, regB)
        execute_cb = 8
    Case 177 ' RES 6,C
        regC = BitResv(&H40, regC)
        execute_cb = 8
    Case 178 ' RES 6,D
        setD BitResv(&H40, Div256(regDE))
        execute_cb = 8
    Case 179 ' RES 6,E
        setE BitResv(&H40, getE)
        execute_cb = 8
    Case 180 ' RES 6,H
        setH BitResv(&H40, Div256(regHL))
        execute_cb = 8
    Case 181 ' RES 6,L
        setL BitResv(&H40, regHL And &HFF)
        execute_cb = 8
    Case 182 ' RES 6,(HL)
        pokeb regHL, BitResv(&H40, peekb(regHL))
        execute_cb = 15
    Case 183 ' RES 6,A
        regA = BitResv(&H40, regA)
        execute_cb = 8


    Case 184 ' RES 7,B
        regB = BitResv(&H80, regB)
        execute_cb = 8
    Case 185 ' RES 7,C
        regC = BitResv(&H80, regC)
        execute_cb = 8
    Case 186 ' RES 7,D
        setD BitResv(&H80, Div256(regDE))
        execute_cb = 8
    Case 187 ' RES 7,E
        setE BitResv(&H80, getE)
        execute_cb = 8
    Case 188 ' RES 7,H
        setH BitResv(&H80, Div256(regHL))
        execute_cb = 8
    Case 189 ' RES 7,L
        setL BitResv(&H80, regHL And &HFF)
        execute_cb = 8
    Case 190 ' RES 7,(HL)
        pokeb regHL, BitResv(&H80, peekb(regHL))
        execute_cb = 15
    Case 191 ' RES 7,A
        regA = BitResv(&H80, regA)
        execute_cb = 8
    End Select
    Exit Function

ex_cb192_255:
    If (xxx And 32) Then GoTo ex_cb224_255 Else GoTo ex_cb192_223
    
ex_cb192_223:
    If (xxx And 16) Then GoTo ex_cb208_223 Else GoTo ex_cb192_207
    
ex_cb192_207:
    If (xxx And 8) Then GoTo ex_cb200_207 Else GoTo ex_cb192_199
    
ex_cb192_199:
    Select Case xxx
    Case 192 ' SET 0,B
        regB = BitSetV(1, regB)
        execute_cb = 8
    Case 193 ' SET 0,C
        regC = BitSetV(1, regC)
        execute_cb = 8
    Case 194 ' SET 0,D
        setD BitSetV(1, Div256(regDE))
        execute_cb = 8
    Case 195 ' SET 0,E
        setE BitSetV(1, getE)
        execute_cb = 8
    Case 196 ' SET 0,H
        setH BitSetV(1, Div256(regHL))
        execute_cb = 8
    Case 197 ' SET 0,L
        setL BitSetV(1, regHL And &HFF)
        execute_cb = 8
    Case 198 ' SET 0,(HL)
        pokeb regHL, BitSetV(1, peekb(regHL))
        execute_cb = 15
    Case 199 ' SET 0,A
        regA = BitSetV(1, regA)
        execute_cb = 8
    End Select
    Exit Function
    
ex_cb200_207:
    Select Case xxx
    Case 200 ' SET 1,B
        regB = BitSetV(2, regB)
        execute_cb = 8
    Case 201 ' SET 1,C
        regC = BitSetV(2, regC)
        execute_cb = 8
    Case 202 ' SET 1,D
        setD BitSetV(2, Div256(regDE))
        execute_cb = 8
    Case 203 ' SET 1,E
        setE BitSetV(2, getE)
        execute_cb = 8
    Case 204 ' SET 1,H
        setH BitSetV(2, Div256(regHL))
        execute_cb = 8
    Case 205 ' SET 1,L
        setL BitSetV(2, regHL And &HFF)
        execute_cb = 8
    Case 206 ' SET 1,(HL)
        pokeb regHL, BitSetV(2, peekb(regHL))
        execute_cb = 15
    Case 207 ' SET 1,A
        regA = BitSetV(2, regA)
        execute_cb = 8
    End Select
    Exit Function
    
ex_cb208_223:
    Select Case xxx
    Case 208 ' SET 2,B
        regB = BitSetV(4, regB)
        execute_cb = 8
    Case 209 ' SET 2,C
        regC = BitSetV(4, regC)
        execute_cb = 8
    Case 210 ' SET 2,D
        setD BitSetV(4, Div256(regDE))
        execute_cb = 8
    Case 211 ' SET 2,E
        setE BitSetV(4, getE)
        execute_cb = 8
    Case 212 ' SET 2,H
        setH BitSetV(4, Div256(regHL))
        execute_cb = 8
    Case 213 ' SET 2,L
        setL BitSetV(4, regHL And &HFF)
        execute_cb = 8
    Case 214 ' SET 2,(HL)
        pokeb regHL, BitSetV(&H4, peekb(regHL))
        execute_cb = 15
    Case 215 ' SET 2,A
        regA = BitSetV(4, regA)
        execute_cb = 8



    Case 216 ' SET 3,B
        regB = BitSetV(8, regB)
        execute_cb = 8
    Case 217 ' SET 3,C
        regC = BitSetV(8, regC)
        execute_cb = 8
    Case 218 ' SET 3,D
        setD BitSetV(8, Div256(regDE))
        execute_cb = 8
    Case 219 ' SET 3,E
        setE BitSetV(8, getE)
        execute_cb = 8
    Case 220 ' SET 3,H
        setH BitSetV(8, Div256(regHL))
        execute_cb = 8
    Case 221 ' SET 3,L
        setL BitSetV(8, regHL And &HFF)
        execute_cb = 8
    Case 222 ' SET 3,(HL)
        pokeb regHL, BitSetV(&H8, peekb(regHL))
        execute_cb = 15
    Case 223 ' SET 3,A
        regA = BitSetV(8, regA)
        execute_cb = 8
    End Select
    Exit Function
    
ex_cb224_255:
    If (xxx And 16) Then GoTo ex_cb240_255 Else GoTo ex_cb224_239
    
ex_cb224_239:
    Select Case xxx
    Case 224 ' SET 4,B
        regB = BitSetV(&H10, regB)
        execute_cb = 8
    Case 225 ' SET 4,C
        regC = BitSetV(&H10, regC)
        execute_cb = 8
    Case 226 ' SET 4,D
        setD BitSetV(&H10, Div256(regDE))
        execute_cb = 8
    Case 227 ' SET 4,E
        setE BitSetV(&H10, getE)
        execute_cb = 8
    Case 228 ' SET 4,H
        setH BitSetV(&H10, Div256(regHL))
        execute_cb = 8
    Case 229 ' SET 4,L
        setL BitSetV(&H10, regHL And &HFF)
        execute_cb = 8
    Case 230 ' SET 4,(HL)
        pokeb regHL, BitSetV(&H10, peekb(regHL))
        execute_cb = 15
    Case 231 ' SET 4,A
        regA = BitSetV(&H10, regA)
        execute_cb = 8



    Case 232 ' SET 5,B
        regB = BitSetV(&H20, regB)
        execute_cb = 8
    Case 233 ' SET 5,C
        regC = BitSetV(&H20, regC)
        execute_cb = 8
    Case 234 ' SET 5,D
        setD BitSetV(&H20, Div256(regDE))
        execute_cb = 8
    Case 235 ' SET 5,E
        setE BitSetV(&H20, getE)
        execute_cb = 8
    Case 236 ' SET 5,H
        setH BitSetV(&H20, Div256(regHL))
        execute_cb = 8
    Case 237 ' SET 5,L
        setL BitSetV(&H20, regHL And &HFF)
        execute_cb = 8
    Case 238 ' SET 5,(HL)
        pokeb regHL, BitSetV(&H20, peekb(regHL))
        execute_cb = 15
    Case 239 ' SET 5,A
        regA = BitSetV(&H20, regA)
        execute_cb = 8
    End Select
    Exit Function
    
ex_cb240_255:
    If (xxx And 8) Then GoTo ex_cb248_255 Else GoTo ex_cb240_247
    
ex_cb240_247:
    Select Case xxx
    Case 240 ' SET 6,B
        regB = BitSetV(&H40, regB)
        execute_cb = 8
    Case 241 ' SET 6,C
        regC = BitSetV(&H40, regC)
        execute_cb = 8
    Case 242 ' SET 6,D
        setD BitSetV(&H40, Div256(regDE))
        execute_cb = 8
    Case 243 ' SET 6,E
        setE BitSetV(&H40, getE)
        execute_cb = 8
    Case 244 ' SET 6,H
        setH BitSetV(&H40, Div256(regHL))
        execute_cb = 8
    Case 245 ' SET 6,L
        setL BitSetV(&H40, regHL And &HFF)
        execute_cb = 8
    Case 246 ' SET 6,(HL)
        pokeb regHL, BitSetV(&H40, peekb(regHL))
        execute_cb = 15
    Case 247 ' SET 6,A
        regA = BitSetV(&H40, regA)
        execute_cb = 8
    End Select
    Exit Function
    
ex_cb248_255:
    If (xxx And 4) Then GoTo ex_cb252_255 Else GoTo ex_cb248_251
    
ex_cb248_251:
    Select Case xxx
    Case 248 ' SET 7,B
        regB = BitSetV(&H80, regB)
        execute_cb = 8
    Case 249 ' SET 7,C
        regC = BitSetV(&H80, regC)
        execute_cb = 8
    Case 250 ' SET 7,D
        setD BitSetV(&H80, Div256(regDE))
        execute_cb = 8
    Case 251 ' SET 7,E
        setE BitSetV(&H80, getE)
        execute_cb = 8
    End Select
    Exit Function
    
ex_cb252_255:
    If (xxx And 2) Then GoTo ex_cb254_255 Else GoTo ex_cb252_253
    
ex_cb252_253:
    If xxx = 252 Then
        ' 252 SET 7,H
        setH BitSetV(&H80, Div256(regHL))
        execute_cb = 8
    Else
        ' 253 SET 7,L
        setL BitSetV(&H80, regHL And &HFF)
        execute_cb = 8
    End If
    Exit Function

ex_cb254_255:
    If xxx = 254 Then
        ' 254 SET 7,(HL)
        pokeb regHL, BitSetV(&H80, peekb(regHL))
        execute_cb = 15
    Else
        ' 255 SET 7,A
        regA = BitSetV(&H80, regA)
        execute_cb = 8
    End If
End Function
Function execute_ed() As Integer
    Dim xxx As integer, count As integer, dest As integer, from As Integer
    Dim c As integer, b As Integer
   
    
    
    Refresco_INTR(1)

    xxx = peekb(regPC)
    regPC = regPC + 1
    
    If (xxx And 128) Then GoTo ex_ed128_255 Else GoTo ex_ed0_127

ex_ed0_127:
    If (xxx And 64) Then
        GoTo ex_ed64_127
    Else
        ' 000 to 063 = NOP * 2
        execute_ed = 8
        Exit Function
    End If
    
ex_ed64_127:
    If (xxx And 32) Then GoTo ex_ed96_127 Else GoTo ex_ed64_95
    
ex_ed64_95:
    If (xxx And 16) Then GoTo ex_ed80_95 Else GoTo ex_ed64_79
    
ex_ed64_79:
    If (xxx And 8) Then GoTo ex_ed72_79 Else GoTo ex_ed64_71
    
ex_ed64_71:
    If (xxx And 4) Then GoTo ex_ed68_71 Else GoTo ex_ed64_67
    
ex_ed64_67:
    If (xxx And 2) Then GoTo ex_ed66_67 Else GoTo ex_ed64_65

ex_ed64_65:
    If xxx = 64 Then
        ' 064 IN B,(c)
        regB = in_bc()
        execute_ed = 12
    Else
        ' 065 OUT (c),B
        outb ((regB * 256) Or regC), regB
        execute_ed = 12
    End If
    Exit Function
    
ex_ed66_67:
    If xxx = 66 Then
        ' 066 SBC HL,BC
        regHL = sbc16(regHL, (regB * 256) Or regC)
        execute_ed = 15
    Else
        ' 067 LD (nn),BC
        pokew nxtpcw(), ((regB * 256) Or regC)
        execute_ed = 20
    End If
    Exit Function
    
ex_ed68_71:
    If (xxx And 2) Then GoTo ex_ed70_71 Else GoTo ex_ed68_69
    
ex_ed68_69:
    If xxx = 68 Then
        ' 068 NEG
        neg_a
        execute_ed = 8
    Else
        ' 069 RETn
        intIFF1 = intIFF2
        poppc
        execute_ed = 14
    End If
    Exit Function
    
ex_ed70_71:
    If xxx = 70 Then
        ' 070 IM 0
        intIM = 0
        execute_ed = 8
    Else
        ' 071 LD I,A
        intI = regA
        execute_ed = 9
    End If
    Exit Function
    
ex_ed72_79:
    If (xxx And 4) Then GoTo ex_ed76_79 Else GoTo ex_ed72_75
    
ex_ed72_75:
    If (xxx And 2) Then GoTo ex_ed74_75 Else GoTo ex_ed72_73

ex_ed72_73:
    If xxx = 72 Then
        ' 072 IN C,(c)
        regC = in_bc()
        execute_ed = 12
    Else
        ' 073 OUT (c),C
        outb ((regB * 256) Or regC), regC
        execute_ed = 12
    End If
    Exit Function
    
ex_ed74_75:
    If xxx = 74 Then
        ' 074 ADC HL,BC
        regHL = adc16(regHL, (regB * 256) Or regC)
        execute_ed = 15
    Else
        ' 075 LD BC,(nn)
        setBC peekw(nxtpcw())
        execute_ed = 20
    End If
    Exit Function
    
ex_ed76_79:
    If (xxx And 2) Then GoTo ex_ed78_79 Else GoTo ex_ed76_77
    
ex_ed76_77:
    If xxx = 76 Then
        ' 076 NEG
        neg_a
        execute_ed = 8
    Else
        ' 077 RETI
        ' TOCHECK: according to the official Z80 docs, IFF2 does not get
        '          copied to IFF1 for RETI - but in a real Z80 it is
        intIFF1 = intIFF2
        poppc
        execute_ed = 14
    End If
    Exit Function
    
ex_ed78_79:
    If xxx = 78 Then
        ' 078 IM 0
        intIM = 0
        execute_ed = 8
    Else
        ' 079 LD R,A
        intR = regA
        intRTemp = intR
        execute_ed = 9
    End If
    Exit Function
    
ex_ed80_95:
    If (xxx And 8) Then GoTo ex_ed88_95 Else GoTo ex_ed80_87
    
ex_ed80_87:
    If (xxx And 4) Then GoTo ex_ed84_87 Else GoTo ex_ed80_83
    
ex_ed80_83:
    If (xxx And 2) Then GoTo ex_ed82_83 Else GoTo ex_ed80_81
    
ex_ed80_81:
    If xxx = 80 Then
        ' 080 IN D,(c)
        setD in_bc()
        execute_ed = 12
    Else
        ' 081 OUT (c),D
        outb ((regB * 256) Or regC), Div256(regDE)
        execute_ed = 12
    End If
    Exit Function

ex_ed82_83:
    If xxx = 82 Then
        ' 082 SBC HL,DE
        regHL = sbc16(regHL, regDE)
        execute_ed = 15
    Else
        ' 083 LD (nn),DE
        pokew nxtpcw(), regDE
        execute_ed = 20
    End If
    Exit Function

ex_ed84_87:
    Select Case xxx
    Case 84 ' NEG
        neg_a
        execute_ed = 8
    Case 85 ' RETn
        intIFF1 = intIFF2
        poppc
        execute_ed = 14
    Case 86 ' IM 1
        intIM = 1
        execute_ed = 8
    Case 87 ' LD A,I
        ld_a_i
        execute_ed = 9
    End Select
    Exit Function
    
ex_ed88_95:
    If (xxx And 4) Then GoTo ex_ed92_95 Else GoTo ex_ed88_91
    
ex_ed88_91:
    If (xxx And 2) Then GoTo ex_ed90_91 Else GoTo ex_ed88_89
    
ex_ed88_89:
    If xxx = 88 Then
        ' 088 IN E,(c)
        setE in_bc()
        execute_ed = 12
    Else
        ' 089 OUT (c),E
        outb ((regB * 256) Or regC), getE
        execute_ed = 12
    End If
    Exit Function
    
ex_ed90_91:
    If xxx = 90 Then
        ' 090 ADC HL,DE
        regHL = adc16(regHL, regDE)
        execute_ed = 15
    Else
        ' 091 LD DE,(nn)
        regDE = peekw(nxtpcw())
        execute_ed = 20
    End If
    Exit Function
    
ex_ed92_95:
    Select Case xxx
    Case 92 ' NEG
        neg_a
        execute_ed = 8
    Case 93 ' RETI
        ' en un Z80 real FF2 es copiado en FF1 (pero no esta verificado)
        intIFF1 = intIFF2
        poppc
        execute_ed = 14
    Case 94 ' IM 2
        intIM = 2
        execute_ed = 8
    Case 95 ' LD A,R
        ld_a_r
        execute_ed = 9
    End Select
    Exit Function
    
ex_ed96_127:
    If (xxx And 16) Then GoTo ex_ed112_127 Else GoTo ex_ed96_111
    
ex_ed96_111:
    If (xxx And 8) Then GoTo ex_ed104_111 Else GoTo ex_ed96_103
    
ex_ed96_103:
    Select Case xxx
    Case 96 ' IN H,(c)
        setH in_bc()
        execute_ed = 12
    Case 97 ' OUT (c),H
        outb ((regB * 256) Or regC), Div256(regHL)
        execute_ed = 12
    Case 98 ' SBC HL,HL
        regHL = sbc16(regHL, regHL)
        execute_ed = 15
    Case 99 ' LD (nn),HL
        pokew nxtpcw(), regHL
        execute_ed = 20
    Case 100 ' NEG
        neg_a
        execute_ed = 8
    Case 101 ' RETn
        intIFF1 = intIFF2
        poppc
        execute_ed = 14
    Case 102 ' IM 0
        intIM = 0
        execute_ed = 8
    Case 103 ' RRD
        rrd_a
        execute_ed = 18
    End Select
    Exit Function
    
ex_ed104_111:
    Select Case xxx
    Case 104 ' IN L,(c)
        setL in_bc()
        execute_ed = 12
    Case 105 ' OUT (c),L
        outb ((regB * 256) Or regC), regHL And &HFF
        execute_ed = 12
    Case 106 ' ADC HL,HL
        regHL = adc16(regHL, regHL)
        execute_ed = 15
    Case 107 ' LD HL,(nn)
        regHL = peekw(nxtpcw())
        execute_ed = 20
    Case 108 ' NEG
        neg_a
        execute_ed = 8
    Case 109 ' RETI
        ' en un Z80 real FF2 es copiado en FF1 (pero no esta verificado)
        intIFF1 = intIFF2
        poppc
        execute_ed = 14
    Case 110 ' IM 0
        intIM = 0
        execute_ed = 8
    Case 111  ' RLD
        rld_a
        execute_ed = 18
    End Select
    Exit Function
    
ex_ed112_127:
    If (xxx And 8) Then GoTo ex_ed120_127 Else GoTo ex_ed112_119
    
ex_ed112_119:
    Select Case xxx
    Case 112 ' IN (c)
        in_bc
        execute_ed = 12
    Case 113 ' OUT (c),0
        outb ((regB * 256) Or regC), 0
        execute_ed = 12
    Case 114 ' SBC HL,SP
        regHL = sbc16(regHL, regSP)
        execute_ed = 15
    Case 115 ' LD (nn),SP
        pokew nxtpcw(), regSP
        execute_ed = 20
    Case 116 ' NEG
        neg_a
        execute_ed = 8
    Case 117 ' RETn
        intIFF1 = intIFF2
        poppc
        execute_ed = 14
    Case 118 ' IM 1
        intIM = 1
        execute_ed = 8
    Case 119
        ' Undocumented NOP * 2
        execute_ed = 8
    End Select
    Exit Function
    
ex_ed120_127:
    Select Case xxx
    Case 120 ' IN A,(c)
        regA = in_bc
        execute_ed = 12
    Case 121 ' OUT (c),A
        outb ((regB * 256) Or regC), regA
        execute_ed = 12
    Case 122 ' ADC HL,SP
        regHL = adc16(regHL, regSP)
        execute_ed = 15
    Case 123 ' LD SP,(nn)
        regSP = peekw(nxtpcw())
        execute_ed = 20
    Case 124 ' NEG
        neg_a
        execute_ed = 8
    Case 125 ' RETI
        ' en un Z80 real FF2 es copiado en FF1 (pero no esta verificado)
        intIFF1 = intIFF2
        poppc
        execute_ed = 14
    Case 126 ' IM 2
        intIM = 2
        execute_ed = 8
    Case 127 ' NOP
        execute_ed = 8
    End Select
    Exit Function
    
ex_ed128_255:
    If (xxx And 64) Then GoTo ex_ed192_255 Else GoTo ex_ed128_191
    
ex_ed128_191:
    If (xxx And 32) Then
        GoTo ex_ed160_191
    Else
        ' NOP * 2 (128 to 159)
        execute_ed = 8
        Exit Function
    End If

ex_ed160_191:
    Select Case xxx
    ' xxI
    Case 160 ' LDI
        pokeb regDE, peekb(regHL)
        regDE = (regDE + 1) And &HFFFF
        regHL = (regHL + 1) And &HFFFF
        setBC dec16((regB * 256) Or regC)

        fPV = (((regB * 256) Or regC) <> 0)
        fH = False
        fN = False

        execute_ed = 16
    Case 161 ' CPI
        c = fC
        cp_a peekb(regHL)
        cp_a peekb(regHL)
        
        regHL = (regHL + 1) And &HFFFF
        setBC dec16((regB * 256) Or regC)

        fPV = (((regB * 256) Or regC) <> 0)
        fC = c

        execute_ed = 16
    Case 162 ' INI
        pokeb regHL, inb((regB * 256) Or regC)
        b = qdec8(regB)
        regB = b
        regHL = (regHL + 1) And &HFFFF

        fZ = (b = 0)
        fN = True

        execute_ed = 16
    Case 163 ' OUTI
        b = qdec8(regB)
        regB = b
        outb ((regB * 256) Or regC), peekb(regHL)
        regHL = (regHL + 1) And &HFFFF

        fZ = (b = 0)
        fN = True

        execute_ed = 16
    
    ' /* xxD */
    Case 168 ' LDD
        pokeb regDE, peekb(regHL)
        regDE = dec16(regDE)
        regHL = dec16(regHL)
        setBC dec16((regB * 256) Or regC)

        fPV = (((regB * 256) Or regC) <> 0)
        fH = False
        fN = False

        execute_ed = 16
    Case 169 ' CPD
        c = fC

        cp_a peekb(regHL)
        regHL = dec16(regHL)
        setBC dec16((regB * 256) Or regC)

        fPV = (((regB * 256) Or regC) <> 0)
        fC = c

        execute_ed = 16
    Case 170 ' IND
        pokeb regHL, inb((regB * 256) Or regC)
        b = qdec8(regB)
        regB = b
        regHL = dec16(regHL)

        fZ = (b = 0)
        fN = True

        execute_ed = 16
    Case 171 ' OUTD
        count = qdec8(regB)
        regB = count
        outb ((regB * 256) Or regC), peekb(regHL)
        regHL = dec16(regHL)

        fZ = (count = 0)
        fN = True

        execute_ed = 16

    ' xxIR
    Case 176 ' LDIR
        b = peekb(regHL)
        pokeb regDE, b

        regHL = (regHL + 1) And &HFFFF
        regDE = (regDE + 1) And &HFFFF
        setBC ((regB * 256) Or regC) - 1
        fH = False
        fN = False
        f3 = (b And F_3) <> 0
        f5 = (b And F_5) <> 0

        If ((regB * 256) Or regC) <> 0 Then
            regPC = regPC - 2
            fPV = True
            execute_ed = 21
        Else
            fPV = False
            execute_ed = 16
        End If
      
    Case 177 ' CPIR
        c = fC
        
        b = peekb(regHL)
        cp_a b
        regHL = (regHL + 1) And &HFFFF
        setBC dec16((regB * 256) Or regC)
        
        fC = c
        
        f3 = (b And F_3) <> 0
        f5 = (b And F_5) <> 0
        
        c = ((regB * 256) Or regC) <> 0
        fPV = c
        If (fPV) And (fZ = False) Then
            regPC = regPC - 2
            execute_ed = 21
        Else
            execute_ed = 16
        End If
    Case 178 ' INIR
        pokeb regHL, inb((regB * 256) Or regC)
        b = qdec8(regB)
        regB = b
        regHL = (regHL + 1) And &HFFFF

        fZ = True
        fN = True
        If (b <> 0) Then
            regPC = regPC - 2
            execute_ed = 21
        Else
            execute_ed = 16
        End If
    Case 179 ' OTIR
        b = qdec8(regB)
        regB = b
        outb ((regB * 256) Or regC), peekb(regHL)
        regHL = (regHL + 1) And &HFFFF

        fZ = True
        fN = True
        If (b <> 0) Then
            regPC = regPC - 2
            execute_ed = 21
        Else
            execute_ed = 16
        End If

    ' xxDR
    Case 184 ' LDDR
        b = peekb(regHL)
        pokeb regDE, b

        regHL = dec16(regHL)
        regDE = dec16(regDE)
        setBC ((regB * 256) Or regC) - 1
        fH = False
        fN = False
        f3 = (b And F_3) <> 0
        f5 = (b And F_5) <> 0

        If ((regB * 256) Or regC) <> 0 Then
            regPC = regPC - 2
            fPV = True
            execute_ed = 21
        Else
            fPV = False
            execute_ed = 16
        End If
        

    Case 185 ' CPDR
        c = fC
        
        b = peekb(regHL)
        cp_a b
        regHL = dec16(regHL)
        setBC dec16((regB * 256) Or regC)

        fPV = ((regB * 256) Or regC) <> 0
        fC = c
        
        f3 = (b And F_3) <> 0
        f5 = (b And F_5) <> 0
        
        If (fPV) And (fZ = False) Then
            regPC = regPC - 2
            execute_ed = 21
        Else
            execute_ed = 16
        End If
    Case 186 ' INDR
        pokeb regHL, inb((regB * 256) Or regC)
        b = qdec8(regB)
        regB = b
        regHL = dec16(regHL)

        fZ = True
        fN = True
        If (b <> 0) Then
            regPC = regPC - 2
            execute_ed = 21
        Else
            execute_ed = 16
        End If
    Case 187 ' OTDR
        b = qdec8(regB)
        regB = b
        outb ((regB * 256) Or regC), peekb(regHL)
        regHL = dec16(regHL)

        fZ = True
        fN = True
        If (b <> 0) Then
            regPC = regPC - 2
            execute_ed = 21
        Else
            execute_ed = 16
        End If
    Case 187 To 191
        msg "Instruccion ED desconocida "+Str(xxx)+" en "+Str(regPC),1
        execute_ed = 8
    Case Else ' (164 To 167, 172 To 175, 180 To 183)
        msg "Instruccion ED desconocida "+Str(xxx)+" en "+Str(regPC),1
        execute_ed = 8
        
    End Select
    Exit Function
    
ex_ed192_255:
    msg "Instruccion ED desconocida "+Str(xxx)+" en "+Str(regPC),1
    execute_ed = 8
End Function

Sub execute_id_cb(op As integer, ByVal z As Integer)
    If (op And 128) Then GoTo ex_id_cb128_255 Else GoTo ex_id_cb0_127
    
ex_id_cb0_127:
    If (op And 64) Then GoTo ex_id_cb64_127 Else GoTo ex_id_cb0_63
    
ex_id_cb0_63:
    If (op And 32) Then GoTo ex_id_cb32_63 Else GoTo ex_id_cb0_31
    
ex_id_cb0_31:
    If (op And 16) Then GoTo ex_id_cb16_31 Else GoTo ex_id_cb0_15
    
ex_id_cb0_15:
    If (op And 8) Then GoTo ex_id_cb8_15 Else GoTo ex_id_cb0_7
    
ex_id_cb0_7:
    If (op And 4) Then GoTo ex_id_cb4_7 Else GoTo ex_id_cb0_3
    
ex_id_cb0_3:
    If (op And 2) Then GoTo ex_id_cb2_3 Else GoTo ex_id_cb0_1
    
ex_id_cb0_1:
    If op = 0 Then
        ' 000 RLC B
        op = rlc(peekb(z))
        regB = op
        pokeb z, op
    Else
        ' 001 RLC C
        op = rlc(peekb(z))
        regC = op
        pokeb z, op
    End If
    Exit Sub

ex_id_cb2_3:
    If op = 2 Then
        ' 002 RLC D
        op = rlc(peekb(z))
        setD op
        pokeb z, op
    Else
        ' 003 RLC E
        op = rlc(peekb(z))
        setE op
        pokeb z, op
    End If
    Exit Sub
    
ex_id_cb4_7:
    If (op And 2) Then GoTo ex_id_cb6_7 Else GoTo ex_id_cb4_5
    
ex_id_cb4_5:
    If op = 4 Then
        ' 004 RLC H
        op = rlc(peekb(z))
        setH op
        pokeb z, op
    Else
        ' 005 RLC L
        op = rlc(peekb(z))
        setL op
        pokeb z, op
    End If
    Exit Sub
    
ex_id_cb6_7:
    If op = 6 Then
        ' 006 RLC (HL)
        pokeb z, rlc(peekb(z))
    Else
        ' 007 RLC A
        op = rlc(peekb(z))
        regA = op
        pokeb z, op
    End If
    Exit Sub

ex_id_cb8_15:
    If (op And 4) Then GoTo ex_id_cb12_15 Else GoTo ex_id_cb8_11
    
ex_id_cb8_11:
    If (op And 2) Then GoTo ex_id_cb10_11 Else GoTo ex_id_cb8_9
    
ex_id_cb8_9:
    If op = 8 Then
        ' 008 RRC B
        op = rrc(peekb(z))
        regB = op
        pokeb z, op
    Else
        ' 009 RRC C
        op = rrc(peekb(z))
        regC = op
        pokeb z, op
    End If
    Exit Sub

ex_id_cb10_11:
    If op = 10 Then
        ' 010 RRC D
        op = rrc(peekb(z))
        setD op
        pokeb z, op
    Else
        ' 011 RRC E
        op = rrc(peekb(z))
        setE op
        pokeb z, op
    End If
    Exit Sub
    
ex_id_cb12_15:
    If (op And 2) Then GoTo ex_id_cb14_15 Else GoTo ex_id_cb12_13
    
ex_id_cb12_13:
    If op = 12 Then
        ' 012 RRC H
        op = rrc(peekb(z))
        setH op
        pokeb z, op
    Else
        ' 013 RRC L
        op = rrc(peekb(z))
        setL op
        pokeb z, op
    End If
    Exit Sub
    
ex_id_cb14_15:
    If op = 14 Then
        ' 014 RRC (HL)
        pokeb z, rrc(peekb(z))
    Else
        ' 015 RRC A
        op = rrc(peekb(z))
        regA = op
        pokeb z, op
    End If
    Exit Sub

ex_id_cb16_31:
    Select Case op
    Case 16 ' RL B
        op = rl(peekb(z))
        regB = op
        pokeb z, op
    Case 17 ' RL C
        op = rl(peekb(z))
        regC = op
        pokeb z, op
    Case 18 ' RL D
        op = rl(peekb(z))
        setD op
        pokeb z, op
    Case 19 ' RL E
        op = rl(peekb(z))
        setE op
        pokeb z, op
    Case 20 ' RL H
        op = rl(peekb(z))
        setH op
        pokeb z, op
    Case 21 ' RL L
        op = rl(peekb(z))
        setL op
        pokeb z, op
    Case 22 ' RL (HL)
        pokeb z, rl(peekb(z))
    Case 23 ' RL A
        op = rl(peekb(z))
        regA = op
        pokeb z, op
    Case 24 ' RR B
        op = rr(peekb(z))
        regB = op
        pokeb z, op
    Case 25 ' RR C
        op = rr(peekb(z))
        regC = op
        pokeb z, op
    Case 26 ' RR D
        op = rr(peekb(z))
        setD op
        pokeb z, op
    Case 27 ' RR E
        op = rr(peekb(z))
        setE op
        pokeb z, op
    Case 28 ' RR H
        op = rr(peekb(z))
        setH op
        pokeb z, op
    Case 29 ' RR L
        op = rr(peekb(z))
        setL op
        pokeb z, op
    Case 30 ' RR (HL)
        pokeb z, rr(peekb(z))
    Case 31 ' RR A
        op = rr(peekb(z))
        regA = op
        pokeb z, op
    End Select
    Exit Sub
    
ex_id_cb32_63:
    Select Case op
    Case 32 ' SLA B
        op = sla(peekb(z))
        regB = op
        pokeb z, op
    Case 33 ' SLA C
        op = sla(peekb(z))
        regC = op
        pokeb z, op
    Case 34 ' SLA D
        op = sla(peekb(z))
        setD op
        pokeb z, op
    Case 35 ' SLA E
        op = sla(peekb(z))
        setE op
        pokeb z, op
    Case 36 ' SLA H
        op = sla(peekb(z))
        setH op
        pokeb z, op
    Case 37 ' SLA L
        op = sla(peekb(z))
        setL op
        pokeb z, op
    Case 38 ' SLA (HL)
        pokeb z, sla(peekb(z))
    Case 39 ' SLA A
        op = sla(peekb(z))
        regA = op
        pokeb z, op
    Case 40 ' SRA B
        op = sra(peekb(z))
        regB = op
        pokeb z, op
    Case 41 ' SRA C
        op = sra(peekb(z))
        regC = op
        pokeb z, op
    Case 42 ' SRA D
        op = sra(peekb(z))
        setD op
        pokeb z, op
    Case 43 ' SRA E
        op = sra(peekb(z))
        setE op
        pokeb z, op
    Case 44 ' SRA H
        op = sra(peekb(z))
        setH op
        pokeb z, op
    Case 45 ' SRA L
        op = sra(peekb(z))
        setL op
        pokeb z, op
    Case 46 ' SRA (HL)
        pokeb z, sra(peekb(z))
    Case 47 ' SRA A
        op = sra(peekb(z))
        regA = op
        pokeb z, op
    Case 48 ' SLS B
        op = sls(peekb(z))
        regB = op
        pokeb z, op
    Case 49 ' SLS C
        op = sls(peekb(z))
        regC = op
        pokeb z, op
    Case 50 ' SLS D
        op = sls(peekb(z))
        setD op
        pokeb z, op
    Case 51 ' SLS E
        op = sls(peekb(z))
        setE op
        pokeb z, op
    Case 52 ' SLS H
        op = sls(peekb(z))
        setH op
        pokeb z, op
    Case 53 ' SLS L
        op = sls(peekb(z))
        setL op
        pokeb z, op
    Case 54 ' SLS (HL)
        pokeb z, sls(peekb(z))
    Case 55 ' SLS A
        op = sls(peekb(z))
        regA = op
        pokeb z, op
    Case 56 ' SRL B
        op = srl(peekb(z))
        regB = op
        pokeb z, op
    Case 57 ' SRL C
        op = srl(peekb(z))
        regC = op
        pokeb z, op
    Case 58 ' SRL D
        op = srl(peekb(z))
        setD op
        pokeb z, op
    Case 59 ' SRL E
        op = srl(peekb(z))
        setE op
        pokeb z, op
    Case 60 ' SRL H
        op = srl(peekb(z))
        setH op
        pokeb z, op
    Case 61 ' SRL L
        op = srl(peekb(z))
        setL op
        pokeb z, op
    Case 62 ' SRL (ID)
        pokeb z, srl(peekb(z))
    Case 63 ' SRL A
        op = srl(peekb(z))
        regA = op
        pokeb z, op
    End Select
    Exit Sub

ex_id_cb64_127:
    If (op And 32) Then GoTo ex_id_cb96_127 Else GoTo ex_id_cb64_95
    
ex_id_cb64_95:
    If (op And 16) Then GoTo ex_id_cb80_95 Else GoTo ex_id_cb64_79
    
ex_id_cb64_79:
    If (op And 8) Then
        ' 072 to 079 bitv1,B
        bitv &H2, peekb(z)
    Else
        ' 064 to 071 bitv0,B
        bitv &H1, peekb(z)
    End If
    Exit Sub
    
ex_id_cb80_95:
    If (op And 8) Then
        ' 088 to 095 bitv3,B
        bitv &H8, peekb(z)
    Else
        ' 080 To 087 bitv2,B
        bitv &H4, peekb(z)
    End If
    Exit Sub
    
ex_id_cb96_127:
    If (op And 16) Then GoTo ex_id_cb112_127 Else GoTo ex_id_cb96_111
    
ex_id_cb96_111:
    If (op And 8) Then
        ' 104 to 111 bitv5,B
        bitv &H20, peekb(z)
    Else
        ' 096 to 103 bitv4,B
        bitv &H10, peekb(z)
    End If
    Exit Sub
    
ex_id_cb112_127:
    If (op And 8) Then
        ' 120 to 127 bitv7,B
        bitv &H80, peekb(z)
    Else
        ' 112 To 119 bitv6,B
        bitv &H40, peekb(z)
    End If
    Exit Sub

ex_id_cb128_255:
    If (op And 64) Then GoTo ex_id_cb192_255 Else GoTo ex_id_cb128_191
    
ex_id_cb128_191:
    Select Case op
    Case 128 ' RES 0,(ID+y)->B
        regB = BitResv(1, peekb(z))
        pokeb z, regB
    Case 129 ' RES 0,(ID+y)->C
        regC = BitResv(1, peekb(z))
        pokeb z, regC
    Case 130 ' RES 0,(ID+y)->D
        setD BitResv(1, peekb(z))
        pokeb z, Div256(regDE)
    Case 131 ' RES 0,(ID+y)->E
        setE BitResv(1, peekb(z))
        pokeb z, getE
    Case 132 ' RES 0,(ID+y)->H
        setH BitResv(1, peekb(z))
        pokeb z, Div256(regHL)
    Case 133 ' RES 0,(ID+y)->L
        setL BitResv(1, peekb(z))
        pokeb z, regHL And &HFF
    Case 134 ' RES 0,(HL)
        pokeb z, BitResv(&H1, peekb(z))
    Case 135 ' RES 0,(ID+y)->A
        regA = BitResv(1, peekb(z))
        pokeb z, regA
    Case 136 ' RES 1,(ID+y)->B
        regB = BitResv(2, peekb(z))
        pokeb z, regB
    Case 137 ' RES 1,(ID+y)->C
        regC = BitResv(2, peekb(z))
        pokeb z, regC
    Case 138 ' RES 1,(ID+y)->D
        setD BitResv(2, peekb(z))
        pokeb z, Div256(regDE)
    Case 139 ' RES 1,(ID+y)->E
        setE BitResv(2, peekb(z))
        pokeb z, getE
    Case 140 ' RES 1,(ID+y)->H
        setH BitResv(2, peekb(z))
        pokeb z, Div256(regHL)
    Case 141 ' RES 1,(ID+y)->L
        setL BitResv(2, peekb(z))
        pokeb z, getL
    Case 142 ' RES 1,(HL)
        pokeb z, BitResv(&H2, peekb(z))
    Case 143 ' RES 1,(ID+y)->A
        regA = BitResv(2, peekb(z))
        pokeb z, regA
    Case 144 ' RES 2,(ID+y)->B
        regB = BitResv(4, peekb(z))
        pokeb z, regB
    Case 145 ' RES 2,(ID+y)->C
        regC = BitResv(4, peekb(z))
        pokeb z, regC
    Case 146 ' RES 2,(ID+y)->D
        setD BitResv(4, peekb(z))
        pokeb z, Div256(regDE)
    Case 147 ' RES 2,(ID+y)->E
        setE BitResv(4, peekb(z))
        pokeb z, getE
    Case 148 ' RES 2,(ID+y)->H
        setH BitResv(4, peekb(z))
        pokeb z, Div256(regHL)
    Case 149 ' RES 2,(ID+y)->L
        setL BitResv(4, peekb(z))
        pokeb z, getL
    Case 150 ' RES 2,(HL)
        pokeb z, BitResv(&H4, peekb(z))
    Case 151 ' RES 2,(ID+y)->A
        regA = BitResv(4, peekb(z))
        pokeb z, regA
    Case 152 ' RES 3,(ID+y)->B
        regB = BitResv(8, peekb(z))
        pokeb z, regB
    Case 153 ' RES 3,(ID+y)->C
        regC = BitResv(8, peekb(z))
        pokeb z, regC
    Case 154 ' RES 3,(ID+y)->D
        setD BitResv(8, peekb(z))
        pokeb z, Div256(regDE)
    Case 155 ' RES 3,(ID+y)->E
        setE BitResv(8, peekb(z))
        pokeb z, getE
    Case 156 ' RES 3,(ID+y)->H
        setH BitResv(8, peekb(z))
        pokeb z, Div256(regHL)
    Case 157 ' RES 3,(ID+y)->L
        setL BitResv(8, peekb(z))
        pokeb z, getL
    Case 158 ' RES 3,(HL)
        pokeb z, BitResv(&H8, peekb(z))
    Case 159 ' RES 3,(ID+y)->A
        regA = BitResv(8, peekb(z))
        pokeb z, regA
    Case 160 ' RES 4,(ID+y)->B
        regB = BitResv(&H10, peekb(z))
        pokeb z, regB
    Case 161 ' RES 4,(ID+y)->C
        regC = BitResv(&H10, peekb(z))
        pokeb z, regC
    Case 162 ' RES 4,(ID+y)->D
        setD BitResv(&H10, peekb(z))
        pokeb z, Div256(regDE)
    Case 163 ' RES 4,(ID+y)->E
        setE BitResv(&H10, peekb(z))
        pokeb z, getE
    Case 164 ' RES 4,(ID+y)->H
        setH BitResv(&H10, peekb(z))
        pokeb z, Div256(regHL)
    Case 165 ' RES 4,(ID+y)->L
        setL BitResv(&H10, peekb(z))
        pokeb z, getL
    Case 166 ' RES 4,(HL)
        pokeb z, BitResv(&H10, peekb(z))
    Case 167 ' RES 4,(ID+y)->A
        regA = BitResv(&H10, peekb(z))
        pokeb z, regA
    Case 168 ' RES 5,(ID+y)->B
        regB = BitResv(&H20, peekb(z))
        pokeb z, regB
    Case 169 ' RES 5,(ID+y)->C
        regC = BitResv(&H20, peekb(z))
        pokeb z, regC
    Case 170 ' RES 5,(ID+y)->D
        setD BitResv(&H20, peekb(z))
        pokeb z, Div256(regDE)
    Case 171 ' RES 5,(ID+y)->E
        setE BitResv(&H20, peekb(z))
        pokeb z, getE
    Case 172 ' RES 5,(ID+y)->H
        setH BitResv(&H20, peekb(z))
        pokeb z, Div256(regHL)
    Case 173 ' RES 5,(ID+y)->L
        setL BitResv(&H20, peekb(z))
        pokeb z, getL
    Case 174 ' RES 5,(HL)
        pokeb z, BitResv(&H20, peekb(z))
    Case 175 ' RES 5,(ID+y)->A
        regA = BitResv(&H20, peekb(z))
        pokeb z, regA
    Case 176 ' RES 6,(ID+y)->B
        regB = BitResv(&H40, peekb(z))
        pokeb z, regB
    Case 177 ' RES 6,(ID+y)->C
        regC = BitResv(&H40, peekb(z))
        pokeb z, regC
    Case 178 ' RES 6,(ID+y)->D
        setD BitResv(&H40, peekb(z))
        pokeb z, Div256(regDE)
    Case 179 ' RES 6,(ID+y)->E
        setE BitResv(&H40, peekb(z))
        pokeb z, getE
    Case 180 ' RES 6,(ID+y)->H
        setH BitResv(&H40, peekb(z))
        pokeb z, Div256(regHL)
    Case 181 ' RES 6,(ID+y)->L
        setL BitResv(&H40, peekb(z))
        pokeb z, getL
    Case 182 ' RES 6,(HL)
        pokeb z, BitResv(&H40, peekb(z))
    Case 183 ' RES 6,(ID+y)->A
        regA = BitResv(&H40, peekb(z))
        pokeb z, regA
    Case 184 ' RES 6,(ID+y)->B
        regB = BitResv(&H80, peekb(z))
        pokeb z, regB
    Case 185 ' RES 6,(ID+y)->C
        regC = BitResv(&H80, peekb(z))
        pokeb z, regC
    Case 186 ' RES 6,(ID+y)->D
        setD BitResv(&H80, peekb(z))
        pokeb z, Div256(regDE)
    Case 187 ' RES 6,(ID+y)->E
        setE BitResv(&H80, peekb(z))
        pokeb z, getE
    Case 188 ' RES 6,(ID+y)->H
        setH BitResv(&H80, peekb(z))
        pokeb z, Div256(regHL)
    Case 189 ' RES 6,(ID+y)->L
        setL BitResv(&H80, peekb(z))
        pokeb z, getL
    Case 190 ' RES 7,(HL)
        pokeb z, BitResv(&H80, peekb(z))
    Case 191 ' RES 7,(ID+y)->A
        regA = BitResv(&H80, peekb(z))
        pokeb z, regA
    End Select
    Exit Sub
    
ex_id_cb192_255:
    Select Case op
    Case 192 ' SET 0,(ID+y)->B
        regB = BitSetV(1, peekb(z))
        pokeb z, regB
    Case 193 ' SET 0,(ID+y)->C
        regC = BitSetV(1, peekb(z))
        pokeb z, regC
    Case 194 ' SET 0,(ID+y)->D
        setD BitSetV(1, peekb(z))
        pokeb z, Div256(regDE)
    Case 195 ' SET 0,(ID+y)->E
        setE BitSetV(1, peekb(z))
        pokeb z, getE
    Case 196 ' SET 0,(ID+y)->H
        setH BitSetV(1, peekb(z))
        pokeb z, Div256(regHL)
    Case 197 ' SET 0,(ID+y)->L
        setL BitSetV(1, peekb(z))
        pokeb z, getL
    Case 198 ' SET 0,(HL)
        pokeb z, BitSetV(&H1, peekb(z))
    Case 199 ' SET 0,(ID+y)->A
        regA = BitSetV(1, peekb(z))
        pokeb z, regA
    Case 200 ' SET 1,(ID+y)->B
        regB = BitSetV(2, peekb(z))
        pokeb z, regB
    Case 201 ' SET 1,(ID+y)->C
        regC = BitSetV(2, peekb(z))
        pokeb z, regC
    Case 202 ' SET 1,(ID+y)->D
        setD BitSetV(2, peekb(z))
        pokeb z, Div256(regDE)
    Case 203 ' SET 1,(ID+y)->E
        setE BitSetV(2, peekb(z))
        pokeb z, getE
    Case 204 ' SET 1,(ID+y)->H
        setH BitSetV(2, peekb(z))
        pokeb z, Div256(regHL)
    Case 205 ' SET 1,(ID+y)->L
        setL BitSetV(2, peekb(z))
        pokeb z, getL
    Case 206 ' SET 1,(HL)
        pokeb z, BitSetV(&H2, peekb(z))
    Case 207 ' SET 1,(ID+y)->A
        regA = BitSetV(2, peekb(z))
        pokeb z, regA
    Case 208 ' SET 2,(ID+y)->B
        regB = BitSetV(4, peekb(z))
        pokeb z, regB
    Case 209 ' SET 2,(ID+y)->C
        regC = BitSetV(4, peekb(z))
        pokeb z, regC
    Case 210 ' SET 2,(ID+y)->D
        setD BitSetV(4, peekb(z))
        pokeb z, Div256(regDE)
    Case 211 ' SET 2,(ID+y)->E
        setE BitSetV(4, peekb(z))
        pokeb z, getE
    Case 212 ' SET 2,(ID+y)->H
        setH BitSetV(4, peekb(z))
        pokeb z, Div256(regHL)
    Case 213 ' SET 2,(ID+y)->L
        setL BitSetV(4, peekb(z))
        pokeb z, getL
    Case 214 ' SET 2,(HL)
        pokeb z, BitSetV(&H4, peekb(z))
    Case 215 ' SET 2,(ID+y)->A
        regA = BitSetV(4, peekb(z))
        pokeb z, regA
    Case 216 ' SET 3,(ID+y)->B
        regB = BitSetV(8, peekb(z))
        pokeb z, regB
    Case 217 ' SET 3,(ID+y)->C
        regC = BitSetV(8, peekb(z))
        pokeb z, regC
    Case 218 ' SET 3,(ID+y)->D
        setD BitSetV(8, peekb(z))
        pokeb z, Div256(regDE)
    Case 219 ' SET 3,(ID+y)->E
        setE BitSetV(8, peekb(z))
        pokeb z, getE
    Case 220 ' SET 3,(ID+y)->H
        setH BitSetV(8, peekb(z))
        pokeb z, Div256(regHL)
    Case 221 ' SET 3,(ID+y)->L
        setL BitSetV(8, peekb(z))
        pokeb z, getL
    Case 222 ' SET 3,(HL)
        pokeb z, BitSetV(&H8, peekb(z))
    Case 223 ' SET 3,(ID+y)->A
        regA = BitSetV(8, peekb(z))
        pokeb z, regA
    Case 224 ' SET 4,(ID+y)->B
        regB = BitSetV(&H10, peekb(z))
        pokeb z, regB
    Case 225 ' SET 4,(ID+y)->C
        regC = BitSetV(&H10, peekb(z))
        pokeb z, regC
    Case 226 ' SET 4,(ID+y)->D
        setD BitSetV(&H10, peekb(z))
        pokeb z, Div256(regDE)
    Case 227 ' SET 4,(ID+y)->E
        setE BitSetV(&H10, peekb(z))
        pokeb z, getE
    Case 228 ' SET 4,(ID+y)->H
        setH BitSetV(&H10, peekb(z))
        pokeb z, Div256(regHL)
    Case 229 ' SET 4,(ID+y)->L
        setL BitSetV(&H10, peekb(z))
        pokeb z, getL
    Case 230 ' SET 4,(HL)
        pokeb z, BitSetV(&H10, peekb(z))
    Case 231 ' SET 4,(ID+y)->A
        regA = BitSetV(&H10, peekb(z))
        pokeb z, regA
    Case 232 ' SET 5,(ID+y)->B
        regB = BitSetV(&H20, peekb(z))
        pokeb z, regB
    Case 233 ' SET 5,(ID+y)->C
        regC = BitSetV(&H20, peekb(z))
        pokeb z, regC
    Case 234 ' SET 5,(ID+y)->D
        setD BitSetV(&H20, peekb(z))
        pokeb z, Div256(regDE)
    Case 235 ' SET 5,(ID+y)->E
        setE BitSetV(&H20, peekb(z))
        pokeb z, getE
    Case 236 ' SET 5,(ID+y)->H
        setH BitSetV(&H20, peekb(z))
        pokeb z, Div256(regHL)
    Case 237 ' SET 5,(ID+y)->L
        setL BitSetV(&H20, peekb(z))
        pokeb z, getL
    Case 238 ' SET 5,(HL)
        pokeb z, BitSetV(&H20, peekb(z))
    Case 239 ' SET 5,(ID+y)->A
        regA = BitSetV(&H20, peekb(z))
        pokeb z, regA
    Case 240 ' SET 6,(ID+y)->B
        regB = BitSetV(&H40, peekb(z))
        pokeb z, regB
    Case 241 ' SET 6,(ID+y)->C
        regC = BitSetV(&H40, peekb(z))
        pokeb z, regC
    Case 242 ' SET 6,(ID+y)->D
        setD BitSetV(&H40, peekb(z))
        pokeb z, Div256(regDE)
    Case 243 ' SET 6,(ID+y)->E
        setE BitSetV(&H40, peekb(z))
        pokeb z, getE
    Case 244 ' SET 6,(ID+y)->H
        setH BitSetV(&H40, peekb(z))
        pokeb z, Div256(regHL)
    Case 245 ' SET 6,(ID+y)->L
        setL BitSetV(&H40, peekb(z))
        pokeb z, getL
    Case 246 ' SET 6,(HL)
        pokeb z, BitSetV(&H40, peekb(z))
    Case 247 ' SET 6,(ID+y)->A
        regA = BitSetV(&H40, peekb(z))
        pokeb z, regA
    Case 248 ' SET 7,(ID+y)->B
        regB = BitSetV(&H80, peekb(z))
        pokeb z, regB
    Case 249 ' SET 7,(ID+y)->C
        regC = BitSetV(&H80, peekb(z))
        pokeb z, regC
    Case 250 ' SET 7,(ID+y)->D
        setD BitSetV(&H80, peekb(z))
        pokeb z, Div256(regDE)
    Case 251 ' SET 7,(ID+y)->E
        setE BitSetV(&H80, peekb(z))
        pokeb z, getE
    Case 252 ' SET 7,(ID+y)->H
        setH BitSetV(&H80, peekb(z))
        pokeb z, Div256(regHL)
    Case 253 ' SET 7,(ID+y)->L
        setL BitSetV(&H80, peekb(z))
        pokeb z, getL
    Case 254 ' SET 7,(HL)
        pokeb z, BitSetV(&H80, peekb(z))
    Case 255 ' SET 7,A
        regA = BitSetV(&H80, peekb(z))
        pokeb z, regA
    End Select
End Sub

Sub exx()
    Dim t As Integer
    
    t = regHL
    regHL = regHL_
    regHL_ = t
    
    t = regDE
    regDE = regDE_
    regDE_ = t
    
    t = ((regB * 256) Or regC)
    setBC regBC_
    regBC_ = t
End Sub

 Function getAF() As Integer
    getAF = (regA * 256) Or getF
End Function

Function getBC() As Integer
    getBC = (regB * 256) Or regC
End Function

Function getD() As Integer
    getD = Div256(regDE)
End Function
Function getE() As Integer
    getE = regDE And &HFF
End Function

Function getF() As Integer
    getF = (-(fS)  * F_S)  Or _
           (-(fZ)  * F_Z)  Or _
           (-(f5)  * F_5)  Or _
           (-(fH)  * F_H)  Or _
           (-(f3)  * F_3)  Or _
           (-(fPV) * F_PV) Or _
           (-(fN)  * F_N)  Or _
           (-(fC)  * F_C)
'    If fS Then getF = getF Or F_S
'    If fZ Then getF = getF Or F_Z
'    If f5 Then getF = getF Or F_5
'    If fH Then getF = getF Or F_H
'    If f3 Then getF = getF Or F_3
'    If fPV Then getF = getF Or F_PV
'    If fN Then getF = getF Or F_N
'    If fC Then getF = getF Or F_C
End Function

Function getH() As Integer
    getH = Div256(regHL)
End Function

Function getIDH() As Integer
    getIDH = Div256(regID)
End Function
Function getIDL() As Integer
    getIDL = regID And &HFF
End Function


Function getL() As Integer
    getL = regHL And &HFF
End Function

Function id_d() As Integer
    Dim d As Integer
    
    d = nxtpcb()
    If d And 128 Then d = -(256 - d)
    id_d = (regID + d) And &HFFFF
End Function

Sub ld_a_i()
    fS = (intI And F_S) <> 0
    f3 = (intI And F_3) <> 0
    f5 = (intI And F_5) <> 0
    fZ = (intI = 0)
    fPV = intIFF2
    fH = False
    fN = False
    regA = intI
End Sub

Sub ld_a_r()
    intRTemp = intRTemp And &H7F
    regA = (intR And &H80) Or intRTemp
    fS = (regA And F_S) <> 0
    f3 = (regA And F_3) <> 0
    f5 = (regA And F_5) <> 0
    fZ = (regA = 0)
    fPV = intIFF2
    fH = False
    fN = False
End Sub

Sub neg_a()
    Dim t As Integer
    
    t = regA
    regA = 0
    sub_a t
End Sub

Sub setIDH(byteval As Integer)
    regID = ((byteval * 256) And &HFF00) Or (regID And &HFF)
End Sub

Sub setIDL(byteval As Integer)
    regID = (regID And &HFF00) Or (byteval And &HFF)
End Sub

Function in_bc() As Integer
    Dim ans As Integer
    
    ans = inb((regB * 256) Or regC)
    
    fZ = (ans = 0)
    fS = (ans And F_S) <> 0
    f3 = (ans And F_3) <> 0
    f5 = (ans And F_5) <> 0
    fPV = Parity(ans)
    fN = False
    fH = False
    
    in_bc = ans
End Function

Function inc8(ByVal ans As integer) As Integer
    fPV = (ans = &H7F)
    fH = (((ans And &HF) + 1) And F_H) <> 0
    
    ans = (ans + 1) And &HFF

    fS = (ans And F_S) <> 0
    f3 = (ans And F_3) <> 0
    f5 = (ans And F_5) <> 0
    fZ = (ans = 0)
    fN = False
    
    inc8 = ans
End Function

Function dec8(ByVal ans As integer) As Integer
    fPV = (ans = &H80)
    fH = (((ans And &HF) - 1) And F_H) <> 0
    
    ans = (ans - 1) And &HFF
    
    fS = (ans And F_S) <> 0
    f3 = (ans And F_3) <> 0
    f5 = (ans And F_5) <> 0
    fZ = (ans = 0)
    
    fN = True
    
    dec8 = ans
End Function

Sub or_a(b As Integer)
    regA = (regA Or b)
    
    fS = (regA And F_S) <> 0
    f3 = (regA And F_3) <> 0
    f5 = (regA And F_5) <> 0
    fH = False
    fPV = Parity(regA)
    fZ = (regA = 0)
    fN = False
    fC = False
End Sub

Function rl(ans As integer) As Integer
    Dim c As Integer
    Dim rl_Aux as Integer

    c = (ans And &H80) <> 0
    
    rl_Aux = ((ans * 2) Or -(fC)) And &HFF
    
    fS = (rl_Aux and F_S) <> 0
    f3 = (rl_Aux and F_3) <> 0
    f5 = (rl_Aux and F_5) <> 0
    fZ = (rl_Aux = 0)

    fPV = Parity(rl_Aux)
    fH = False
    fN = False
    fC = c
    rl = rl_Aux

End Function

Sub rl_a()
    Dim c As Integer
    
    c = (regA And &H80) <> 0
    
    regA = ((regA * 2) Or -(fC)) And &HFF
    
    f3 = (regA And F_3) <> 0
    f5 = (regA And F_5) <> 0
    fN = False
    fH = False
    fC = c
End Sub

Function rlc(ans As integer) As Integer
    Dim rlc_Aux as Integer

    fC = (ans And &H80) <> 0
    
    rlc_aux = ((ans * 2) Or -(fC)) And &HFF

    fS = (rlc_Aux And F_S) <> 0
    f3 = (rlc_Aux And F_3) <> 0
    f5 = (rlc_Aux And F_5) <> 0
    fZ = (rlc_Aux = 0)
    fPV = Parity(rlc_Aux)
    fH = False
    fN = False
    rlc = rlc_Aux
End Function

Sub rlc_a()
    fC = (regA And &H80) <> 0
    
    regA = ((regA * 2) Or -(fC)) And &HFF
    
    f3 = (regA And F_3) <> 0
    f5 = (regA And F_5) <> 0
    fN = False
    fH = False
End Sub

Sub rld_a()
    Dim t As integer, q As Integer
    
    t = peekb(regHL)
    q = t
    t = (t * 16) Or (regA And &HF)
    regA = (regA And &HF0) Or (q \ 16)
    pokeb regHL, (t And &HFF)

    fS = (regA And F_S) <> 0
    f3 = (regA And F_3) <> 0
    f5 = (regA And F_5) <> 0
    fZ = (regA = 0)
    fPV = intIFF2
    fH = False
    fN = False
End Sub

Function rr(ans As integer) As Integer
    Dim c As Integer
    Dim rr_Aux as Integer
    
    c = (ans And &H1) <> 0
    
    rr_aux = (ans \ 2) Or (&H80 * -(fC))

    fS = (rr_Aux And F_S) <> 0
    f3 = (rr_Aux And F_3) <> 0
    f5 = (rr_Aux And F_5) <> 0
    fZ = (rr_Aux = 0)
    fPV = Parity(rr_Aux)
    fH = False
    fN = False
    fC = c
    rr = rr_Aux
End Function

Sub rr_a()
    Dim c As Integer
    
    c = (regA And &H1) <> 0
    
    regA = (regA \ 2) Or (&H80 * -(fC))

    f3 = (regA And F_3) <> 0
    f5 = (regA And F_5) <> 0
    fN = False
    fH = False
    fC = c
End Sub

Function rrc(ans As integer) As Integer
    Dim rrc_Aux as Integer

    fC = (ans And &H1) <> 0
    
    rrc_aux = (ans \ 2) Or (&H80 * -(fC))
    
    fS = (rrc_Aux And F_S) <> 0
    f3 = (rrc_Aux And F_3) <> 0
    f5 = (rrc_Aux And F_5) <> 0
    fZ = (rrc_Aux = 0)
    fPV = Parity(rrc_Aux)
    fH = False
    fN = False
    rrc = rrc_Aux
End Function

Sub rrd_a()
    Dim t As integer, q As Integer
    
    t = peekb(regHL)
    q = t

    t = (t \ 16) Or (regA * 16)
    regA = (regA And &HF0) Or (q And &HF)
    pokeb regHL, t

    fS = (regA And F_S) <> 0
    f3 = (regA And F_3) <> 0
    f5 = (regA And F_5) <> 0
    fZ = (regA = 0)
    fPV = intIFF2
    fH = False
    fN = False
End Sub

Sub rrc_a()
    fC = (regA And &H1) <> 0
    regA = (regA \ 2) Or (&H80 * -(fC))

    f3 = (regA And F_3) <> 0
    f5 = (regA And F_5) <> 0
    fN = False
    fH = False
End Sub

Sub sbc_a(b As Integer)
    Dim ans As integer, c As Integer
    
    c = -(fC)

    ans = (regA - b - c) And &HFF
    
    fS = (ans And F_S) <> 0
    f3 = (ans And F_3) <> 0
    f5 = (ans And F_5) <> 0
    fZ = (ans = 0)
    fC = ((regA - b - c) And &H100) <> 0
    fPV = ((regA Xor b) And (regA Xor ans) And &H80) <> 0
    fH = (((regA And &HF) - (b And &HF) - c) And F_H) <> 0
    fN = True
    
    regA = ans
End Sub

Function sbc16(a As integer, b As integer) As Integer
    Dim c As integer, ans As Integer
    
    c = -(fC)
    
    ans = (a - b - c) And &HFFFF
    
    fS = (ans And (F_S * 256)) <> 0
    f3 = (ans And (F_3 * 256)) <> 0
    f5 = (ans And (F_5 * 256)) <> 0
    fZ = (ans = 0)
    fC = ((a - b - c) And &H10000) <> 0
    fPV = ((a Xor b) And (a Xor ans) And &H8000) <> 0
    fH = (((a And &HFFF) - (b And &HFFF) - c) And &H1000) <> 0
    fN = True
    
    sbc16 = ans
End Function

Sub scf()
    f3 = (regA And F_3) <> 0
    f5 = (regA And F_5) <> 0
    fN = False
    fH = False
    fC = True
End Sub

Sub setAF(v As Integer)
    regA = Div256(v And &HFF00)
    setF (v And &HFF)
End Sub

Sub setBC(nn As Integer)
    regB = Div256(nn And &HFF00)
    regC = nn And &HFF
End Sub

Sub execute()
    Dim d As integer, lTemp As Integer, tecla As integer
    Dim xxx As integer, lScanLine As integer, lPrevScanLine As Integer
    Dim lTStates As Integer 'guarda los estados "gastados" en cada ejecucion de INS para descontar luego al total GLTS
    
    ' asignamos el total de ciclos a gastar, para ir descontando hasta 0
    WastedStates = -TotalStates

    While TRUE
    	'Locate 21,21: Print WastedStates;"      "
        If (WastedStates >= 0) Then
            ' dispara interrupcion
            WastedStates = WastedStates - TotalStates - interrupt()
        End If
   
        Refresco_INTR(1)

      If dibujar>100 Then
        dibujar=0
        ponpantalla
      End If
      dibujar+=1 ' por cada INS ejecutada, descontamos una linea de refresco de pantalla
      
		' una forma de parar en una direccion....
		'IF REGPC=&h1f2 THEN DEB=1:BreakPoint=1
		' por ahora, no llega nunca. se queda en el CB CALL SCAN y no sale sin pulsar tecla
		'IF REGPC=&hce THEN DEB=1:BreakPoint=1 ' 261 ce
		' bucle ente 252 y 25f, no llega nunca a 261

       if deb then 
         if MultiKey(SC_F1) then BreakPoint=1 ' entrar en depuracion
         if MultiKey(SC_F2) then BreakPoint=0 ' salir de depuracion
         If MultiKey(SC_ESCAPE) Then End
         
         locate 1,1
         print "PC="+hex(regpc)+" Byte:"+hex(peekb(regpc))+"  Word+1:"+Hex(peekw(regpc+1))+"    "
         print "SP="+Hex(regsp)+"    "
         print
         print "A ="+Hex(regA)+"    "
         print "B ="+Hex(regB)+"    "
         print "C ="+Hex(regC)+"    "
         print "DE="+Hex(regDE)+"    "
         print "HL="+Hex(regHL)+"    "
         print
         print "AF'="+hex(regAF_)+"    ","IX="+Hex(regIX)+"    "
         print "BC'="+hex(regBC_)+"    ","IY="+Hex(regIY)+"    "
         print "DE'="+hex(regDE_)+"    ","ID="+Hex(regID)+"    "
         print "HL'="+hex(regHL_)+"    "
         Print " C N P 3 H 5 Z S"
         Print fC;fN;fPV;f3;fH;f5;fZ;fS
         ScreenCopy
         If BreakPoint=1 then Sleep
       end if

        xxx = peekb(regPC)
        regPC = regPC + 1

        
        If (xxx And 128) Then GoTo ex128_255 Else GoTo ex0_127
ex0_127:
        If (xxx And 64) Then GoTo ex64_127 Else GoTo ex0_63
ex0_63:
        If (xxx And 32) Then GoTo ex32_63 Else GoTo ex0_31
ex0_31:
        If (xxx And 16) Then GoTo ex16_31 Else GoTo ex0_15
ex0_15:
        If (xxx And 8) Then GoTo ex8_15 Else GoTo ex0_7
        
ex0_7:
        If (xxx And 4) Then GoTo ex4_7 Else GoTo ex0_3
        
ex0_3:
        If (xxx And 2) Then GoTo ex2_3 Else GoTo ex0_1

ex0_1:
        If xxx = 0 Then
            ' 000 NOP
            lTStates = 4
        Else
            ' 001 LD BC,nn
            setBC nxtpcw()
            lTStates = 10
        End If
        GoTo execute_end
        
ex2_3:
        If xxx = 2 Then
            ' 002 LD (BC),A
            pokeb ((regB * 256) Or regC), regA
            lTStates = 7
        Else
            ' 003 INC BC
            setBC (((regB * 256) Or regC) + 1 And &HFFFF)
            lTStates = 6
        End If
        GoTo execute_end
        
ex4_7:
    If (xxx And 2) Then GoTo ex6_7 Else GoTo ex4_5
    
ex4_5:
        If xxx = 4 Then
            ' 004 INC B
            regB = inc8(regB)
            lTStates = 4
        Else
            ' 005 DEC B
            regB = dec8(regB)
            lTStates = 4
        End If
        GoTo execute_end
    
ex6_7:
        If xxx = 6 Then
            ' 006 LD B,n
            regB = nxtpcb()
            lTStates = 7
        Else
            ' 007 RLCA
            rlc_a
            lTStates = 4
        End If
        GoTo execute_end

ex8_15:
        If (xxx And 4) Then GoTo ex12_15 Else GoTo ex8_11
        
ex8_11:
        If (xxx And 2) Then GoTo ex10_11 Else GoTo ex8_9
        
ex8_9:
        If xxx = 8 Then
            ' 008 EX AF,AF'
            ex_af_af
            lTStates = 4
        Else
            '009 ADD HL,BC
            regHL = add16(regHL, (regB * 256) Or regC)
            lTStates = 11
        End If
        GoTo execute_end
        
ex10_11:
        If xxx = 10 Then
            ' 010 LD A,(BC)
            regA = peekb((regB * 256) Or regC)
            lTStates = 7
        Else
            ' 011 DEC BC
            setBC dec16((regB * 256) Or regC)
            lTStates = 6
        End If
        GoTo execute_end
        
ex12_15:
        If (xxx And 2) Then GoTo ex14_15 Else GoTo ex12_13
        
ex12_13:
        If xxx = 12 Then
            ' 012 INC C
            regC = inc8(regC)
            lTStates = 4
        Else
            ' 013 DEC C
            regC = dec8(regC)
            lTStates = 4
        End If
        GoTo execute_end
        
ex14_15:
        If xxx = 14 Then
            ' 014 LD C,n
            regC = nxtpcb()
            lTStates = 7
        Else
            ' 015 RRCA
            rrc_a
            lTStates = 4
        End If
        GoTo execute_end
            
ex16_31:
        If (xxx And 8) Then GoTo ex24_31 Else GoTo ex16_23
        
ex16_23:
        If (xxx And 4) Then GoTo ex20_23 Else GoTo ex16_19
        
ex16_19:
        If (xxx And 2) Then GoTo ex18_19 Else GoTo ex16_17
        
ex16_17:
        If xxx = 16 Then
            ' 016 DJNZ dis
            regB = (regB - 1) And &HFF
            
            If regB <> 0 Then
                d = nxtpcb()
                If d And 128 Then d = -(256 - d)
                regPC = (regPC + d) And &HFFFF
                lTStates = 13
            Else
                regPC = (regPC + 1) And &HFFFF
                lTStates = 8
            End If
        Else
            ' 017 LD DE,nn
            regDE = nxtpcw()
            lTStates = 10
        End If
        GoTo execute_end
        
ex18_19:
        If xxx = 18 Then
            ' 018 LD (DE),A
            pokeb regDE, regA
            lTStates = 7
        Else
            ' 019 INC DE
            regDE = (regDE + 1) And &HFFFF
            lTStates = 6
        End If
        GoTo execute_end
        
ex20_23:
        If (xxx And 2) Then GoTo ex22_23 Else GoTo ex20_21
        
ex20_21:
        If xxx = 20 Then
        ' 020 INC D
            setD inc8(Div256(regDE))
            lTStates = 4
        Else
        ' 021 DEC D
            setD dec8(Div256(regDE))
            lTStates = 4
        End If
        GoTo execute_end
        
ex22_23:
        If xxx = 22 Then
            ' 022 LD D,n
            setD nxtpcb()
            lTStates = 7
        Else
            ' 023 ' RLA
            rl_a
            lTStates = 4
        End If
        GoTo execute_end
        
ex24_31:
        If (xxx And 4) Then GoTo ex28_31 Else GoTo ex24_27
        
ex24_27:
        If (xxx And 2) Then GoTo ex26_27 Else GoTo ex24_25
        
ex24_25:
        If xxx = 24 Then
        ' 024 JR dis
            d = nxtpcb()
            If d And 128 Then d = -(256 - d)
            regPC = (regPC + d) And &HFFFF
            lTStates = 12
        Else
            ' 025 ADD HL,DE
            regHL = add16(regHL, regDE)
            lTStates = 11
        End If
        GoTo execute_end
        
ex26_27:
        If xxx = 26 Then
            ' 026 LD A,(DE)
            regA = peekb(regDE)
            lTStates = 7
        Else
            ' 027 DEC DE
            regDE = dec16(regDE)
            lTStates = 6
        End If
        GoTo execute_end
        
ex28_31:
        If (xxx And 2) Then GoTo ex30_31 Else GoTo ex28_29

ex28_29:
        If xxx = 28 Then
            ' 028 INC E
            setE inc8(getE)
            lTStates = 4
        Else
            ' 029 DEC E
            setE dec8(getE)
            lTStates = 4
        End If
        GoTo execute_end

ex30_31:
        If xxx = 30 Then
            ' 030 LD E,n
            setE nxtpcb()
            lTStates = 7
        Else
            ' 031 RRA
            rr_a
            lTStates = 4
        End If
        GoTo execute_end
        
ex32_63:
        If (xxx And 16) Then GoTo ex48_63 Else GoTo ex32_47
        
ex32_47:
        If (xxx And 8) Then GoTo ex40_47 Else GoTo ex32_39
        
ex32_39:
        If (xxx And 4) Then GoTo ex36_39 Else GoTo ex32_35
        
ex32_35:
        If (xxx And 2) Then GoTo ex34_35 Else GoTo ex32_33
        
ex32_33:
        If xxx = 32 Then
            ' 032 JR NZ dis
            If fZ = False Then
                d = nxtpcb()
                If d And 128 Then d = -(256 - d)
                regPC = ((regPC + d) And &HFFFF)
                lTStates = 12
            Else
                regPC = (regPC + 1) And &HFFFF
                lTStates = 7
            End If
        Else
            ' 033 LD HL,nn
            regHL = nxtpcw()
            lTStates = 10
        End If
        GoTo execute_end
        
ex34_35:
        If xxx = 34 Then
            ' 034 LD (nn),HL
            pokew nxtpcw(), regHL
            lTStates = 16
        Else
            ' 035 INC HL
            regHL = (regHL + 1) And &HFFFF
            lTStates = 6
        End If
        GoTo execute_end
        
ex36_39:
        If (xxx And 2) Then GoTo ex38_39 Else GoTo ex36_37
        
ex36_37:
        If xxx = 36 Then
            ' 036 INC H
            setH inc8(Div256(regHL))
            lTStates = 4
        Else
            ' 037 DEC H
            setH dec8(Div256(regHL))
            lTStates = 4
        End If
        GoTo execute_end
        
ex38_39:
        If xxx = 38 Then
            ' 038 LD H,n
            setH nxtpcb()
            lTStates = 7
        Else
            ' 039 DAA
            daa_a
            lTStates = 4
        End If
        GoTo execute_end
        
ex40_47:
        If (xxx And 4) Then GoTo ex44_47 Else GoTo ex40_43

ex40_43:
        If (xxx And 2) Then GoTo ex42_43 Else GoTo ex40_41
        
ex40_41:
        If xxx = 40 Then
            ' 040 JR Z dis
            If fZ = True Then
                d = nxtpcb()
                If d And 128 Then d = -(256 - d)
                regPC = ((regPC + d) And &HFFFF)
                lTStates = 12
            Else
                regPC = (regPC + 1) And &HFFFF
                lTStates = 7
            End If
        Else
            ' 041 ADD HL,HL
            regHL = add16(regHL, regHL)
            lTStates = 11
        End If
        GoTo execute_end
        
ex42_43:
        If xxx = 42 Then
            ' 042 LD HL,(nn)
            regHL = peekw(nxtpcw())
            lTStates = 16
        Else
            ' 043 DEC HL
            regHL = dec16(regHL)
            lTStates = 6
        End If
        GoTo execute_end
        
ex44_47:
        If (xxx And 2) Then GoTo ex46_47 Else GoTo ex44_45
        
ex44_45:
        If xxx = 44 Then
            ' 044 INC L
            setL inc8(regHL And &HFF)
            lTStates = 4
        Else
            ' 045 DEC L
            setL dec8(regHL And &HFF)
            lTStates = 4
        End If
        GoTo execute_end
        
ex46_47:
        If xxx = 46 Then
            ' 046 LD L,n
            setL nxtpcb()
            lTStates = 7
        Else
            ' 047 CPL
            cpl_a
            lTStates = 4
        End If
        GoTo execute_end
        
ex48_63:
        If (xxx And 8) Then GoTo ex56_63 Else GoTo ex48_55
        
ex48_55:
        If (xxx And 4) Then GoTo ex52_55 Else GoTo ex48_51
        
ex48_51:
        If (xxx And 2) Then GoTo ex50_51 Else GoTo ex48_49
        
ex48_49:
        If xxx = 48 Then
            ' 048 JR NC dis
            If fC = False Then
                d = nxtpcb()
                If d And 128 Then d = -(256 - d)
                regPC = ((regPC + d) And &HFFFF)
                lTStates = 12
            Else
                regPC = (regPC + 1) And &HFFFF
                lTStates = 7
            End If
        Else
            ' 049 LD SP,nn
            regSP = nxtpcw()
            lTStates = 10
        End If
        GoTo execute_end
        
ex50_51:
        If xxx = 50 Then
            ' 050 LD (nn),A
            pokeb nxtpcw, regA
            lTStates = 13
        Else
            ' 051 INC SP
            regSP = (regSP + 1) And &HFFFF
            lTStates = 6
        End If
        GoTo execute_end
        
ex52_55:
        If (xxx And 2) Then GoTo ex54_55 Else GoTo ex52_53
        
ex52_53:
        If xxx = 52 Then
            ' 052 INC (HL)
            pokeb regHL, inc8(peekb(regHL))
            lTStates = 11
        Else
            ' 053 DEC (HL)
            pokeb regHL, dec8(peekb(regHL))
            lTStates = 11
        End If
        GoTo execute_end
        
ex54_55:
        If xxx = 54 Then
            ' 054 LD (HL),n
            pokeb regHL, nxtpcb()
            lTStates = 10
        Else
            ' 055 SCF
            scf
            lTStates = 4
        End If
        GoTo execute_end
        
ex56_63:
        If (xxx And 4) Then GoTo ex60_63 Else GoTo ex56_59
        
ex56_59:
        If (xxx And 2) Then GoTo ex58_59 Else GoTo ex56_57

ex56_57:
        If xxx = 56 Then
            ' 056 JR C dis
            If fC = True Then
                d = nxtpcb()
                If d And 128 Then d = -(256 - d)
                regPC = ((regPC + d) And &HFFFF)
                lTStates = 12
            Else
                regPC = (regPC + 1) And &HFFFF
                lTStates = 7
            End If
        Else
            ' 057 ADD HL,SP
            regHL = add16(regHL, regSP)
            lTStates = 11
        End If
        GoTo execute_end
        
ex58_59:
        If xxx = 58 Then
            ' 058 LD A,(nn)
            regA = peekb(nxtpcw())
            lTStates = 13
        Else
            ' 059 DEC SP
            regSP = dec16(regSP)
            lTStates = 6
        End If
        GoTo execute_end
        
ex60_63:
        If (xxx And 2) Then GoTo ex62_63 Else GoTo ex60_61
        
ex60_61:
        If xxx = 60 Then
            ' 060 INC A
            regA = inc8(regA)
            lTStates = 4
        Else
            ' 061 DEC A
            regA = dec8(regA)
            lTStates = 4
        End If
        GoTo execute_end
        
ex62_63:
        If xxx = 62 Then
            ' 062 LD A,n
            regA = nxtpcb()
            lTStates = 7
        Else
            ' 063 CCF
            ccf
            lTStates = 4
        End If
        GoTo execute_end
        
ex64_127:
        If (xxx And 32) Then GoTo ex96_127 Else GoTo ex64_95
        
ex64_95:
        If (xxx And 16) Then GoTo ex80_95 Else GoTo ex64_79

ex64_79:
        If (xxx And 8) Then GoTo ex72_79 Else GoTo ex64_71

ex64_71:
        If (xxx And 4) Then GoTo ex68_71 Else GoTo ex64_67

ex64_67:
        If (xxx And 2) Then GoTo ex66_67 Else GoTo ex64_65

ex64_65:
        If xxx = 64 Then
            ' LD B,B
            lTStates = 4
        Else
            ' 65 ' LD B,C
            regB = regC
            lTStates = 4
        End If
        GoTo execute_end

ex66_67:
        If xxx = 66 Then
            ' LD B,D
            regB = Div256(regDE)
            lTStates = 4
        Else
            ' 67 ' LD B,E
            regB = getE
            lTStates = 4
        End If
        GoTo execute_end

ex68_71:
        If (xxx And 2) Then GoTo ex70_71 Else GoTo ex68_69

ex68_69:
        If xxx = 68 Then
             ' LD B,H
            regB = Div256(regHL)
            lTStates = 4
        Else
            ' 69 ' LD B,L
            regB = (regHL And &HFF)
            lTStates = 4
        End If
        GoTo execute_end

ex70_71:
        If xxx = 70 Then
            ' LD B,(HL)
            regB = peekb(regHL)
            lTStates = 7
        Else
            ' 71 ' LD B,A
            regB = regA
            lTStates = 4
        End If
        GoTo execute_end

ex72_79:
        If (xxx And 4) Then GoTo ex76_79 Else GoTo ex72_75
        
ex72_75:
        If (xxx And 2) Then GoTo ex74_75 Else GoTo ex72_73
        
ex72_73:
        If xxx = 72 Then
            ' 72 ' LD C,B
            regC = regB
            lTStates = 4
        Else
            ' 73 ' LD C,C
            lTStates = 4
        End If
        GoTo execute_end
        
ex74_75:
        If xxx = 74 Then
            ' 74 ' LD C,D
            regC = Div256(regDE)
            lTStates = 4
        Else
            ' 75 ' LD C,E
            regC = getE
            lTStates = 4
        End If
        GoTo execute_end
        
ex76_79:
        If (xxx And 2) Then GoTo ex78_79 Else GoTo ex76_77
        
ex76_77:
        If xxx = 76 Then
            ' 76 ' LD C,H
            regC = Div256(regHL)
            lTStates = 4
        Else
            ' 77 ' LD C,L
            regC = (regHL And &HFF)
            lTStates = 4
        End If
        GoTo execute_end
        
ex78_79:
        If xxx = 78 Then
            ' 78 ' LD C,(HL)
            regC = peekb(regHL)
            lTStates = 7
        Else
            ' 79 ' LD C,A
            regC = regA
            lTStates = 4
        End If
        GoTo execute_end
        
ex80_95:
        If (xxx And 8) Then GoTo ex88_95 Else GoTo ex80_87
        
ex80_87:
        If (xxx And 4) Then GoTo ex84_87 Else GoTo ex80_83
        
ex80_83:
        If (xxx And 2) Then GoTo ex82_83 Else GoTo ex80_81
        
ex80_81:
        If xxx = 80 Then
            ' 80 ' LD D,B
            setD regB
            lTStates = 4
        Else
            ' 81 ' LD D,C
            setD regC
            lTStates = 4
        End If
        GoTo execute_end
        
ex82_83:
        If xxx = 82 Then
            ' 82 ' LD D,D
            lTStates = 4
        Else
            ' 83 ' LD D,E
            setD getE
            lTStates = 4
        End If
        GoTo execute_end
        
ex84_87:
        If (xxx And 2) Then GoTo ex86_87 Else GoTo ex84_85
        
ex84_85:
        If xxx = 84 Then
            ' 84 ' LD D,H
            setD Div256(regHL)
            lTStates = 4
        Else
            ' 85 ' LD D,L
            setD (regHL And &HFF)
            lTStates = 4
        End If
        GoTo execute_end

ex86_87:
        If xxx = 86 Then
            ' 86 ' LD D,(HL)
            setD peekb(regHL)
            lTStates = 7
        Else
            ' 87 ' LD D,A
            setD regA
            lTStates = 4
        End If
        GoTo execute_end

ex88_95:
        If (xxx And 4) Then GoTo ex92_95 Else GoTo ex88_91
        
ex88_91:
        If (xxx And 2) Then GoTo ex90_91 Else GoTo ex88_89
        
ex88_89:
        If xxx = 88 Then
            ' 88 ' LD E,B
            setE regB
            lTStates = 4
        Else
            ' 89 ' LD E,C
            setE regC
            lTStates = 4
        End If
        GoTo execute_end
        
ex90_91:
        If xxx = 90 Then
            ' 90 ' LD E,D
            setE Div256(regDE)
            lTStates = 4
        Else
            ' 91 ' LD E,E
            lTStates = 4
        End If
        GoTo execute_end
        
ex92_95:
        If (xxx And 2) Then GoTo ex94_95 Else GoTo ex92_93
        
ex92_93:
        If xxx = 92 Then
            ' 92 ' LD E,H
            setE Div256(regHL)
            lTStates = 4
        Else
            ' 93 ' LD E,L
            setE (regHL And &HFF)
            lTStates = 4
        End If
        GoTo execute_end
        
ex94_95:
        If xxx = 94 Then
            ' 94 ' LD E,(HL)
            setE peekb(regHL)
            lTStates = 7
        Else
            ' 95 ' LD E,A
            setE regA
            lTStates = 4
        End If
        GoTo execute_end

ex96_127:
        If (xxx And 16) Then GoTo ex112_127 Else GoTo ex96_111
        
ex96_111:
        If (xxx And 8) Then GoTo ex104_111 Else GoTo ex96_103
        
ex96_103:
        If (xxx And 4) Then GoTo ex100_103 Else GoTo ex96_99
        
ex96_99:
        If (xxx And 2) Then GoTo ex98_99 Else GoTo ex96_97
        
ex96_97:
        If xxx = 96 Then
            ' 96 ' LD H,B
            setH regB
            lTStates = 4
        Else
            ' 97 ' LD H,C
            setH regC
            lTStates = 4
        End If
        GoTo execute_end
        
ex98_99:
        If xxx = 98 Then
            ' 98 ' LD H,D
            setH Div256(regDE)
            lTStates = 4
        Else
            ' 99 ' LD H,E
            setH getE
            lTStates = 4
        End If
        GoTo execute_end
        
ex100_103:
        If (xxx And 2) Then GoTo ex102_103 Else GoTo ex100_101
        
ex100_101:
        If xxx = 100 Then
            ' 100 ' LD H,H
            lTStates = 4
        Else
            ' 101 ' LD H,L
            setH (regHL And &HFF)
            lTStates = 4
        End If
        GoTo execute_end
        
ex102_103:
        If xxx = 102 Then
            ' 102 ' LD H,(HL)
            setH peekb(regHL)
            lTStates = 7
        Else
            ' 103 ' LD H,A
            setH regA
            lTStates = 4
        End If
        GoTo execute_end
        
ex104_111:
        If (xxx And 4) Then GoTo ex108_111 Else GoTo ex104_107
        
ex104_107:
        If (xxx And 2) Then GoTo ex106_107 Else GoTo ex104_105
        
ex104_105:
        If xxx = 104 Then
            ' 104 ' LD L,B
            setL regB
            lTStates = 4
        Else
            ' 105 ' LD L,C
            setL regC
            lTStates = 4
        End If
        GoTo execute_end
        
ex106_107:
        If xxx = 106 Then
            ' 106 ' LD L,D
            setL Div256(regDE)
            lTStates = 4
        Else
            ' 107 ' LD L,E
            setL getE
            lTStates = 4
        End If
        GoTo execute_end
        
ex108_111:
        If (xxx And 2) Then GoTo ex110_111 Else GoTo ex108_109
        
ex108_109:
        If xxx = 108 Then
            ' 108 ' LD L,H
            setL Div256(regHL)
            lTStates = 4
        Else
            ' 109 ' LD L,L
            lTStates = 4
        End If
        GoTo execute_end
        
ex110_111:
        If xxx = 110 Then
            ' 110 ' LD L,(HL)
            setL peekb(regHL)
            lTStates = 7
        Else
            ' 111 ' LD L,A
            setL regA
            lTStates = 4
        End If
        GoTo execute_end
        
ex112_127:
        If (xxx And 8) Then GoTo ex120_127 Else GoTo ex112_119
        
ex112_119:
        If (xxx And 4) Then GoTo ex116_119 Else GoTo ex112_115
        
ex112_115:
        If (xxx And 2) Then GoTo ex114_115 Else GoTo ex112_113
        
ex112_113:
        If xxx = 112 Then
            ' 112 ' LD (HL),B
            pokeb regHL, regB
            lTStates = 7
        Else
            ' 113 ' LD (HL),C
            pokeb regHL, regC
            lTStates = 7
        End If
        GoTo execute_end
        
ex114_115:
        If xxx = 114 Then
            ' 114 ' LD (HL),D
            pokeb regHL, Div256(regDE)
            lTStates = 7
        Else
            ' 115 ' LD (HL),E
            pokeb regHL, getE
            lTStates = 7
        End If
        GoTo execute_end
        
ex116_119:
        If (xxx And 2) Then GoTo ex118_119 Else GoTo ex116_117
        
ex116_117:
        If xxx = 116 Then
            ' 116 ' LD (HL),H
            pokeb regHL, Div256(regHL)
            lTStates = 7
        Else
            ' 117 ' LD (HL),L
            pokeb regHL, (regHL And &HFF)
            lTStates = 7
        End If
        GoTo execute_end
        
ex118_119:
        If xxx = 118 Then
            ' 118 ' HALT
            lTStates = 4
        Else
            ' 119 ' LD (HL),A
            pokeb regHL, regA
            lTStates = 7
        End If
        GoTo execute_end
        
ex120_127:
        If (xxx And 4) Then GoTo ex124_127 Else GoTo ex120_123
        
ex120_123:
        If (xxx And 2) Then GoTo ex122_123 Else GoTo ex120_121
        
ex120_121:
        If xxx = 120 Then
            ' 120 ' LD A,B
            regA = regB
            lTStates = 4
        Else
            ' 121 ' LD A,C
            regA = regC
            lTStates = 4
        End If
        GoTo execute_end
        
ex122_123:
        If xxx = 122 Then
            ' 122 ' LD A,D
            regA = Div256(regDE)
            lTStates = 4
        Else
            ' 123 ' LD A,E
            regA = getE
            lTStates = 4
        End If
        GoTo execute_end
        
ex124_127:
        If (xxx And 2) Then GoTo ex126_127 Else GoTo ex124_125
        
ex124_125:
        If xxx = 124 Then
            ' 124 ' LD A,H
            regA = Div256(regHL)
            lTStates = 4
        Else
            ' 125 ' LD A,L
            regA = (regHL And &HFF)
            lTStates = 4
        End If
        GoTo execute_end
        
ex126_127:
        If xxx = 126 Then
            ' 126 ' LD A,(HL)
            regA = peekb(regHL)
            lTStates = 7
        Else
            ' 127 ' LD A,A
            lTStates = 4
        End If
        GoTo execute_end
        
ex128_255:
        If (xxx And 64) Then GoTo ex192_255 Else GoTo ex128_191
        
ex128_191:
        If (xxx And 32) Then GoTo ex160_191 Else GoTo ex128_159
        
ex128_159:
        If (xxx And 16) Then GoTo ex144_159 Else GoTo ex128_143
        
ex128_143:
        Select Case xxx
        ' ADD A,*
        Case 128 ' ADD A,B
            add_a regB
            lTStates = 4
        Case 129 ' ADD A,C
            add_a regC
            lTStates = 4
        Case 130 ' ADD A,D
            add_a Div256(regDE)
            lTStates = 4
        Case 131 ' ADD A,E
            add_a getE
            lTStates = 4
        Case 132 ' ADD A,H
            add_a Div256(regHL)
            lTStates = 4
        Case 133 ' ADD A,L
            add_a (regHL And &HFF)
            lTStates = 4
        Case 134 ' ADD A,(HL)
            add_a peekb(regHL)
            lTStates = 7
        Case 135 ' ADD A,A
            add_a regA
            lTStates = 4
        Case 136 ' ADC A,B
            adc_a regB
            lTStates = 4
        Case 137 ' ADC A,C
            adc_a regC
            lTStates = 4
        Case 138 ' ADC A,D
            adc_a Div256(regDE)
            lTStates = 4
        Case 139 ' ADC A,E
            adc_a getE
            lTStates = 4
        Case 140 ' ADC A,H
            adc_a Div256(regHL)
            lTStates = 4
        Case 141 ' ADC A,L
            adc_a (regHL And &HFF)
            lTStates = 4
        Case 142 ' ADC A,(HL)
            adc_a peekb(regHL)
            lTStates = 7
        Case 143 ' ADC A,A
            adc_a regA
            lTStates = 4
        End Select
        GoTo execute_end
        
ex144_159:
        Select Case xxx
        Case 144 ' SUB B
            sub_a regB
            lTStates = 4
        Case 145 ' SUB C
            sub_a regC
            lTStates = 4
        Case 146 ' SUB D
            sub_a Div256(regDE)
            lTStates = 4
        Case 147 ' SUB E
            sub_a getE
            lTStates = 4
        Case 148 ' SUB H
            sub_a Div256(regHL)
            lTStates = 4
        Case 149 ' SUB L
            sub_a (regHL And &HFF)
            lTStates = 4
        Case 150 ' SUB (HL)
            sub_a peekb(regHL)
            lTStates = 7
        Case 151 ' SUB A
            sub_a regA
            lTStates = 4
        Case 152 ' SBC A,B
            sbc_a regB
            lTStates = 4
        Case 153 ' SBC A,C
            sbc_a regC
            lTStates = 4
        Case 154 ' SBC A,D
            sbc_a Div256(regDE)
            lTStates = 4
        Case 155 ' SBC A,E
            sbc_a getE
            lTStates = 4
        Case 156 ' SBC A,H
            sbc_a Div256(regHL)
            lTStates = 4
        Case 157 ' SBC A,L
            sbc_a (regHL And &HFF)
            lTStates = 4
        Case 158 ' SBC A,(HL)
            sbc_a peekb(regHL)
            lTStates = 7
        Case 159 ' SBC A,A
            sbc_a regA
            lTStates = 4
        End Select
        GoTo execute_end

ex160_191:
        If (xxx And 16) Then GoTo ex176_191 Else GoTo ex160_175
        
ex160_175:
        If (xxx And 8) Then GoTo ex168_175 Else GoTo ex160_167
        
ex160_167:
        If (xxx And 4) Then GoTo ex164_167 Else GoTo ex160_163
        
ex160_163:
        If (xxx And 2) Then GoTo ex162_163 Else GoTo ex160_161
        
ex164_167:
        If (xxx And 2) Then GoTo ex166_167 Else GoTo ex164_165
        
ex160_161:
        If xxx = 160 Then
            ' 160 ' AND B
            and_a regB
            lTStates = 4
        Else
            ' 161 ' AND C
            and_a regC
            lTStates = 4
        End If
        GoTo execute_end
        
ex162_163:
        If xxx = 162 Then
            ' 162 ' AND D
            and_a Div256(regDE)
            lTStates = 4
        Else
            ' 163 ' AND E
            and_a getE
            lTStates = 4
        End If
        GoTo execute_end
        
ex164_165:
        If xxx = 164 Then
            ' 164 ' AND H
            and_a Div256(regHL)
            lTStates = 4
        Else
            ' 165 ' AND L
            and_a (regHL And &HFF)
            lTStates = 4
        End If
        GoTo execute_end
        
ex166_167:
        If xxx = 166 Then
            ' 166 ' AND (HL)
            and_a peekb(regHL)
            lTStates = 7
        Else
            ' 167 ' AND A
            and_a regA
            lTStates = 4
        End If
        GoTo execute_end
        
ex168_175:
        If (xxx And 4) Then GoTo ex172_175 Else GoTo ex168_171
        
ex168_171:
        If (xxx And 2) Then GoTo ex170_171 Else GoTo ex168_169
        
ex168_169:
        If xxx = 168 Then
            ' 168 ' XOR B
            xor_a regB
            lTStates = 4
        Else
            ' 169 ' XOR C
            xor_a regC
            lTStates = 4
        End If
        GoTo execute_end
        
ex170_171:
        If xxx = 170 Then
            ' 170 ' XOR D
            xor_a Div256(regDE)
            lTStates = 4
        Else
            ' 171 ' XOR E
            xor_a getE
            lTStates = 4
        End If
        GoTo execute_end
        
ex172_175:
        If (xxx And 2) Then GoTo ex174_175 Else GoTo ex172_173
        
ex172_173:
        If xxx = 172 Then
            ' 172 ' XOR H
            xor_a Div256(regHL)
            lTStates = 4
        Else
            ' 173 ' XOR L
            xor_a (regHL And &HFF)
            lTStates = 4
        End If
        GoTo execute_end
        
ex174_175:
        If xxx = 174 Then
            ' 174 ' XOR (HL)
            xor_a peekb(regHL)
            lTStates = 7
        Else
            ' 175 ' XOR A
            regA = 0
            fS = False
            f3 = False
            f5 = False
            fH = False
            fPV = True
            fZ = True
            fN = False
            fC = False
            lTStates = 4
        End If
        GoTo execute_end

ex176_191:
        If (xxx And 8) Then GoTo ex184_191 Else GoTo ex176_183
        
ex176_183:
        If (xxx And 4) Then GoTo ex180_183 Else GoTo ex176_179
        
ex176_179:
        If (xxx And 2) Then GoTo ex178_179 Else GoTo ex176_177
        
ex176_177:
        If xxx = 176 Then
            ' 176 ' OR B
            or_a regB
            lTStates = 4
        Else
            ' 177 ' OR C
            or_a regC
            lTStates = 4
        End If
        GoTo execute_end
        
ex178_179:
        If xxx = 178 Then
            ' 178 ' OR D'
            or_a Div256(regDE)
            lTStates = 4
        Else
            ' 179 ' OR E
            or_a getE
            lTStates = 4
        End If
        GoTo execute_end
        
ex180_183:
        If (xxx And 2) Then GoTo ex182_183 Else GoTo ex180_181
        
ex180_181:
        If xxx = 180 Then
            ' 180 ' OR H
            or_a Div256(regHL)
            lTStates = 4
        Else
            ' 181 ' OR L
            or_a (regHL And &HFF)
            lTStates = 4
        End If
        GoTo execute_end
        
ex182_183:
        If xxx = 182 Then
            ' 182 ' OR (HL)
            or_a peekb(regHL)
            lTStates = 7
        Else
            ' 183 ' OR A
            or_a regA
            lTStates = 4
        End If
        GoTo execute_end
        
ex184_191:
        If (xxx And 4) Then GoTo ex188_191 Else GoTo ex184_187
        
ex184_187:
        If (xxx And 2) Then GoTo ex186_187 Else GoTo ex184_185
        
ex184_185:
        If xxx = 184 Then
            ' 184 ' CP B
            cp_a regB
            lTStates = 4
        Else
            ' 185 ' CP C
            cp_a regC
            lTStates = 4
        End If
        GoTo execute_end
        
ex186_187:
        If xxx = 186 Then
            ' 186 ' CP D
            cp_a Div256(regDE)
            lTStates = 4
        Else
            ' 187 ' CP E
            cp_a getE
            lTStates = 4
        End If
        GoTo execute_end
        
ex188_191:
        If (xxx And 2) Then GoTo ex190_191 Else GoTo ex188_189
        
ex188_189:
        If xxx = 188 Then
            ' 188 ' CP H
            cp_a Div256(regHL)
            lTStates = 4
        Else
            ' 189 ' CP L
            cp_a (regHL And &HFF)
            lTStates = 4
        End If
        GoTo execute_end
        
ex190_191:
        If xxx = 190 Then
            ' 190 ' CP (HL)
            cp_a peekb(regHL)
            lTStates = 7
        Else
            ' 191 ' CP A
            cp_a regA
            lTStates = 4 
        End If
        GoTo execute_end
        
ex192_255:
        If (xxx And 32) Then GoTo ex224_255 Else GoTo ex192_223
        
ex192_223:
        If (xxx And 16) Then GoTo ex208_223 Else GoTo ex192_207

ex192_207:
        If (xxx And 8) Then GoTo ex200_207 Else GoTo ex192_199
        
ex192_199:
        If (xxx And 4) Then GoTo ex196_199 Else GoTo ex192_195
        
ex192_195:
        If (xxx And 2) Then GoTo ex194_195 Else GoTo ex192_193
        
ex192_193:
        If xxx = 192 Then
            ' 192 ' RET NZ
            If fZ = False Then
                poppc
                lTStates = 11
            Else
                lTStates = 5
            End If
        Else
            ' 193 ' POP BC
            setBC popw
            lTStates = 10
        End If
        GoTo execute_end
        
ex194_195:
        If xxx = 194 Then
            ' 194 ' JP NZ,nn
            If fZ = False Then
                regPC = nxtpcw
            Else
                regPC = regPC + 2
            End If
            lTStates = 10
        Else
            ' 195 ' JP nn
            regPC = peekw(regPC)
            lTStates = 10
        End If
        GoTo execute_end
        
ex196_199:
        If (xxx And 2) Then GoTo ex198_199 Else GoTo ex196_197
        
ex196_197:
        If xxx = 196 Then
            ' 196 ' CALL NZ,nn
            If fZ = False Then
                lTemp = nxtpcw
                pushw regPC
                regPC = lTemp
                lTStates = 17
            Else
                regPC = regPC + 2
                lTStates = 10
            End If
        Else
            ' 197 ' PUSH BC
            pushw ((regB * 256) Or regC)
            lTStates = 11
        End If
        GoTo execute_end
        
        
ex198_199:
        If xxx = 198 Then
            ' 198 ' ADD A,n
            add_a nxtpcb()
            lTStates = 7
        Else
            ' 199 ' RST 0
            pushpc
            regPC = 0
            lTStates = 11
        End If
        GoTo execute_end
        
ex200_207:
        If (xxx And 4) Then GoTo ex204_207 Else GoTo ex200_203
        
ex200_203:
        If (xxx And 2) Then GoTo ex202_203 Else GoTo ex200_201
        
ex200_201:
        If xxx = 200 Then
            ' 200 ' RET Z
            If fZ Then
                poppc
                lTStates = 11
            Else
                lTStates = 5
            End If
        Else
            ' 201 ' RET
            poppc
            lTStates = 10
        End If
        GoTo execute_end
        
ex202_203:
        If xxx = 202 Then
            ' 202 ' JP Z,nn
            If fZ Then
                regPC = nxtpcw
            Else
                regPC = regPC + 2
            End If
            lTStates = 10
        Else
            ' 203 ' Prefix CB
            lTStates = execute_cb
        End If
        GoTo execute_end
        
ex204_207:
        If (xxx And 2) Then GoTo ex206_207 Else GoTo ex204_205
        
ex204_205:
        If xxx = 204 Then
            ' 204 ' CALL Z,nn
            If fZ Then
                lTemp = nxtpcw
                pushw regPC
                regPC = lTemp
                lTStates = 17
            Else
                regPC = regPC + 2
                lTStates = 10
            End If
        Else
            ' 205 ' CALL nn
            lTemp = nxtpcw
            pushw regPC
            regPC = lTemp
            lTStates = 17
        End If
        GoTo execute_end
        
ex206_207:
        If xxx = 206 Then
            ' 206 ' ADC A,n
            adc_a nxtpcb()
            lTStates = 7
        Else
            ' 207 ' RST 8
            pushpc
            regPC = 8
            lTStates = 11
        End If
        GoTo execute_end
        
ex208_223:
        If (xxx And 8) Then GoTo ex216_223 Else GoTo ex208_215
        
ex208_215:
        If (xxx And 4) Then GoTo ex212_215 Else GoTo ex208_211
        
ex208_211:
        If (xxx And 2) Then GoTo ex210_211 Else GoTo ex208_209
        
ex208_209:
        If xxx = 208 Then
            ' 208 ' RET NC
            If fC = False Then
                poppc
                lTStates = 11
            Else
                lTStates = 5
            End If
        Else
            ' 209 ' POP DE
            regDE = popw
            lTStates = 10
        End If
        GoTo execute_end
        
ex210_211:
        If xxx = 210 Then
            ' 210 '  JP NC,nn
            If fC = False Then
                regPC = nxtpcw
            Else
                regPC = regPC + 2
            End If
            lTStates = 10
        Else
            ' 211 ' OUT (n),A
            outb (256 * regA + nxtpcb), regA
            lTStates = 11
        End If
        GoTo execute_end
        
ex212_215:
        If (xxx And 2) Then GoTo ex214_215 Else GoTo ex212_213
        
ex212_213:
        If xxx = 212 Then
            ' 212 ' CALL NC,nn
            If fC = False Then
                lTemp = nxtpcw
                pushw regPC
                regPC = lTemp
                lTStates = 17
            Else
                regPC = regPC + 2
                lTStates = 10
            End If
        Else
            ' 213 ' PUSH DE
            pushw regDE
            lTStates = 11
        End If
        GoTo execute_end
        
ex214_215:
        If xxx = 214 Then
            ' 214 ' SUB n
            sub_a nxtpcb()
            lTStates = 7
        Else
            ' 215 ' RST 16
            pushpc
            regPC = 16
            
            lTStates = 11
        End If
        GoTo execute_end
        
ex216_223:
        Select Case xxx
        Case 216 ' RET C
            If fC Then
                poppc
                lTStates = 11
            Else
                lTStates = 5
            End If
        Case 217 ' EXX
            exx
            lTStates = 4
        Case 218 ' JP C,nn
            If fC Then
                regPC = nxtpcw
            Else
                regPC = regPC + 2
            End If
            lTStates = 10
        Case 219 ' IN A,(n)
            regA = inb((regA * 256) Or nxtpcb)
            lTStates = 11
        Case 220 ' CALL C,nn
            If fC Then
                lTemp = nxtpcw
                pushw regPC
                regPC = lTemp
                lTStates = 17
            Else
                regPC = regPC + 2
                lTStates = 10
            End If
        Case 221 ' prefix IX
            regID = regIX
            lTStates = execute_id()
            regIX = regID
        Case 222 ' SBC n
            sbc_a nxtpcb()
            lTStates = 7
        Case 223 ' RST 24
            pushpc
            regPC = 24
            lTStates = 11
        End Select
        GoTo execute_end
        
ex224_255:
        If (xxx And 16) Then GoTo ex240_255 Else GoTo ex224_239
        
ex224_239:
        If (xxx And 8) Then GoTo ex232_239 Else GoTo ex224_231
        
ex224_231:
        If (xxx And 4) Then GoTo ex228_231 Else GoTo ex224_227
        
ex224_227:
        If (xxx And 2) Then GoTo ex226_227 Else GoTo ex224_225
        
ex224_225:
        If xxx = 224 Then
            ' 224 ' RET PO
            If fPV = False Then
                poppc
                lTStates = 11
            Else
                lTStates = 5
            End If
        Else
            ' 225 ' POP HL
            regHL = popw
            lTStates = 10
        End If
        GoTo execute_end
        
ex226_227:
        If xxx = 226 Then
            ' 226 ' JP PO,nn
            If fPV = False Then
                regPC = nxtpcw
            Else
                regPC = regPC + 2
            End If
            lTStates = 10
        Else
            ' 227 ' EX (SP),HL
            lTemp = regHL
            regHL = peekw(regSP)
            pokew regSP, lTemp
            lTStates = 19
        End If
        GoTo execute_end
        
ex228_231:
        If (xxx And 2) Then GoTo ex230_231 Else GoTo ex228_229
        
ex228_229:
        If xxx = 228 Then
            ' 228 ' CALL PO,nn
            If fPV = False Then
                lTemp = nxtpcw
                pushw regPC
                regPC = lTemp
                lTStates = 17
            Else
                regPC = regPC + 2
                lTStates = 10
            End If
        Else
            ' 229 ' PUSH HL
            pushw regHL
            lTStates = 11
        End If
        GoTo execute_end
        
ex230_231:
        If xxx = 230 Then
            ' 230 ' AND n
            and_a nxtpcb()
            lTStates = 7
        Else
            ' 231 ' RST 32
            pushpc
            regPC = 32
            lTStates = 11
        End If
        GoTo execute_end
        
ex232_239:
        If (xxx And 4) Then GoTo ex236_239 Else GoTo ex232_235
        
ex232_235:
        If (xxx And 2) Then GoTo ex234_235 Else GoTo ex232_233
        
ex232_233:
        If xxx = 232 Then
            ' RET PE
            If fPV Then
                poppc
                lTStates = 11
            Else
                lTStates = 5
            End If
        Else
            ' 233 ' JP HL
            regPC = regHL
            lTStates = 4
        End If
        GoTo execute_end
        
ex234_235:
        If xxx = 234 Then
            ' 234 ' JP PE,nn
            If fPV Then
                regPC = nxtpcw
            Else
                regPC = regPC + 2
            End If
            lTStates = 10
        Else
            ' 235 ' EX DE,HL
            lTemp = regHL
            regHL = regDE
            regDE = lTemp
            lTStates = 4
        End If
        GoTo execute_end
        
ex236_239:
        If (xxx And 2) Then GoTo ex238_239 Else GoTo ex236_237
        
ex236_237:
        If xxx = 236 Then
            ' 236 ' CALL PE,nn
            If fPV Then
                lTemp = nxtpcw
                pushw regPC
                regPC = lTemp
                lTStates = 17
            Else
                regPC = regPC + 2
                lTStates = 10
            End If
        Else
            ' 237 ' prefix ED
            lTStates = execute_ed()
        End If
        GoTo execute_end
        
ex238_239:
        If xxx = 238 Then
            ' 238 ' XOR n
            xor_a nxtpcb()
            lTStates = 7
        Else
            ' 239 ' RST 40
            pushpc
            regPC = 40
            lTStates = 11
        End If
        GoTo execute_end
        
ex240_255:
        If (xxx And 8) Then GoTo ex248_255 Else GoTo ex240_247
        
ex240_247:
        If (xxx And 4) Then GoTo ex244_247 Else GoTo ex240_243
        
ex240_243:
        If (xxx And 2) Then GoTo ex242_243 Else GoTo ex240_241
        
ex240_241:
        If xxx = 240 Then
            ' 240 RET P
            If fS = False Then
                poppc
                lTStates = 11
            Else
                lTStates = 5
            End If
        Else
            ' 241 POP AF
            setAF popw
            lTStates = 10
        End If
        GoTo execute_end
        
ex242_243:
        If xxx = 242 Then
            ' 242 JP P,nn
            If fS = False Then
                regPC = nxtpcw
            Else
                regPC = regPC + 2
            End If
            lTStates = 10
        Else
            ' 243 DI
            intIFF1 = False
            intIFF2 = False
            lTStates = 4
        End If
        GoTo execute_end
        
ex244_247:
        If (xxx And 2) Then GoTo ex246_247 Else GoTo ex244_245
        
ex244_245:
        If xxx = 244 Then
        ' 244 CALL P,nn
            If fS = False Then
                lTemp = nxtpcw
                pushw regPC
                regPC = lTemp
                lTStates = 17
            Else
                regPC = regPC + 2
                lTStates = 10
            End If
        Else
            ' 245 PUSH AF
            pushw ((regA * 256) Or getF)
            lTStates = 11
        End If
        GoTo execute_end
        
ex246_247:
        If xxx = 246 Then
            ' 246 OR n
            or_a nxtpcb()
            lTStates = 7
        Else
            ' 247 RST 48
            pushpc
            regPC = 48
            lTStates = 11
        End If
        GoTo execute_end

ex248_255:
        If (xxx And 4) Then GoTo ex252_255 Else GoTo ex248_251
        
ex248_251:
        If (xxx And 2) Then GoTo ex250_251 Else GoTo ex248_249
        
ex248_249:
        If xxx = 248 Then
            ' 248 RET M
            If fS Then
                poppc
                lTStates = 11
            Else
                lTStates = 5
            End If
        Else
            ' 249 LD SP,HL
            regSP = regHL
            lTStates = 6
        End If
        GoTo execute_end

ex250_251:
        If xxx = 250 Then
            ' 250 JP M,nn
            If fS Then
                regPC = nxtpcw
            Else
                regPC = regPC + 2
            End If
            lTStates = 10
        Else
            ' 251 EI
            intIFF1 = True
            intIFF2 = True
            lTStates = 4
        End If
        GoTo execute_end
        
ex252_255:
        If (xxx And 2) Then GoTo ex254_255 Else GoTo ex252_253
        
ex252_253:
        If xxx = 252 Then
            ' 252 CALL M,nn
            If fS Then
                lTemp = nxtpcw
                pushw regPC
                regPC = lTemp
                lTStates = 17
            Else
                regPC = regPC + 2
                lTStates = 10
            End If
        Else
            ' 253 prefix IY
            regID = regIY
            lTStates = execute_id()
            regIY = regID
        End If
        GoTo execute_end
        
ex254_255:
        If xxx = 254 Then
            ' 254 CP n
            cp_a nxtpcb()
            lTStates = 7
        Else
            ' 255 RST 56
            pushpc
            regPC = 56
            lTStates = 11
        End If
        GoTo execute_end
        
execute_end:

    ' descontamos los ciclos gastados al total
    WastedStates+=lTStates
    
    Wend
End Sub

Function qdec8(a As integer) As Integer
    qdec8 = (a - 1) And &HFF
End Function

Function execute_id() As Integer
    Dim xxx As integer, lTemp As integer, op As Integer
      
    
    Refresco_INTR(1)
    
    xxx = peekb(regPC)
    regPC = regPC + 1
    
    If (xxx And 128) Then GoTo ex_id128_255 Else GoTo ex_id0_127
    
ex_id0_127:
    If (xxx And 64) Then GoTo ex_id64_127 Else GoTo ex_id0_63
    
ex_id0_63:
    If (xxx And 32) Then GoTo ex_id32_63 Else GoTo ex_id0_31
    
ex_id0_31:
    If (xxx And 16) Then GoTo ex_id16_31 Else GoTo ex_id0_15
    
ex_id0_15:
    Select Case xxx
    Case 0 To 8
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 9 ' ADD ID,BC
        regID = add16(regID, (regB * 256) Or regC)
        execute_id = 15
    Case 10 To 15
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    End Select
    Exit Function
    
ex_id16_31:
    If (xxx And 8) Then
        GoTo ex_id24_31
    Else
        ' 16 To 23
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    End If
    Exit Function
    
ex_id24_31:
    If xxx = 24 Then
        ' 24
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    ElseIf xxx = 25 Then
        ' 25 ' ADD ID,DE
        regID = add16(regID, regDE)
        execute_id = 15
    Else
        ' 26 To 31
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    End If
    Exit Function

ex_id32_63:
    Select Case xxx
    Case 32
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 33 ' LD ID,nn
        regID = nxtpcw
        execute_id = 14
    Case 34 ' LD (nn),ID
        pokew nxtpcw, regID
        execute_id = 20
    Case 35 ' INC ID
        regID = (regID + 1) And &HFFFF
        execute_id = 10
    Case 36 ' INC IDH
        setIDH inc8(getIDH)
        execute_id = 8
    Case 37 ' DEC IDH
        setIDH dec8(getIDH)
        execute_id = 8
    Case 38 ' LD IDH,n
        setIDH nxtpcb()
        execute_id = 11
    Case 39, 40
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 41 ' ADD ID,ID
        lTemp = regID
        regID = add16(lTemp, lTemp)
        execute_id = 15
    Case 42 ' LD ID,(nn)
        regID = peekw(nxtpcw)
        execute_id = 20
    Case 43 ' DEC ID
        regID = dec16(regID)
        execute_id = 10
    Case 44 ' INC IDL
        setIDL inc8(getIDL)
        execute_id = 8
    Case 45 ' DEC IDL
        setIDL dec8(getIDL)
        execute_id = 8
    Case 46 ' LD IDL,n
        setIDL nxtpcb()
        execute_id = 11
    Case 47 To 51
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 52 ' INC (ID+d)
        lTemp = id_d
        pokeb lTemp, inc8(peekb(lTemp))
        execute_id = 23
    Case 53 ' DEC (ID+d)
        lTemp = id_d
        pokeb lTemp, dec8(peekb(lTemp))
        execute_id = 23
    Case 54 ' LD (ID+d),n
        lTemp = id_d
        pokeb lTemp, nxtpcb()
        execute_id = 19
    Case 55, 56
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 57 ' ADD ID,SP
        regID = add16(regID, regSP)
        execute_id = 15
    Case 58 To 63
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    End Select
    Exit Function
    
ex_id64_127:
    If (xxx And 32) Then GoTo ex_id96_127 Else GoTo ex_id64_95
    
ex_id64_95:
    If (xxx And 16) Then GoTo ex_id80_95 Else GoTo ex_id64_79
    
ex_id64_79:
    Select Case xxx
    Case 64 To 67
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 68 ' LD B,IDH
        regB = getIDH
        execute_id = 8
    Case 69 ' LD B,IDL
        regB = getIDL
        execute_id = 8
    Case 70 ' LD B,(ID+d)
        regB = peekb(id_d)
        execute_id = 19
    Case 71 To 75
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 76 ' LD C,IDH
        regC = getIDH
        execute_id = 8
    Case 77 ' LD C,IDL
        regC = getIDL
        execute_id = 8
    Case 78 ' LD C,(ID+d)
        regC = peekb(id_d)
        execute_id = 19
    Case 79
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    End Select
    Exit Function
    
ex_id80_95:
    If (xxx And 8) Then GoTo ex_id88_95 Else GoTo ex_id80_87
    
ex_id80_87:
    Select Case xxx
    Case 80 To 83
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 84 ' LD D,IDH
        setD getIDH
        execute_id = 8
    Case 85 ' LD D,IDL
        setD getIDL
        execute_id = 8
    Case 86 ' LD D,(ID+d)
        setD peekb(id_d)
        execute_id = 19
    Case 87
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    End Select
    Exit Function
    
ex_id88_95:
    If (xxx And 4) Then GoTo ex_id92_95 Else GoTo ex_id88_91
    
ex_id88_91:
    ' 88 To 91
    regPC = dec16(regPC)
    
    Refresco_INTR(-1)
    execute_id = 4
    Exit Function
    
ex_id92_95:
    If (xxx And 2) Then GoTo ex_id94_95 Else GoTo ex_id92_93
    
ex_id92_93:
    If xxx = 92 Then
        ' 92 ' LD E,IDH
        setE getIDH
        execute_id = 8
    Else
        ' 93 ' LD E,IDL
        setE getIDL
        execute_id = 8
    End If
    Exit Function
    
ex_id94_95:
    If xxx = 94 Then
        ' 94 ' LD E,(ID+d)
        setE peekb(id_d)
        execute_id = 19
    Else
        ' 95
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    End If
    Exit Function
    
ex_id96_127:
    If (xxx And 16) Then GoTo ex_id112_127 Else GoTo ex_id96_111
    
ex_id96_111:
    If (xxx And 8) Then GoTo ex_id104_111 Else GoTo ex_id96_103
    
ex_id96_103:
    If (xxx And 4) Then GoTo ex_id100_103 Else GoTo ex_id96_99
    
ex_id96_99:
    If (xxx And 2) Then GoTo ex_id98_99 Else GoTo ex_id96_97
    
ex_id96_97:
    If xxx = 96 Then
        ' 96 ' LD IDH,B
        setIDH regB
        execute_id = 8
    Else
        ' 97 ' LD IDH,C
        setIDH regC
        execute_id = 8
    End If
    Exit Function
    
ex_id98_99:
    If xxx = 98 Then
        ' 98 ' LD IDH,D
        setIDH Div256(regDE)
        execute_id = 8
    Else
        ' 99 ' LD IDH,E
        setIDH getE
        execute_id = 8
    End If
    Exit Function

ex_id100_103:
    If (xxx And 2) Then GoTo ex_id102_103 Else GoTo ex_id100_101
    
ex_id100_101:
    If xxx = 100 Then
        ' 100 ' LD IDH,IDH
        execute_id = 8
    Else
        ' 101 ' LD IDH,IDL
        setIDH getIDL
        execute_id = 8
    End If
    Exit Function
    
ex_id102_103:
    If xxx = 102 Then
        ' 102 ' LD H,(ID+d)
        setH peekb(id_d)
        execute_id = 19
    Else
        ' 103 ' LD IDH,A
        setIDH regA
        execute_id = 8
    End If
    Exit Function
    
ex_id104_111:
    If (xxx And 4) Then GoTo ex_id108_111 Else GoTo ex_id104_107
    
ex_id104_107:
    Select Case xxx
    Case 104 ' LD IDL,B
        setIDL regB
        execute_id = 9
    Case 105 ' LD IDL,C
        setIDL regC
        execute_id = 9
    Case 106 ' LD IDL,D
        setIDL Div256(regDE)
        execute_id = 9
    Case 107 ' LD IDL,E
        setIDL getE
        execute_id = 9
    End Select
    Exit Function
    
ex_id108_111:
    If (xxx And 2) Then GoTo ex_id110_111 Else GoTo ex_id108_109
    
ex_id108_109:
    If xxx = 108 Then
        ' 108 ' LD IDL,IDH
        setIDL getIDH
        execute_id = 8
    Else
        ' 109 ' LD IDL,IDL
        execute_id = 8
    End If
    Exit Function
    
ex_id110_111:
    If xxx = 110 Then
        ' 110 ' LD L,(ID+d)
        setL peekb(id_d)
        execute_id = 19
    Else
        ' 111 ' LD IDL,A
        setIDL regA
        execute_id = 8
    End If
    Exit Function
    
ex_id112_127:
    If (xxx And 8) Then GoTo ex_id120_127 Else GoTo ex_id112_119
    
ex_id112_119:
    Select Case xxx
    Case 112 ' LD (ID+d),B
        pokeb id_d, regB
        execute_id = 19
    Case 113 ' LD (ID+d),C
        pokeb id_d, regC
        execute_id = 19
    Case 114 ' LD (ID+d),D
        pokeb id_d, Div256(regDE)
        execute_id = 19
    Case 115 ' LD (ID+d),E
        pokeb id_d, getE
        execute_id = 19
    Case 116 ' LD (ID+d),H
        pokeb id_d, Div256(regHL)
        execute_id = 19
    Case 117 ' LD (ID+d),L
        pokeb id_d, (regHL And &HFF)
        execute_id = 19
    Case 118 ' desconocida
        msg "Instruccion desconocida "+Str(xxx)+" en "+Str(regPC),1
    Case 119 ' LD (ID+d),A
        pokeb id_d, regA
        execute_id = 19
    End Select
    Exit Function
    
ex_id120_127:
    Select Case xxx
    Case 120 To 123
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 124 ' LD A,IDH
        regA = getIDH
        execute_id = 8
    Case 125 ' LD A,IDL
        regA = getIDL
        execute_id = 8
    Case 126 ' LD A,(ID+d)
        regA = peekb(Id_d)
        execute_id = 19
    Case 127
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    End Select
    Exit Function

ex_id128_255:
    If (xxx And 64) Then GoTo ex_id192_255 Else GoTo ex_id128_191
    
ex_id128_191:
    If (xxx And 32) Then GoTo ex_id160_191 Else GoTo ex_id128_159
    
ex_id128_159:
    Select Case xxx
    Case 128 To 131
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 132 ' ADD A,IDH
        add_a getIDH
        execute_id = 8
    Case 133 ' ADD A,IDL
        add_a getIDL
        execute_id = 8
    Case 134 ' ADD A,(ID+d)
        add_a peekb(id_d)
        execute_id = 19
    Case 135 To 139
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 140 ' ADC A,IDH
        adc_a getIDH
        execute_id = 8
    Case 141 ' ADC A,IDL
        adc_a getIDL
        execute_id = 8
    Case 142 ' ADC A,(ID+d)
        adc_a peekb(id_d)
        execute_id = 19
    Case 143 To 147
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 148 ' SUB IDH
        sub_a getIDH
        execute_id = 8
    Case 149 ' SUB IDL
        sub_a getIDL
        execute_id = 8
    Case 150 ' SUB (ID+d)
        sub_a peekb(id_d)
        execute_id = 19
    Case 151 To 155
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 156 ' SBC A,IDH
        sbc_a getIDH
        execute_id = 8
    Case 157 ' SBC A,IDL
        sbc_a getIDL
        execute_id = 8
    Case 158 ' SBC A,(ID+d)
        sbc_a peekb(id_d)
        execute_id = 19
    Case 159
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    End Select
    Exit Function
    
ex_id160_191:
    Select Case xxx
    Case 160 To 163
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 164 ' AND IDH
        and_a getIDH
        execute_id = 8
    Case 165 ' AND IDL
        and_a getIDL
        execute_id = 8
    Case 166 ' AND (ID+d)
        and_a peekb(id_d)
        execute_id = 19
    Case 167 To 171
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 172 ' XOR IDH
        xor_a getIDH
        execute_id = 8
    Case 173 ' XOR IDL
        xor_a getIDL
        execute_id = 8
    Case 174 'XOR (ID+d)
        xor_a (peekb(id_d))
        execute_id = 19
    Case 175 To 179
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 180 ' OR IDH
        or_a getIDH
        execute_id = 8
    Case 181 ' OR IDL
        or_a getIDL
        execute_id = 8
    Case 182 ' OR (ID+d)
        or_a peekb(id_d)
        execute_id = 19
    Case 183 To 187
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 188 ' CP IDH
        cp_a getIDH
        execute_id = 8
    Case 189 ' CP IDL
        cp_a getIDL
        execute_id = 8
    Case 190 ' CP (ID+d)
        cp_a peekb(id_d)
        execute_id = 19
    Case 191
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    End Select
    Exit Function
    
ex_id192_255:
    Select Case xxx
    Case 192 To 202
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 203 ' prefix CB
        lTemp = id_d
        op = nxtpcb()
        execute_id_cb op, lTemp
        If ((op And &HC0) = &H40) Then execute_id = 20 Else execute_id = 23
    Case 204 To 224
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 225 ' POP ID
        regID = popw()
        execute_id = 14
    Case 226
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 227 ' EX (SP),ID
        lTemp = regID
        regID = peekw(regSP)
        pokew regSP, lTemp
        execute_id = 23
    Case 228
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 229 ' PUSH ID
        pushw regID
        execute_id = 15
    Case 230 To 232
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 233 ' JP ID
        regPC = regID
        execute_id = 8
    Case 234 To 248
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case 249 ' LD SP,ID
        regSP = regID
        execute_id = 10
    Case 253 ' Prefix FD
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    Case Else
        regPC = dec16(regPC)
        
        Refresco_INTR(-1)
        execute_id = 4
    End Select
End Function


Function inc16(a As integer) As Integer
    inc16 = (a + 1) And &HFFFF
End Function

Function nxtpcw() As Integer
    nxtpcw = peekw(regPC)
    regPC = regPC + 2
End Function

Function nxtpcb() As Integer
    nxtpcb = peekb(regPC)
    regPC = regPC + 1
End Function

Sub setD(l As Integer)
    regDE = (l * 256) Or (regDE And &HFF)
End Sub

Sub setE(l As Integer)
    regDE = (regDE And &HFF00) Or l
End Sub

Sub setF(b As Integer)
    fS = (b And F_S) <> 0
    fZ = (b And F_Z) <> 0
    f5 = (b And F_5) <> 0
    fH = (b And F_H) <> 0
    f3 = (b And F_3) <> 0
    fPV = (b And F_PV) <> 0
    fN = (b And F_N) <> 0
    fC = (b And F_C) <> 0
End Sub

Sub setH(l As Integer)
    regHL = (l * 256) Or (regHL And &HFF)
End Sub


Sub setL(l As Integer)
    regHL = (regHL And &HFF00) Or l
End Sub



Function sla(ByVal ans As integer) As Integer
    fC = (ans And &H80) <> 0
    ans = (ans * 2) And &HFF

    fS = (ans And F_S) <> 0
    f3 = (ans And F_3) <> 0
    f5 = (ans And F_5) <> 0
    fZ = (ans = 0)
    fPV = Parity(ans)
    fH = False
    fN = False
    
    sla = ans
End Function

Function sls(ByVal ans As integer) As Integer
    fC = (ans And &H80) <> 0
    ans = ((ans * 2) Or &H1) And &HFF
  
    fS = (ans And F_S) <> 0
    f3 = (ans And F_3) <> 0
    f5 = (ans And F_5) <> 0
    fZ = (ans = 0)
    fPV = Parity(ans)
    fH = False
    fN = False
    
    sls = ans
End Function

Function sra(ByVal ans As integer) As Integer
    fC = (ans And &H1) <> 0
    ans = (ans \ 2) Or (ans And &H80)
   
    fS = (ans And F_S) <> 0
    f3 = (ans And F_3) <> 0
    f5 = (ans And F_5) <> 0
    fZ = (ans = 0)
    fPV = Parity(ans)
    fH = False
    fN = False
    
    sra = ans
End Function

Function srl(ByVal ans As integer) As Integer
    fC = (ans And &H1) <> 0
    ans = ans \ 2

    fS = (ans And F_S) <> 0
    f3 = (ans And F_3) <> 0
    f5 = (ans And F_5) <> 0
    fZ = (ans = 0)
    fPV = Parity(ans)
    fH = False
    fN = False
    
    srl = ans
End Function

Sub sub_a(b As Integer)
    Dim wans As integer, ans As Integer
    
    wans = regA - b
    ans = wans And &HFF
    
    fS = (ans And F_S) <> 0
    f3 = (ans And F_3) <> 0
    f5 = (ans And F_5) <> 0
    fZ = (ans = 0)
    fC = (wans And &H100) <> 0
    fPV = ((regA Xor b) And (regA Xor ans) And &H80) <> 0
    fH = (((regA And &HF) - (b And &HF)) And F_H) <> 0
    fN = True
    
    regA = ans
End Sub


Sub xor_a(b As Integer)
    regA = (regA Xor b) And &HFF
    
    fS = (regA And F_S) <> 0
    f3 = (regA And F_3) <> 0
    f5 = (regA And F_5) <> 0
    fH = False
    fPV = Parity(regA)
    fZ = (regA = 0)
    fN = False
    fC = False
End Sub
